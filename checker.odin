// Assign types to AST nodes, resolve identifiers.
package vecc

import "core:fmt"
import "core:strings"
import "core:strconv"
import "core:slice"
import "core:os"
import "core:reflect"

Checker :: struct {
    filename:           string,
    curr_scope:         ^Scope,
    curr_lanes:         int,
    curr_entity:        ^Entity,
    curr_file_scope:    ^Scope,
    basic_types:        [Type_Basic_Kind]^Type,
    types:              [dynamic]^Type,
}

Entity :: struct {
    ast:            ^Ast,
    name:           string,
    order_index:    int,
    variant:        Entity_Variant,
    // TODO
    depends:        map[string]struct{},
}

Entity_Variant :: union {
    Entity_Proc,
    Entity_Variable,
    Entity_Struct,
    Entity_Struct_Field,
    Entity_Builtin,
}

Entity_Proc :: struct {

}

Entity_Variable :: struct {

}

Entity_Builtin :: struct {
    kind:   Entity_Builtin_Kind,
    type:   ^Type,
    value:  Value,
}

Entity_Builtin_Kind :: enum u8 {
    Vector_Index,
}

Entity_Struct :: struct {
    type:   ^Type,
}

Entity_Struct_Field :: struct {
}

Entity_Alias :: struct {
    entity: ^Entity,
}

Scope :: struct {
    ast:            ^Ast,
    parent:         ^Scope,
    children:       [dynamic]^Scope,
    depth:          int,
    entities:       map[string]^Entity,
}

checker_error :: proc(c: ^Checker, format: string, args: ..any) -> ! {
    // fmt.eprintf("[{}] %s(%d:%d) ", loc, p.filename, pos.line, pos.column)
	fmt.eprintf(format, ..args)
	fmt.eprintln()
	os.exit(1)
}

check_ident :: proc(c: ^Checker, ast: ^Ast) -> ^Entity {
    ident := ast.variant.(Ast_Ident)
    name := ident.token.text

    if ent, scope, ok := find_entity(c.curr_scope, name); ok {
        #partial switch v in ent.variant {
        case Entity_Builtin:
            ast.type = v.type
            ast.value = v.value

        case:
            ast.type = ent.ast.type
            ast.value = ent.ast.value
            if scope.parent != nil && ast.order_index < ent.ast.order_index {
                checker_error(c, "Entity is used before declaration: {}", name)
            }
        }

        return ent
    }

    checker_error(c, "Ident not found: {}", name)
}

check_type :: proc(c: ^Checker, ast: ^Ast) {
    if ast.type != nil {
        return
    }

    #partial switch v in ast.variant {
    case Ast_Array_Type:
        check_expr(c, v.len)
        check_type(c, v.type)
        #partial switch l in v.len.value {
        case i128:
            assert(l > 0)
        case:
            assert(false)
        }

    case Ast_Struct_Type:
        for field in v.fields {
            field := field.variant.(Ast_Field)
            check_type(c, field.type)
        }

    case Ast_Pointer_Type:
        check_type(c, v.type)

    case Ast_Multi_Pointer_Type:
        check_type(c, v.type)
    }

    ast.type = find_or_create_type_ast(c, ast)
}

check_basic_literal :: proc(c: ^Checker, ast: ^Ast, type_hint: ^Type = nil) {
    assert(ast.type == nil)

    lit := ast.variant.(Ast_Basic_Literal)
    ast.type = find_or_create_type_ast(c, ast, type_hint = type_hint)

    #partial switch v in ast.type.variant {
    case Type_Basic:
        #partial switch v.kind {
        case .B8,
             .B16,
             .B32,
             .B64:
            #partial switch lit.token.kind {
            case .True:
                ast.value = true
            case .False:
                ast.value = false
            case:
                checker_error(c, "Invalid boolean literal")
            }

        case .I8,
             .I16,
             .I32,
             .I64,
             .U8,
             .U16,
             .U32,
             .U64:
            val, ok := strconv.parse_i128_maybe_prefixed(lit.token.text)
            assert(ok)
            ast.value = i128(val)

        case .F32:
            val, ok := strconv.parse_f32(lit.token.text)
            assert(ok)
            ast.value = f64(val)

        case .F64:
            val, ok := strconv.parse_f64(lit.token.text)
            assert(ok)
            ast.value = f64(val)
        }

    case:
        assert(false)
    }
}

check_call_expr :: proc(c: ^Checker, ast: ^Ast) {
    assert(ast.type == nil)

    call_expr := ast.variant.(Ast_Call_Expr)

    ent := check_ident(c, call_expr.procedure)

    if _, ok := ent.variant.(Entity_Proc); !ok {
        checker_error(c, "Invalid call entity")
    }

    proc_decl := ent.ast.variant.(Ast_Proc_Decl)
    proc_type := proc_decl.type.variant.(Ast_Proc_Type)

    for arg, i in call_expr.args {
        check_expr(c, arg)
        if arg.type != proc_type.params[i].type {
            checker_error(c, "Invalid argument type")
        }
    }

    if proc_type.result != nil {
        ast.type = proc_type.result.type
    }
}

check_cast_expr :: proc(c: ^Checker, ast: ^Ast) {
    assert(ast.type == nil)

    expr := ast.variant.(Ast_Cast_Expr)

    check_type(c, expr.type)
    check_expr(c, expr.value)

    ast.type = expr.type.type
}

check_urnary_expr :: proc(c: ^Checker, ast: ^Ast) {
    assert(ast.type == nil)

    expr := ast.variant.(Ast_Unary_Expr)
    #partial switch expr.op.kind {
    case .Sub:
        check_expr(c, expr.expr)
        ast.type = expr.expr.type

        if !type_is_numeric(expr.expr.type) {
            checker_error(c, "Unary 'Not' operator can be only applied to numeric values")
        }

    case .Not:
        check_expr(c, expr.expr)
        ast.type = expr.expr.type

        if !type_is_boolean(expr.expr.type) && !type_is_integer(expr.expr.type) {
            checker_error(c, "Unary 'Not' operator can be only applied to boolean and integer values")
        }

    case:
        checker_error(c, "Invalid unary operator: ", expr.op)
    }
}

check_binary_op :: proc(c: ^Checker, left: ^Ast, right: ^Ast, op: Token_Kind) -> ^Type {
    check_expr(c, left)
    check_expr(c, right, type_hint = left.type)

    left_elem := type_elem_basic_type(left.type)

    if left_elem != right.type {
        checker_error(c, "Types in binary expression don't match: {} vs {}",
            type_to_string(left_elem), type_to_string(right.type))
    }

    type := left.type

    #partial switch op {
    case .Equal,
         .Less_Than,
         .Less_Than_Equal,
         .Greater_Than,
         .Greater_Than_Equal,
         .Not_Equal:
        type = c.basic_types[.B32]
        if !type_is_numeric(left_elem) {
            checker_error(c, "{} operator can be only applied to numeric values", op)
        }

    case .Add,
         .Sub,
         .Mul,
         .Div:
        if !type_is_numeric(left_elem) {
            checker_error(c, "{} operator can be only applied to numeric values", op)
        }

    case .Bit_And,
         .Bit_Or,
         .Bit_Xor,
         .Bit_Shift_Left,
         .Bit_Shift_Right,
         .Mod:
    }

    return type
}

// check_type_internal, check_type, add_type_info_internal, check_ident
// https://github.com/odin-lang/Odin/blob/master/src/check_type.cpp#L827

check_expr :: proc(c: ^Checker, ast: ^Ast, type_hint: ^Type = nil) {
    if ast == nil do return

    assert(ast.type == nil)

    #partial switch v in ast.variant {
    case Ast_Basic_Literal:
        check_basic_literal(c, ast, type_hint = type_hint)

    case Ast_Ident:
        check_ident(c, ast)

    case Ast_Call_Expr:
        check_call_expr(c, ast)

    case Ast_Unary_Expr:
        check_urnary_expr(c, ast)

    case Ast_Cast_Expr:
        check_cast_expr(c, ast)

    case Ast_Binary_Expr:
        check_binary_expr(c, ast)

    case Ast_Selector_Expr:
        check_selector_expr(c, ast)

    case Ast_Index_Expr:
        check_index_expr(c, ast)

    case Ast_Address_Expr:
        check_address_expr(c, ast)

    case Ast_Deref_Expr:
        check_deref_expr(c, ast)

    case:
        assert(false)
    }
}

check_selector_expr :: proc(c: ^Checker, ast: ^Ast) {
    assert(ast.type == nil)

    expr := ast.variant.(Ast_Selector_Expr)
    check_expr(c, expr.left)

    {
        ident := expr.right.variant.(Ast_Ident)
        name := ident.token.text

        base_type := expr.left.type

        loop: for {
            #partial switch v in base_type.variant {
            case Type_Pointer:
                base_type = v.type

            case:
                break loop
            }
        }

        #partial switch v in base_type.variant {
        case Type_Struct:
            for field in v.fields {
                if ident.token.text == field.name {
                    expr.right.type = field.type
                    break
                }
            }

        case Type_Array:
            // Swizzling later?
            if v.len > 4 {
                break
            }

            all_fields := [?][2]u8 {
                0 = {'x', 'r'},
                1 = {'y', 'g'},
                2 = {'z', 'b'},
                3 = {'w', 'a'},
            }
            fields := all_fields[:v.len]

            if len(name) != 1 {
                break
            }

            field_loop: for field in fields {
                for ch in field {
                    if ch == name[0] {
                        expr.right.type = v.type
                        break field_loop
                    }
                }
            }
        }

        if expr.right.type == nil {
            checker_error(c, "Invalid field")
        }
    }

    ast.type = expr.right.type
}

check_index_expr :: proc(c: ^Checker, ast: ^Ast) {
    assert(ast.type == nil)

    expr := ast.variant.(Ast_Index_Expr)
    check_expr(c, expr.left)
    check_expr(c, expr.index)

    if !type_is_integer(expr.index.type) {
        checker_error(c, "Index must be of integer type, got", type_to_string(expr.index.type))
    }

    #partial switch v in expr.left.type.variant {
    case Type_Array:
        ast.type = v.type

    case Type_Pointer:
        ast.type = v.type
        if v.kind == .Single {
            checker_error(c, "Cannot index a single pointer. Did you want to use multi-pointer instead? (^ vs [^])")
        }

    case:
        assert(false)
    }

    #partial switch v in expr.index.value {
    case i128:
        #partial switch lv in expr.left.type.variant {
        case Type_Array:
            if v < 0 || int(v) > lv.len {
                checker_error(c, "Constant index is out of bounds")
            }
        }

    case nil:
        break
    case:
        assert(false)
    }
}

check_address_expr :: proc(c: ^Checker, ast: ^Ast) {
    assert(ast.type == nil)

    expr := ast.variant.(Ast_Address_Expr)
    check_expr(c, expr.expr)

    ptr_type := new(Type)
    // HACK
    ptr_type.size = 8
    ptr_type.variant = Type_Pointer{
        kind = .Single,
        type = expr.expr.type,
    }

    ast.type = find_or_create_type(c, ptr_type)
}

check_deref_expr :: proc(c: ^Checker, ast: ^Ast) {
    assert(ast.type == nil)

    expr := ast.variant.(Ast_Deref_Expr)
    check_expr(c, expr.expr)
    #partial switch v in expr.expr.type.variant {
    case Type_Pointer:
    case:
        checker_error(c, "Only pointer types can be de-referenced, got: {}", type_to_string(expr.expr.type))
    }
}

check_binary_expr :: proc(c: ^Checker, ast: ^Ast, type_hint: ^Type = nil) {
    assert(ast.type == nil)

    expr := ast.variant.(Ast_Binary_Expr)
    ast.type = check_binary_op(c, expr.left, expr.right, expr.op.kind)

    // Constant folding
    #partial switch l in expr.left.value {
    case i128:
        #partial switch r in expr.right.value {
        case i128:
            #partial switch expr.op.kind {
            case .Equal:              ast.value = l == r
            case .Less_Than:          ast.value = l < r
            case .Less_Than_Equal:    ast.value = l <= r
            case .Greater_Than:       ast.value = l > r
            case .Greater_Than_Equal: ast.value = l >= r
            case .Not_Equal:          ast.value = l != r

            case .Add: ast.value = l + r
            case .Sub: ast.value = l - r
            case .Mul: ast.value = l * r
            case .Div: ast.value = l / r
            case .Mod: ast.value = l % r

            case .Bit_And:          ast.value = l & r
            case .Bit_Or:           ast.value = l | r
            case .Bit_Xor:          ast.value = l ~ r
            case .Bit_Shift_Left:   ast.value = l << uint(r)
            case .Bit_Shift_Right:  ast.value = l >> uint(r)

            case:
                assert(false)
            }

        case: assert(false)
        }
    }
}

check_value_decl :: proc(c: ^Checker, ast: ^Ast) {
    assert(ast.type == nil)

    decl := ast.variant.(Ast_Value_Decl)
    check_type(c, decl.type)
    // check_ident(c, value.name)

    if decl.vector {
        ast_print(ast, "", 0)
        fmt.println(type_to_string(decl.type.type))
        // NOTE: things like this could be cached on the scalar type, but idgaf for now
        vec := type_vectorize(decl.type.type)
        decl.type.type = find_or_create_type(c, vec)
    }

    if decl.value != nil {
        check_expr(c, decl.value)
        // TODO check same as in assign stmt
        if decl.type.type != decl.value.type {
            checker_error(c, "Initializer value type doesn't match, expected {}, got {}", type_to_string(decl.type.type), type_to_string(decl.value.type))
        }
        ast.value = decl.value.value
    }
    ast.type = decl.type.type
}

check_proc_param_field :: proc(c: ^Checker, ast: ^Ast) {
    decl := ast.variant.(Ast_Value_Decl)
    check_type(c, decl.type)
    ast.type = decl.type.type
}

check_stmt :: proc(c: ^Checker, ast: ^Ast) {
    #partial switch v in ast.variant {
    case Ast_Value_Decl:
        check_value_decl(c, ast)

    case Ast_Assign_Stmt:
        op, op_ok := token_normalize_assign_op(v.op.kind)
        if !op_ok {
            checker_error(c, "Invalid assign statement op")
        }

        #partial switch v.op.kind {
        case .Assign:
            check_expr(c, v.left)
            check_expr(c, v.right, type_hint = v.left.type)

            if v.left.type == v.right.type do return
            if vec, ok := v.left.type.variant.(Type_Array);
               ok && vec.kind == .Vector && vec.type == v.right.type {
                return
            }
            checker_error(c, "Assign statement types don't match")

        case:
            ast.type = check_binary_op(c, v.left, v.right, op)
        }

    case Ast_Return_Stmt:
        check_return_stmt(c, ast)

    case Ast_Call_Expr:
        check_call_expr(c, ast)

    case Ast_For_Stmt:
        check_begin_scope(c, v.scope)
        check_value_decl(c, v.init)
        check_expr(c, v.cond)
        check_stmt(c, v.post)
        check_block_stmt(c, v.body)
        check_end_scope(c)

    case Ast_For_Range_Stmt:
        iter := v.ident.variant.(Ast_Ident).token.text

    case Ast_If_Stmt:
        check_if_stmt(c, ast)

    case Ast_Break_Stmt:
        // for s := c.curr_scope; s != nil; s = s.parent {
        // }

    case Ast_Continue_Stmt:

    case Ast_Block_Stmt:
        check_block_stmt(c, ast)
    }
}

check_block_stmt :: proc(c: ^Checker, ast: ^Ast) {
    block := ast.variant.(Ast_Block_Stmt)

    check_begin_scope(c, block.scope)

    for stmt, i in block.statements {
        check_stmt(c, stmt)
        #partial switch v in stmt.variant {
        case Ast_Return_Stmt:
            if i != len(block.statements) - 1 {
                assert(false)
                checker_error(c, "Statements after return statement will never get executed")
            }
        }
    }

    check_end_scope(c)
}

check_if_stmt :: proc(c: ^Checker, ast: ^Ast) {
    stmt := ast.variant.(Ast_If_Stmt)
    check_expr(c, stmt.cond)

    if !type_is_boolean(stmt.cond.type) {
        checker_error(c, "Condition in an if statement must have boolean type")
    }

    check_block_stmt(c, stmt.if_body)
    if stmt.else_body != nil {
        check_stmt(c, stmt.else_body)
    }
}

check_return_stmt :: proc(c: ^Checker, ast: ^Ast) {
    stmt := ast.variant.(Ast_Return_Stmt)
    check_expr(c, stmt.value)
    proc_decl := c.curr_entity.ast.variant.(Ast_Proc_Decl)
    proc_type := proc_decl.type.variant.(Ast_Proc_Type)
    if stmt.value.type != proc_type.result.type {
        checker_error(c, "Invalid return type")
    }
}

check_begin_scope :: proc(c: ^Checker, scope: ^Scope) -> ^Scope {
    assert(scope.parent == c.curr_scope)
    prev_scope := c.curr_scope
    c.curr_scope = scope
    return prev_scope
}

check_end_scope :: proc(c: ^Checker) {
    c.curr_scope = c.curr_scope.parent
}

check_proc_type :: proc(c: ^Checker, ast: ^Ast, name: string) {
    type := ast.variant.(Ast_Proc_Type)

    if type.result != nil {
        check_type(c, type.result)
        ast.type = type.result.type
    }

    for param, i in type.params {
        check_proc_param_field(c, param)
    }
}

create_new_type_from_ast :: proc(c: ^Checker, ast: ^Ast, type_hint: ^Type = nil) -> (result: ^Type) {
    #partial switch v in ast.variant {
    case Ast_Basic_Literal:
        hint_basic := type_elem_basic_type(type_hint)

        if hint_basic != nil {
            #partial switch v.token.kind {
            case .Integer:
                if type_is_integer(hint_basic) {
                    return hint_basic
                } else {
                    checker_error(c, "Integer literal cannot be assigned to non-integer-element type")
                }

            case .Float:
                // Accept ints without a fraction
                if type_is_numeric(hint_basic) {
                    return hint_basic
                } else {
                    checker_error(c, "Integer literal cannot be assigned to non-numeric-element type")
                }

            case .True, .False:
                if type_is_boolean(hint_basic) {
                    return hint_basic
                } else {
                    checker_error(c, "Boolean literal cannot be assigned to non-boolean-element type")
                }

            case .Char:
                if type_is_integer(hint_basic) {
                    return hint_basic
                } else {
                    checker_error(c, "Character literal cannot be assigned to non-integer-element type")
                }

            case .String:
                unimplemented("fuck strings tbh")
            }
        }

        #partial switch v.token.kind {
        case .Integer:      return c.basic_types[.I32]
        case .Float:        return c.basic_types[.F32]
        case .True, .False: return c.basic_types[.B8]
        case .Char:         return c.basic_types[.U8]
        case .String:       unimplemented("fuck strings tbh")
        }

    case Ast_Pointer_Type:
        result = new(Type)
        result.size = 8
        result.variant = Type_Pointer{
            kind = .Single,
            type = find_or_create_type_ast(c, v.type),
        }

    case Ast_Multi_Pointer_Type:
        result = new(Type)
        result.size = 8
        result.variant = Type_Pointer{
            kind = .Multi,
            type = find_or_create_type_ast(c, v.type),
        }

    case Ast_Array_Type:
        result = new(Type)
        result.size = 0 // TODO
        length := 0
        #partial switch l in v.len.value {
        case i128:
            length = int_cast(int, l)
        case:
            assert(false)
        }
        assert(length > 0)
        result.variant = Type_Array{
            kind = v.kind,
            len = length,
            type = find_or_create_type_ast(c, v.type),
        }

    case Ast_Struct_Type:
        result = new(Type)
        str: Type_Struct
        str.fields = make([]Type_Struct_Field, len(v.fields))
        for field, i in v.fields {
            f := field.variant.(Ast_Field)
            str.fields[i] = {
                name = f.name.variant.(Ast_Ident).token.text,
                type = find_or_create_type_ast(c, f.type),
            }
        }
        result.variant = str

    case Ast_Field:
        result = find_or_create_type_ast(c, v.type)

    case:
        assert(false, "AST is not a valid type")
    }

    return result
}

find_or_create_type_ast :: proc(c: ^Checker, ast: ^Ast, type_hint: ^Type = nil) -> (result: ^Type) {
    #partial switch v in ast.variant {
    case Ast_Ident:
        switch v.token.text {
        case "b8", "bool"   : result = c.basic_types[.B8 ]
        case "b16"          : result = c.basic_types[.B16]
        case "b32"          : result = c.basic_types[.B32]
        case "b64"          : result = c.basic_types[.B64]
        case "i8"           : result = c.basic_types[.I8 ]
        case "i16"          : result = c.basic_types[.I16]
        case "i32"          : result = c.basic_types[.I32]
        case "i64"          : result = c.basic_types[.I64]
        case "u8"           : result = c.basic_types[.U8 ]
        case "u16"          : result = c.basic_types[.U16]
        case "u32"          : result = c.basic_types[.U32]
        case "u64"          : result = c.basic_types[.U64]
        case "f32", "float" : result = c.basic_types[.F32]
        case "f64"          : result = c.basic_types[.F64]

        case:
            if ent, _, ok := find_entity(c.curr_scope, v.token.text); ok {
                #partial switch v in ent.variant {
                // TODO: alias
                case Entity_Struct:
                    result = v.type

                case:
                    assert(false)
                }
            } else {
                assert(false)
            }
        }

    case:
        result = find_or_create_type(c, type = create_new_type_from_ast(c, ast, type_hint = type_hint))
    }

    assert(result != nil)

    return result
}

// If an already existing match is found, the match gets returned.
// Otherwise the input type will get registered as a new type and reused later!
find_or_create_type :: proc(c: ^Checker, type: ^Type) -> ^Type {
    switch &v in type.variant {
    case Type_Basic:
        return c.basic_types[v.kind]

    case Type_Array:
        v.type = find_or_create_type(c, v.type)

    case Type_Pointer:
        v.type = find_or_create_type(c, v.type)

    case Type_Struct:
        for &field in v.fields {
            field.type = find_or_create_type(c, field.type)
        }
    }

    // Linear search, bit dumb. Could hash some type info to split into buckets.
    for t in c.types {
        if types_equal(t, type) {
            return t
        }
    }

    append(&c.types, type)

    return type
}

check_type_decl :: proc(c: ^Checker, ast: ^Ast, name: string) {

}

check_scope_structs_recursive :: proc(c: ^Checker, scope: ^Scope) {
    for name, ent in scope.entities {
        c.curr_entity = ent

        #partial switch &v in ent.variant {
        case Entity_Struct:
            if ent.ast == nil do break
            decl := ent.ast.variant.(Ast_Struct_Decl) or_break
            check_type(c, decl.type)
            ent.ast.type = decl.type.type
            v.type = decl.type.type
        }
    }

    for child in scope.children {
        c.curr_scope = child
        check_scope_structs_recursive(c, child)
        c.curr_scope = c.curr_scope.parent
    }
}

check_scope_procedures_recursive :: proc(c: ^Checker, scope: ^Scope) {
    for name, ent in scope.entities {
        c.curr_entity = ent

        ast_print(ent.ast, "", 0)

        #partial switch v in ent.variant {
        case Entity_Proc:
            decl := ent.ast.variant.(Ast_Proc_Decl)
            check_proc_type(c, decl.type, name)
            ent.ast.type = decl.type.type
        }
    }

    for child in scope.children {
        c.curr_scope = child
        check_scope_procedures_recursive(c, child)
        c.curr_scope = c.curr_scope.parent
    }
}

// Check and codegen C
check_program :: proc(c: ^Checker) {
    c.basic_types = [Type_Basic_Kind]^Type{
        .B8  = new_clone(Type{size = 1, variant = Type_Basic{kind = .B8 }}),
        .B16 = new_clone(Type{size = 4, variant = Type_Basic{kind = .B16}}),
        .B32 = new_clone(Type{size = 4, variant = Type_Basic{kind = .B32}}),
        .B64 = new_clone(Type{size = 4, variant = Type_Basic{kind = .B64}}),
        .I8  = new_clone(Type{size = 4, variant = Type_Basic{kind = .I8 }}),
        .I16 = new_clone(Type{size = 4, variant = Type_Basic{kind = .I16}}),
        .I32 = new_clone(Type{size = 4, variant = Type_Basic{kind = .I32}}),
        .I64 = new_clone(Type{size = 4, variant = Type_Basic{kind = .I64}}),
        .U8  = new_clone(Type{size = 4, variant = Type_Basic{kind = .U8 }}),
        .U16 = new_clone(Type{size = 4, variant = Type_Basic{kind = .U16}}),
        .U32 = new_clone(Type{size = 4, variant = Type_Basic{kind = .U32}}),
        .U64 = new_clone(Type{size = 4, variant = Type_Basic{kind = .U64}}),
        .F32 = new_clone(Type{size = 4, variant = Type_Basic{kind = .F32}}),
        .F64 = new_clone(Type{size = 4, variant = Type_Basic{kind = .F64}}),
    }

    for t in c.basic_types {
        append(&c.types, t)
    }

    v8i32_type := find_or_create_type(c, new_clone(Type{variant = Type_Array{
        kind = .Vector, len = 8, type = c.basic_types[.I32],
    }}))

    create_entity(c.curr_file_scope, "vector_width", nil, Entity_Builtin{
        kind = .Vector_Index, value = VECTOR_WIDTH, type = c.basic_types[.I32]})
    create_entity(c.curr_file_scope, "vector_index", nil, Entity_Builtin{
        kind = .Vector_Index, value = [8]i32{0, 1, 2, 3, 4, 5, 6, 7}, type = v8i32_type})

    // 1. check all types
    // 2. check all entity declarations
    // 3. check procedure bodies - needs proc declarations for checking

    check_scope_structs_recursive(c, c.curr_file_scope)

    for name, ent in c.curr_file_scope.entities {
        c.curr_entity = ent

        ast_print(ent.ast, "", 0)

        #partial switch v in ent.variant {
        case Entity_Variable:
            decl := ent.ast.variant.(Ast_Value_Decl)
            if decl.scope != c.curr_file_scope {
                continue
            }

            check_value_decl(c, ent.ast)
            ent.ast.type = decl.type.type
        }
    }

    check_scope_procedures_recursive(c, c.curr_file_scope)

    for name, ent in c.curr_file_scope.entities {
        c.curr_entity = ent

        #partial switch v in ent.variant {
        case Entity_Proc:
            fmt.println("\nPROC ENTITY", name, ent)
            decl := ent.ast.variant.(Ast_Proc_Decl)
        }
    }

    proc_entities := make([dynamic]^Entity)

    for name, ent in c.curr_file_scope.entities {
        #partial switch v in ent.variant {
        case Entity_Proc:
            fmt.println("\nPROC ENTITY", name, ent)
            append(&proc_entities, ent)
        }
    }

    // for name, ent in c.curr_file_scope.entities {
    for ent in proc_entities {
        c.curr_entity = ent

        #partial switch v in ent.variant {
        case Entity_Proc:
            // fmt.println("\nPROC ENTITY", name, ent)
            decl := ent.ast.variant.(Ast_Proc_Decl)
            check_block_stmt(c, decl.body)
        }
    }
}