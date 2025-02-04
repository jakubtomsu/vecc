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
}

Entity :: struct {
    ast:            ^Ast,
    name:           string,
    order_index:    int,
    variant:        Entity_Variant,
    depends:        map[string]struct{}, // identifiers
}

Entity_Variant :: union {
    Entity_Proc,
    Entity_Variable,
    Entity_Type,
    Entity_Alias,
    Entity_Struct,
    Entity_Struct_Field,
}

Entity_Proc :: struct {

}

Entity_Variable :: struct {

}

Entity_Type :: struct {
    type:   ^Type,
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

checker_error :: proc(c: ^Checker, format: string, args: ..any) {
    // fmt.eprintf("[{}] %s(%d:%d) ", loc, p.filename, pos.line, pos.column)
	fmt.eprintf(format, ..args)
	fmt.eprintln()
	os.exit(1)
}

check_ident :: proc(c: ^Checker, ast: ^Ast) {
    ident := ast.variant.(Ast_Ident)
    name := ident.token.text
    if ent, ok := find_entity(c.curr_scope, name).?; ok {
        ast.type = ent.ast.type
    } else {
        checker_error(c, "Ident not found: {}", name)
    }
}

check_type :: proc(c: ^Checker, ast: ^Ast) {
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
    }
    ast.type = find_or_create_type_entity(c, ast)
}

check_basic_literal :: proc(c: ^Checker, ast: ^Ast, type_hint: ^Type = nil) {
    lit := ast.variant.(Ast_Basic_Literal)
    ast.type = find_or_create_type_entity(c, ast)
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
    call_expr := ast.variant.(Ast_Call_Expr)

    check_ident(c, call_expr.procedure)

    // HACK
    procedure := find_entity(c.curr_scope, call_expr.procedure.variant.(Ast_Ident).token.text).?
    proc_decl := procedure.ast.variant.(Ast_Proc_Decl)
    proc_type := proc_decl.type.variant.(Ast_Proc_Type)

    for arg, i in call_expr.args {
        check_expr(c, arg)
        if arg.type != proc_type.params[i].type {
            checker_error(c, "Invalid argument type")
        }
    }

    ast.type = proc_type.result.type
}

check_cast_expr :: proc(c: ^Checker, ast: ^Ast) {
    expr := ast.variant.(Ast_Cast_Expr)

    check_type(c, expr.type)
    check_expr(c, expr.value)

    ast.type = expr.type.type
}

check_urnary_expr :: proc(c: ^Checker, ast: ^Ast) {
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

        if !type_is_boolean(expr.expr.type) {
            checker_error(c, "Unary 'Not' operator can be only applied to boolean values")
        }

    case:
        checker_error(c, "Invalid unary operator: ", expr.op)
    }
}

check_binary_op :: proc(c: ^Checker, left: ^Ast, right: ^Ast, op: Token_Kind) -> ^Type {
    check_expr(c, left)
    check_expr(c, right)

    if left.type != right.type {
        fmt.printfln("%p", left.type)
        fmt.printfln("%p", right.type)
        checker_error(c, "Types in binary expression don't match: {} vs {}",
            type_to_string(left.type), type_to_string(right.type))
    }

    type := left.type

    #partial switch op {
    case .Equal,
         .Less_Than,
         .Less_Than_Equal,
         .Greater_Than,
         .Greater_Than_Equal,
         .Not_Equal:
        // HACK
        type = find_entity(c.curr_file_scope, "bool").?.variant.(Entity_Type).type

        if !type_is_numeric(left.type) {
            checker_error(c, "{} operator can be only applied to numeric values", op)
        }

    case .Add,
         .Sub,
         .Mul,
         .Div:

        if !type_is_numeric(left.type) {
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

    #partial switch v in ast.variant {
    case Ast_Basic_Literal:
        check_basic_literal(c, ast)

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

    case:
        assert(false)
    }
}

check_selector_expr :: proc(c: ^Checker, ast: ^Ast) {
    expr := ast.variant.(Ast_Selector_Expr)
    check_expr(c, expr.left)

    {
        ident := expr.right.variant.(Ast_Ident)
        name := ident.token.text

        #partial switch v in expr.left.type.variant {
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
            all_fields := "xyzw"
            fields := all_fields[:v.len]

            if len(name) != 1 {
                break
            }

            for field in transmute([]u8)fields {
                if field != name[0] {
                    continue
                }

                expr.right.type = v.type
            }
        }

        if expr.right.type == nil {
            checker_error(c, "Invalid field")
        }
    }

    ast.type = expr.right.type
}

check_index_expr :: proc(c: ^Checker, ast: ^Ast) {
    expr := ast.variant.(Ast_Index_Expr)
    check_expr(c, expr.left)
    check_expr(c, expr.index)

    if !type_is_integer(expr.index.type) {
        checker_error(c, "Index must be of integer type, got", type_to_string(expr.index.type))
    }

    #partial switch v in expr.left.type.variant {
    case Type_Array:
        ast.type = v.type

    case:
        assert(false)
    }

    #partial switch v in expr.index.value {
    case i128:
        arr := expr.left.type.variant.(Type_Array)
        if v < 0 || int(v) > arr.len {
            checker_error(c, "Constant index is out of bounds")
        }

    case nil:
        break
    case:
        assert(false)
    }
}


check_binary_expr :: proc(c: ^Checker, ast: ^Ast, type_hint: ^Type = nil) {
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
    decl := ast.variant.(Ast_Value_Decl)
    check_type(c, decl.type)
    // check_ident(c, value.name)

    if decl.value != nil {
        check_expr(c, decl.value)
        if decl.type.type != decl.value.type {
            checker_error(c, "Initializer value type doesn't match")
        }
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
        check_expr(c, v.left)

        op: Token_Kind
        #partial switch v.op.kind {
        case .Assign_Add:               op = .Add
        case .Assign_Sub:               op = .Sub
        case .Assign_Mul:               op = .Mul
        case .Assign_Div:               op = .Div
        case .Assign_Mod:               op = .Mod
        case .Assign_Bit_And:           op = .Bit_And
        case .Assign_Bit_Or:            op = .Bit_Or
        case .Assign_Bit_Xor:           op = .Bit_Xor
        case .Assign_Bit_Shift_Left:    op = .Bit_Shift_Left
        case .Assign_Bit_Shift_Right:   op = .Bit_Shift_Right
        case .Assign:
            check_expr(c, v.right)
            if v.left.type != v.right.type {
                checker_error(c, "Assign statement types don't match")
            }
            return
        case:
            assert(false)
        }

        check_expr(c, v.right)
        ast.type = check_binary_op(c, v.left, v.right, op)

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


// Note: the name is used for comparing if types are the same so it has to be unique.
// There is probably a better way to generate an unique type ID but this is good enough for now.
ast_type_to_id :: proc(c: ^Checker, ast: ^Ast) -> string {
    #partial switch v in ast.variant {
    case Ast_Ident:
        return v.token.text

    // HACK: TODO: support untyped literals
    // TODO: type hints here?
    case Ast_Basic_Literal:
        #partial switch v.token.kind {
        case .Float:
            return "f32"
        case .Integer:
            return "i32"
        case .True, .False:
            return "bool"
        }

    case Ast_Array_Type:
        return fmt.tprintf("{}[{}]{}", v.kind, v.len.value.(i128), ast_type_to_id(c, v.type))

    case Ast_Pointer_Type:
        return fmt.tprintf("^{}", ast_type_to_id(c, v.type))

    case Ast_Multi_Pointer_Type:
        return fmt.tprintf("[^]{}", ast_type_to_id(c, v.type))

    case Ast_Struct_Decl:
        return ast_type_to_id(c, v.type)

    case Ast_Struct_Type:
        fields := make([]string, len(v.fields))
        for field, i in v.fields {
            fields[i] = strings.concatenate({ast_type_to_id(c, field), ";"})
        }
        return fmt.tprintf("struct{{{}}}", strings.concatenate(fields))

    case Ast_Field:
        return fmt.tprintf("{}.{}", ast_type_to_id(c, v.name), ast_type_to_id(c, v.type))

    case:
        assert(false, "AST is not a valid type")
    }

    return ""
}

create_new_type_from_ast :: proc(c: ^Checker, id: string, ast: ^Ast) -> (result: ^Type) {
    #partial switch v in ast.variant {
    case Ast_Pointer_Type:
        result = new(Type)
        result.size = 8
        result.variant = Type_Pointer{
            kind = .Single,
            type = find_or_create_type_entity(c, v.type),
        }

    case Ast_Multi_Pointer_Type:

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
            type = find_or_create_type_entity(c, v.type),
        }

    case Ast_Struct_Type:
        result = new(Type)
        str: Type_Struct
        str.fields = make([]Type_Struct_Field, len(v.fields))
        for field, i in v.fields {
            f := field.variant.(Ast_Field)
            str.fields[i] = {
                name = f.name.variant.(Ast_Ident).token.text,
                type = find_or_create_type_entity(c, f.type),
            }
        }
        result.variant = str

    case Ast_Field:
        result = find_or_create_type_entity(c, v.type)

    case:
        assert(false, "AST is not a valid type")
    }

    return result
}

find_or_create_type_entity :: proc(c: ^Checker, ast: ^Ast, type_hint: ^Type = nil) -> ^Type {
    id := ast_type_to_id(c, ast)

    fmt.println("find or create type:", id)

    if ent, ok := find_entity(c.curr_scope, id).?; ok {
        #partial switch v in ent.variant {
        case Entity_Type:
            return v.type

        case Entity_Struct:
            return v.type

        case:
            assert(false)
        }
    }

    type := create_new_type_from_ast(c, id, ast)
    create_type_entity(c.curr_file_scope, id, type, ast)

    return type
}

create_type_entity :: proc(scope: ^Scope, id: string, type: ^Type, ast: ^Ast) -> ^Entity {
    fmt.println("create type:", id)
    ent := new(Entity)
    ent.ast = ast
    ent.order_index = g_entity_order_counter
    g_entity_order_counter += 1
    ent.variant = Entity_Type{
        type = type,
    }
    scope.entities[id] = ent
    return ent
}

check_type_decl :: proc(c: ^Checker, ast: ^Ast, name: string) {

}

// Check and codegen C
check_program :: proc(c: ^Checker) {
    create_type_entity(c.curr_file_scope, "bool",new_clone(Type{size = 1, variant = Type_Basic{kind = .B8 }}), nil)
    create_type_entity(c.curr_file_scope, "b8" , new_clone(Type{size = 1, variant = Type_Basic{kind = .B8 }}), nil)
    create_type_entity(c.curr_file_scope, "b16", new_clone(Type{size = 4, variant = Type_Basic{kind = .B16}}), nil)
    create_type_entity(c.curr_file_scope, "b32", new_clone(Type{size = 4, variant = Type_Basic{kind = .B32}}), nil)
    create_type_entity(c.curr_file_scope, "b64", new_clone(Type{size = 4, variant = Type_Basic{kind = .B64}}), nil)
    create_type_entity(c.curr_file_scope, "i8" , new_clone(Type{size = 4, variant = Type_Basic{kind = .I8 }}), nil)
    create_type_entity(c.curr_file_scope, "i16", new_clone(Type{size = 4, variant = Type_Basic{kind = .I16}}), nil)
    create_type_entity(c.curr_file_scope, "i32", new_clone(Type{size = 4, variant = Type_Basic{kind = .I32}}), nil)
    create_type_entity(c.curr_file_scope, "i64", new_clone(Type{size = 4, variant = Type_Basic{kind = .I64}}), nil)
    create_type_entity(c.curr_file_scope, "u8" , new_clone(Type{size = 4, variant = Type_Basic{kind = .U8 }}), nil)
    create_type_entity(c.curr_file_scope, "u16", new_clone(Type{size = 4, variant = Type_Basic{kind = .U16}}), nil)
    create_type_entity(c.curr_file_scope, "u32", new_clone(Type{size = 4, variant = Type_Basic{kind = .U32}}), nil)
    create_type_entity(c.curr_file_scope, "u64", new_clone(Type{size = 4, variant = Type_Basic{kind = .U64}}), nil)
    create_type_entity(c.curr_file_scope, "f32", new_clone(Type{size = 4, variant = Type_Basic{kind = .F32}}), nil)
    create_type_entity(c.curr_file_scope, "f64", new_clone(Type{size = 4, variant = Type_Basic{kind = .F64}}), nil)

    // 1. check all types
    // 2. check all entity declarations
    // 3. check procedure bodies - needs proc declarations for checking

    for name, ent in c.curr_scope.entities {
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

    for name, ent in c.curr_scope.entities {
        c.curr_entity = ent

        #partial switch v in ent.variant {
        case Entity_Proc:
            decl := ent.ast.variant.(Ast_Proc_Decl)
            check_proc_type(c, decl.type, name)
            ent.ast.type = decl.type.type
        }
    }

    for name, ent in c.curr_scope.entities {
        c.curr_entity = ent

        #partial switch v in ent.variant {
        case Entity_Proc:
            decl := ent.ast.variant.(Ast_Proc_Decl)
            check_block_stmt(c, decl.body)
        }
    }
}