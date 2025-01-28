// Assign types to AST nodes, resolve identifiers.
package vecc

import "core:fmt"
import "core:strings"
import "core:strconv"
import "core:slice"

Checker :: struct {
    filename:           string,
    curr_scope:         ^Scope,
    curr_lanes:         int,
    curr_entity:        ^Entity,
    curr_file_scope:    ^Scope,
}

Entity :: struct {
    ast:            ^Ast,
    order_index:    int,
    variant:        Entity_Variant,
    depends:        map[string]struct{}, // identifiers
}

Entity_Variant :: union {
    Entity_Proc,
    Entity_Variable,
    Entity_Type,
    Entity_Alias,
}

Entity_Proc :: struct {

}

Entity_Variable :: struct {

}

Entity_Type :: struct {
    type: ^Type,
}

Entity_Alias :: struct {
    entity: ^Entity,
}

Scope :: struct {
    ast:        ^Ast,
    parent:     ^Scope,
    children:   [dynamic]^Scope,
    depth:      int,
    entities:   map[string]^Entity,
}

Type :: struct {
    size:       int,
    variant:    Type_Variant,
    cname:      string,
}

Type_Variant :: union {
    Type_Basic,
    Type_Array,
    Type_Pointer,
}

Type_Basic :: struct {
    kind:   Type_Basic_Kind,
}

Type_Basic_Kind :: enum u8 {
    B8,
    B16,
    B32,
    B64,
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    F32,
    F64,
}

Type_Array :: struct {
    kind:   Type_Array_Kind,
    len:    int,
    type:   ^Type,
}

Type_Array_Kind :: enum u8 {
    Fixed_Array = 0,
    Vector,
}

Type_Pointer :: struct {
    kind:   Type_Pointer_Kind,
    type:   ^Type,
}

Type_Pointer_Kind :: enum u8 {
    Single = 0,
    Multi,
}

Type_Struct :: struct {
    fields: [dynamic]Type_Struct_Field,
}

Type_Struct_Field :: struct {
    name:   string,
    type:   ^Type,
}

check_ident :: proc(c: ^Checker, ast: ^Ast) {
    ident := ast.variant.(Ast_Ident)
    name := ident.token.text
    if ent, ok := find_entity(c.curr_scope, name).?; ok {
        ast.type = ent.ast.type
    } else {
        assert(false, "Ident not found")
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
                assert(false)
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

    for arg, i in call_expr.args {
        check_expr(c, arg)
    }

    // ast.type = call_expr.procedure.type
}

check_cast_expr :: proc(c: ^Checker, ast: ^Ast) {
    expr := ast.variant.(Ast_Cast_Expr)

    check_type(c, expr.type)
    check_expr(c, expr.value)

    ast.type = expr.type.type

}

check_urnary_op :: proc(c: ^Checker, ast: ^Ast, op: Token_Kind) {

}

check_binary_op :: proc(c: ^Checker, left: ^Ast, right: ^Ast, op: Token_Kind) -> ^Type {
    check_expr(c, left)
    check_expr(c, right)

    if left.type != right.type {
        assert(false)
    }

    return left.type
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
        check_urnary_op(c, v.expr, v.op.kind)

    case Ast_Cast_Expr:
        check_cast_expr(c, ast)

    case Ast_Binary_Expr:
        check_binary_expr(c, ast)

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
            assert(false)
        }
    }
    ast.type = decl.type.type
}

check_proc_param_field :: proc(c: ^Checker, ast: ^Ast) {
    field := ast.variant.(Ast_Field)
    check_type(c, field.type)
    ast.type = field.type.type
}

check_stmt :: proc(c: ^Checker, ast: ^Ast) {
    #partial switch v in ast.variant {
    case Ast_Value_Decl:
        check_value_decl(c, ast)

    case Ast_Assign_Stmt:
        check_ident(c, v.left)

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
                assert(false)
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
        check_value_decl(c, v.init)
        check_expr(c, v.cond)
        check_stmt(c, v.post)
        check_block_stmt(c, v.body)

    case Ast_For_Range_Stmt:
        iter := v.ident.variant.(Ast_Ident).token.text

    case Ast_If_Stmt:
        check_expr(c, v.cond)
        check_block_stmt(c, v.if_body)
        if v.else_body != nil {
            check_stmt(c, v.else_body)
        }

    case Ast_Break_Stmt:

    case Ast_Continue_Stmt:

    case Ast_Block_Stmt:
        check_block_stmt(c, ast)
    }
}

check_block_stmt :: proc(c: ^Checker, ast: ^Ast, skip_scope := false) {
    block := ast.variant.(Ast_Block_Stmt)

    prev_scope: ^Scope
    if !skip_scope {
        prev_scope = check_begin_scope(c, block.scope)
    }

    for stmt in block.statements {
        check_stmt(c, stmt)
    }

    if !skip_scope {
        check_end_scope(c, prev_scope)
    }
}

check_return_stmt :: proc(c: ^Checker, ast: ^Ast) {
    stmt := ast.variant.(Ast_Return_Stmt)
    check_expr(c, stmt.value)
    proc_decl := c.curr_entity.ast.variant.(Ast_Proc_Decl)
    proc_type := proc_decl.type.variant.(Ast_Proc_Type)
    if stmt.value.type != proc_type.result.type {
        assert(false)
    }
}

check_begin_scope :: proc(c: ^Checker, scope: ^Scope, name := "") -> ^Scope {
    prev_scope := c.curr_scope
    c.curr_scope = scope
    return prev_scope
}

check_end_scope :: proc(c: ^Checker, prev_scope: ^Scope) {
    c.curr_scope = prev_scope
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


// Note: the name is used for comparing if types are the same so it has to be unique
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
        return fmt.tprintf("{}[{}]{}", v.kind, v.len, ast_type_to_id(c, v.type))

    case Ast_Pointer_Type:
        return fmt.tprintf("^{}", ast_type_to_id(c, v.type))

    case Ast_Multi_Pointer_Type:
        return fmt.tprintf("[^]{}", ast_type_to_id(c, v.type))

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

    case:
        assert(false, "AST is not a valid type")
    }

    return result
}

find_or_create_type_entity :: proc(c: ^Checker, ast: ^Ast, type_hint: ^Type = nil) -> ^Type {
    id := ast_type_to_id(c, ast)

    if ent, ok := find_entity(c.curr_scope, id).?; ok {
        return ent.variant.(Entity_Type).type
    }

    type := create_new_type_from_ast(c, id, ast)
    create_type_entity(c.curr_file_scope, id, type, ast)

    return type
}

create_type_entity :: proc(scope: ^Scope, id: string, type: ^Type, ast: ^Ast) -> ^Entity {
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

// Check and codegen C
check_program :: proc(c: ^Checker) {
    // HACK
    create_type_entity(c.curr_file_scope, "i32", new_clone(Type{size = 4, variant = Type_Basic{kind = .I32}}), nil)
    create_type_entity(c.curr_file_scope, "f32", new_clone(Type{size = 4, variant = Type_Basic{kind = .F32}}), nil)

    // 1. check all types
    // 2. check all entity declarations
    // 3. check procedure bodies - needs proc declarations for checking

    for name, ent in c.curr_scope.entities {
        ast_print(ent.ast, name, 0)
        c.curr_entity = ent

        #partial switch v in ent.variant {
        case Entity_Proc:
            decl := ent.ast.variant.(Ast_Proc_Decl)
            check_proc_type(c, decl.type, name)
            ent.ast.type = decl.type.type
        }
    }

    for name, ent in c.curr_scope.entities {
        ast_print(ent.ast, name, 0)
        c.curr_entity = ent

        #partial switch v in ent.variant {
        case Entity_Proc:
            decl := ent.ast.variant.(Ast_Proc_Decl)
            check_block_stmt(c, decl.body)
        }
    }
}