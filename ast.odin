package vecc

import "core:fmt"
import "core:reflect"

Ast :: struct {
    variant:        Ast_Variant,
    type:           ^Type,
    value:          Value, // compile time known only
    order_index:    int,
}

Value :: union {
    bool,
    i128,
    f64,
    [8]i32, // HACK
    [8]f32
}

Ast_Variant :: union {
    Ast_Ident,
    Ast_Basic_Literal,
    Ast_Compound_Literal,

    Ast_Proc_Decl,
    Ast_Value_Decl,
    Ast_Struct_Decl,
    Ast_Field,

    Ast_Block_Stmt,
    Ast_If_Stmt,
    Ast_For_Stmt,
    Ast_Break_Stmt,
    Ast_Continue_Stmt,
    Ast_For_Range_Stmt,
    Ast_Assign_Stmt,
    Ast_Return_Stmt,

    Ast_Unary_Expr,
    Ast_Binary_Expr,
    Ast_Call_Expr,
    Ast_Cast_Expr,
    Ast_Selector_Expr,
    Ast_Index_Expr,
    Ast_Address_Expr,
    Ast_Deref_Expr,

    Ast_Proc_Type,
    Ast_Struct_Type,

    Ast_Pointer_Type,
    Ast_Multi_Pointer_Type,
    Ast_Array_Type,
}

Ast_Ident :: struct {
    token: Token,
}

Ast_Basic_Literal :: struct {
    token: Token,
}

Ast_Compound_Literal :: struct {
    type:   ^Ast,
    elems:  []^Ast,
}

Ast_Value_Decl :: struct {
    private:    bool,
    export:     bool,
    vector:     Value_Vectorization,
    mut:        Value_Mutablity,
    scope:      ^Scope,
    entity:     ^Entity,
    name:       ^Ast,
    type:       ^Ast,
    value:      ^Ast,
}

Value_Vectorization :: enum u8 {
    None = 0,
    Default,
    Scalar,
    Vector,
}

Value_Mutablity :: enum u8 {
    Invalid = 0,
    Mutable,
    Immutable,
    Constant, // compile-time
}

value_mutability_from_token :: proc(tok: Token_Kind) -> (Value_Mutablity, bool) #optional_ok {
    #partial switch tok {
    case .Mut:      return .Mutable, true
    case .Immut:    return .Immutable, true
    case .Const:    return .Constant, true
    }
    return .Mutable, false
}

Ast_Proc_Decl :: struct {
    export:     bool,
    private:    bool,
    scope:      ^Scope,
    entity:     ^Entity,
    type:       ^Ast,
    body:       ^Ast,
}

Ast_Field :: struct {
    name:   ^Ast,
    type:   ^Ast,
    value:  ^Ast,
}

Ast_Proc_Type :: struct {
    params: []^Ast,
    result: ^Ast,
}

Ast_Struct_Decl :: struct {
    entity: ^Entity,
    type:   ^Ast,
}

Ast_Block_Stmt :: struct {
    scope:      ^Scope,
    statements: []^Ast,
}

Ast_If_Stmt :: struct {
    cond:       ^Ast,
    if_body:    ^Ast,
    else_body:  ^Ast,
}

Ast_For_Stmt :: struct {
    scope:  ^Scope, // Scope of the persistent values, not the body
    init:   ^Ast,
    cond:   ^Ast,
    post:   ^Ast,
    body:   ^Ast,
}

Ast_For_Range_Stmt :: struct {
    ident:  ^Ast,
    start:  ^Ast,
    range:  Token,
    end:    ^Ast,
    body:   ^Ast,
}

Ast_Break_Stmt :: struct {
    token: Token,
}

Ast_Continue_Stmt :: struct {
    token: Token,
}

Ast_Assign_Stmt :: struct {
    op:     Token,
    left:   ^Ast,
    right:  ^Ast,
}

Ast_Return_Stmt :: struct {
    value:  ^Ast,
}

Ast_Unary_Expr :: struct {
    op:     Token,
    expr:   ^Ast,
}

Ast_Binary_Expr :: struct {
    op:     Token,
    left:   ^Ast,
    right:  ^Ast,
}

Ast_Call_Expr :: struct {
    procedure:  ^Ast,
    entity:     ^Entity,
    args:       []^Ast,
}

Ast_Cast_Expr :: struct {
    op:     Token, // conv or reinterpret
    type:   ^Ast,
    value:  ^Ast,
}

Ast_Selector_Expr :: struct {
    left:   ^Ast,
    right:  ^Ast,
}

Ast_Index_Expr :: struct {
    left:   ^Ast,
    index:  ^Ast,
}

Ast_Address_Expr :: struct {
    expr:  ^Ast,
}

Ast_Deref_Expr :: struct {
    expr:  ^Ast,
}


Ast_Struct_Type :: struct {
    fields: [dynamic]^Ast,
}

Ast_Pointer_Type :: struct {
    token:  Token,
    type:   ^Ast,
}

Ast_Multi_Pointer_Type :: struct {
    token:  Token,
    type:   ^Ast,
}

Ast_Array_Type :: struct {
    kind:   Type_Array_Kind,
    len:    ^Ast,
    type:   ^Ast,
}

ast_print :: proc(ast: ^Ast, name: string, depth: int) {
    depth := depth
    for i in 0..<depth {
        fmt.print("|   ")
    }

    if name != "" {
        if ast == nil {
            fmt.print(name, ":", "nil node\n")
            return
        }
        if ast.variant == nil {
            fmt.print(name, ":", "nil variant")
        } else {
            fmt.print(name, ":", reflect.union_variant_typeid(ast.variant))
        }
    } else {
        if ast == nil {
            fmt.print("nil node\n")
            return
        }
        if ast.variant == nil {
            fmt.print("nil variant")
        } else {
            fmt.print(reflect.union_variant_typeid(ast.variant))
        }
    }

    #partial switch v in ast.variant {
    case Ast_Ident:         fmt.print(" :", v.token.text)
    case Ast_Basic_Literal: fmt.print(" :", v.token.text)
    case Ast_Assign_Stmt:   fmt.print(" :", v.op.text)
    case Ast_Binary_Expr:   fmt.print(" :", v.op.text)
    case Ast_Unary_Expr:    fmt.print(" :", v.op.text)
    case Ast_Cast_Expr:     fmt.print(" :", v.op.text)
    case Ast_Break_Stmt:    fmt.print(" :", v.token.text)
    case Ast_Continue_Stmt: fmt.print(" :", v.token.text)
    case Ast_Value_Decl:    fmt.printf(" : {} export={} vector={} private={}", v.mut, v.export, v.vector, v.private)
    case Ast_Proc_Decl:     fmt.printf(" : export={} private={}", v.export, v.private)
    case Ast_Array_Type:    fmt.printf(" : kind={}", v.kind)
    }

    if ast.type != nil {
        fmt.printf("  ({})", type_to_string(ast.type))
    }

    fmt.println()

    depth += 1
    #partial switch v in ast.variant {
    case Ast_Block_Stmt:
        for it in v.statements {
            ast_print(it, "stmt", depth)
        }

    case Ast_Call_Expr:
        ast_print(v.procedure, "procedure", depth)
        for arg in v.args {
            ast_print(arg, "arg", depth)
        }

    case Ast_Cast_Expr:
        ast_print(v.type, "type", depth)
        ast_print(v.value, "value", depth)

    case Ast_Binary_Expr:
        ast_print(v.left, "left", depth)
        ast_print(v.right, "right", depth)

    case Ast_Index_Expr:
        ast_print(v.index, "index", depth)
        ast_print(v.left, "left", depth)

    case Ast_Address_Expr:
        ast_print(v.expr, "expr", depth)

    case Ast_Deref_Expr:
        ast_print(v.expr, "deref", depth)

    case Ast_Value_Decl:
        ast_print(v.name, "name", depth)
        ast_print(v.type, "type", depth)
        ast_print(v.value, "value", depth)

    case Ast_Proc_Decl:
        ast_print(v.type, "type", depth)
        ast_print(v.body, "body", depth)

    case Ast_Proc_Type:
        for it in v.params {
            ast_print(it, "param", depth)
        }
        ast_print(v.result, "result", depth)

    case Ast_Array_Type:
        ast_print(v.len, "len", depth)
        ast_print(v.type, "type", depth)

    case Ast_Assign_Stmt:
        ast_print(v.left, "left", depth)
        ast_print(v.right, "right", depth)

    case Ast_If_Stmt:
        ast_print(v.cond, "cond", depth)
        ast_print(v.if_body, "if_body", depth)
        ast_print(v.else_body, "else_body", depth)

    case Ast_For_Stmt:
        ast_print(v.init, "init", depth)
        ast_print(v.cond, "cond", depth)
        ast_print(v.post, "post", depth)
        ast_print(v.body, "body", depth)

    case Ast_For_Range_Stmt:
        ast_print(v.start, "start", depth)
        ast_print(v.end,  "end", depth)
        ast_print(v.body, "body", depth)
    }
}