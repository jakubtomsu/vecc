// token stream -> AST and most entities
package vecc

import "core:fmt"
import "core:os"
import "core:strings"
import "core:strconv"
import "core:reflect"

g_entity_order_counter: int
g_ast_order_counter: int

Parser :: struct {
    filename:               string,
    tokenizer:              Tokenizer,
    prev_token:             Token,
    curr_token:             Token,
    curr_scope:             ^Scope,
    curr_entity:            ^Entity,
    file_scope:             ^Scope,
}

Ast :: struct {
    variant:        Ast_Variant,
    type:           ^Type,
    value:          Value, // compile time known only
    order_index:    int, // local scope
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

    Ast_Proc_Decl,
    Ast_Value_Decl,
    Ast_Struct_Decl,
    Ast_Basic_Literal,
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

Ast_Value_Decl :: struct {
    private:    bool,
    vector:     bool,
    mut:        Value_Mutablity,
    scope:      ^Scope,
    entity:     ^Entity,
    name:       ^Ast,
    type:       ^Ast,
    value:      ^Ast,
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
    case Ast_Value_Decl:    fmt.printf(" : {} vector={} private={}", v.mut, v.vector, v.private)
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


parser_error :: proc(p: ^Parser, pos: Pos, format: string, args: ..any, loc := #caller_location) -> ! {
	fmt.eprintf("[{}] %s(%d:%d) ", loc, p.filename, pos.line, pos.column)
	fmt.eprintf(format, ..args)
	fmt.eprintln()
	os.exit(1)
}

// Grammar

next :: proc(p: ^Parser) -> Token {
    token, err := get_token(&p.tokenizer)
    if err != nil && token.kind != .EOF {
        parser_error(p, token.pos, "Found invalid token: {}", err)
    }

    // Print every token. Very useful for parser debugging
    fmt.println(token)

    p.prev_token, p.curr_token = p.curr_token, token
    return p.prev_token
}

expect :: proc(p: ^Parser, kind: Token_Kind, loc := #caller_location) -> Token {
    token := next(p)
    if token.kind != kind {
        parser_error(p, token.pos, "Expected {}, got {} ({})", kind, token.kind, token.text, loc = loc)
    }
    return token
}

allow :: proc(p: ^Parser, kind: Token_Kind) -> bool {
    if p.curr_token.kind == kind {
        next(p)
        return true
    }
    return false
}

peek :: proc(p: ^Parser) -> Token_Kind {
    return p.curr_token.kind
}

parse_ident :: proc(p: ^Parser, token: Token) -> ^Ast {
    result := create_ast()
    result.variant = Ast_Ident{
        token = token,
    }
    // if p.curr_entity != nil {
    //     p.curr_entity.depends[token.text] = {}
    // }
    return result
}

parse_basic_literal :: proc(p: ^Parser, token: Token) -> ^Ast {
    result := create_ast()
    result.variant = Ast_Basic_Literal{
        token = token,
    }
    return result
}

parse_call_args :: proc(p: ^Parser, args: ^[dynamic]^Ast) {
    for peek(p) != .Close_Paren {
        arg := parse_expr(p)
        append(args, arg)
        if !allow(p, .Comma) {
            break
        }
        allow(p, .Semicolon)
    }
}

parse_call_expr :: proc(p: ^Parser, procedure: ^Ast) -> ^Ast {
    expect(p, .Open_Paren)
    allow(p, .Semicolon)

    result := create_ast()

    call_expr: Ast_Call_Expr
    call_expr.procedure = procedure

    args: [dynamic]^Ast
    parse_call_args(p, &args)
    call_expr.args = args[:]

    expect(p, .Close_Paren)

    result.variant = call_expr
    return result
}

parse_self_call_expr :: proc(p: ^Parser, self_ast: ^Ast) -> ^Ast {
    result := create_ast()

    expect(p, .Colon)

    call_expr: Ast_Call_Expr
    call_expr.procedure = parse_ident(p, expect(p, .Ident))

    args: [dynamic]^Ast

    append(&args, self_ast)

    if allow(p, .Open_Paren) {
        parse_call_args(p, &args)
        expect(p, .Close_Paren)
    }

    call_expr.args = args[:]

    result.variant = call_expr

    return result
}

parse_cast_expr :: proc(p: ^Parser, op: Token) -> ^Ast {
    ast := create_ast()
    expect(p, .Open_Paren)
    type := parse_type(p)
    expect(p, .Comma)
    value := parse_expr(p)
    expect(p, .Close_Paren)

    ast.variant = Ast_Cast_Expr{
        op = op,
        type = type,
        value = value,
    }
    return ast
}

parse_selector_expr :: proc(p: ^Parser, left: ^Ast) -> ^Ast {
    expect(p, .Period)

    right := parse_ident(p, expect(p, .Ident))

    ast := create_ast()
    ast.variant = Ast_Selector_Expr{
        left    = left,
        right   = right,
    }

    return ast
}

parse_index_expr :: proc(p: ^Parser, left: ^Ast) -> ^Ast {
    expect(p, .Open_Bracket)

    index := parse_expr(p)

    ast := create_ast()
    ast.variant = Ast_Index_Expr{
        left    = left,
        index   = index,
    }

    expect(p, .Close_Bracket)

    return ast
}

parse_address_expr :: proc(p: ^Parser) -> ^Ast {
    ast := create_ast()
    ast.variant = Ast_Address_Expr{
        expr = parse_factor(p),
    }
    return ast
}

parse_deref_expr :: proc(p: ^Parser) -> ^Ast {
    ast := create_ast()
    ast.variant = Ast_Deref_Expr{
        expr = parse_factor(p),
    }
    return ast
}

parse_factor :: proc(p: ^Parser, loc := #caller_location) -> ^Ast {
    tok := next(p)
    #partial switch tok.kind {
    case .Ident:
        ident := parse_ident(p, tok)

        curr_ast := ident

        // NOTE: the tree here is inverted, index/selector exprs on the right side are parents.

        loop: for {
            #partial switch peek(p) {
            case .Open_Paren:
                curr_ast = parse_call_expr(p, curr_ast)
            case .Colon:
                curr_ast = parse_self_call_expr(p, curr_ast)
            case .Period:
                curr_ast = parse_selector_expr(p, curr_ast)
            case .Open_Bracket:
                curr_ast = parse_index_expr(p, curr_ast)
            case:
                break loop
            }
        }
        return curr_ast

    case .Integer, .Float, .String, .False, .True:
        return parse_basic_literal(p, tok)

    case .Conv, .Reinterpret:
        return parse_cast_expr(p, tok)

    case .Bit_And:
        return parse_address_expr(p)

    case .Mul:
        return parse_deref_expr(p)

    case .Open_Paren:
        result := parse_expr(p)
        expect(p, .Close_Paren)
        return result
    }
    parser_error(p, tok.pos, "Invalid factor, got {}", tok.kind, loc = loc)
}

parse_unary_expr :: proc(p: ^Parser) -> ^Ast {
    #partial switch peek(p) {
    case .Sub, .Not:
        ast := create_ast()
        ast.variant = Ast_Unary_Expr{
            op = next(p),
            expr = parse_factor(p),
        }
        return ast

    case:
        return parse_factor(p)
    }
}

parse_expr :: proc(p: ^Parser) -> ^Ast {
    left := parse_unary_expr(p)

    op_tok := p.curr_token
    #partial switch op_tok.kind {
    case
        .Equal,
        .Less_Than,
        .Less_Than_Equal,
        .Greater_Than,
        .Greater_Than_Equal,
        .Not,
        .Not_Equal,
        .Add,
        .Sub,
        .Mul,
        .Div,
        .Mod,
        .Bit_And,
        .Bit_Or,
        .Bit_Xor,
        .Bit_Not,
        .Bit_Shift_Left,
        .Bit_Shift_Right:
        // Ok

    case
        .Assign_Add,
        .Assign_Sub,
        .Assign_Mul,
        .Assign_Div,
        .Assign_Mod,
        .Assign_Bit_And,
        .Assign_Bit_Or,
        .Assign_Bit_Xor,
        .Assign_Bit_Shift_Left,
        .Assign_Bit_Shift_Right:
        parser_error(p, op_tok, "Invalid operator {}", op_tok.kind)

    case:
        return left
    }
    next(p)

    right := parse_unary_expr(p)

    result := create_ast()
    result.variant = Ast_Binary_Expr{
        left    = left,
        right   = right,
        op      = op_tok,
    }

    return result
}

parse_stmt :: proc(p: ^Parser) -> ^Ast {
    result: ^Ast

    #partial switch peek(p) {
    case .Return:
        result = parse_return_stmt(p)

    case .If:
        result = parse_if_stmt(p)

    case .For:
        result = parse_for_stmt(p)

    case .Range:
        result = parse_range_stmt(p)

    case .Break:
        result = create_ast()
        result.variant = Ast_Break_Stmt{
            token = expect(p, .Break),
        }

    case .Continue:
        result = create_ast()
        result.variant = Ast_Continue_Stmt{
            token = expect(p, .Continue),
        }

    case .Open_Brace:
        result = parse_block(p)

    case .Mut, .Immut, .Const:
        mut, mut_ok := value_mutability_from_token(next(p).kind)
        if !mut_ok {
            parser_error(p, p.curr_token, "Invalid variable declaration, expected mutability keyword")
        }
        result = parse_value_decl(p, mut = mut)

    case .Proc:
        result = parse_proc_decl(p)

    case .Struct:
        result = parse_struct_decl(p)

    case .Semicolon:

    case:
        result = parse_factor(p)

        #partial switch peek(p) {
        case
            .Assign,
            .Assign_Add,
            .Assign_Sub,
            .Assign_Mul,
            .Assign_Div,
            .Assign_Mod,
            .Assign_Bit_And,
            .Assign_Bit_Or,
            .Assign_Bit_Xor,
            .Assign_Bit_Shift_Left,
            .Assign_Bit_Shift_Right:

            assign_stmt: Ast_Assign_Stmt
            assign_stmt.op = next(p)

            assign_stmt.left = result
            assign_stmt.right = parse_expr(p)

            result = create_ast()
            result.variant = assign_stmt
        }
    }

    assert(result != nil)

    return result
}

parse_return_stmt :: proc(p: ^Parser) -> ^Ast {
    expect(p, .Return)

    ast := create_ast()
    return_stmt: Ast_Return_Stmt
    return_stmt.value = parse_expr(p)
    ast.variant = return_stmt
    return ast
}

parse_if_stmt :: proc(p: ^Parser) -> ^Ast {
    expect(p, .If)

    ast := create_ast()
    if_stmt: Ast_If_Stmt
    if_stmt.cond = parse_expr(p)
    if_stmt.if_body = parse_block(p)
    if allow(p, .Else) {
        if_stmt.else_body = parse_stmt(p)
    }
    ast.variant = if_stmt
    return ast
}

parse_for_stmt :: proc(p: ^Parser) -> ^Ast {
    expect(p, .For)

    ast := create_ast()
    for_stmt: Ast_For_Stmt

    parse_begin_scope(p)
    for_stmt.scope = p.curr_scope

    for_stmt.init = parse_value_decl(p, .Mutable)

    expect(p, .Semicolon)
    for_stmt.cond = parse_expr(p)
    expect(p, .Semicolon)
    for_stmt.post = parse_stmt(p)
    for_stmt.body = parse_block(p)

    parse_end_curr_scope(p)

    ast.variant = for_stmt
    return ast
}

parse_range_stmt :: proc(p: ^Parser) -> ^Ast {
    expect(p, .Range)

    ast := create_ast()
    for_stmt: Ast_For_Range_Stmt

    parse_begin_scope(p)

    ident_tok := expect(p, .Ident)
    for_stmt.ident = parse_ident(p, ident_tok)

    type := create_ast()
    type.variant = Ast_Ident{token = {text = "i64"}}
    register_value_entity(p,
        mut = .Mutable,
        vector = false,
        private = false,
        ident_tok = ident_tok,
        name = for_stmt.ident,
        type = type,
        value = nil,
    )

    expect(p, .Colon)

    for_stmt.start = parse_expr(p)
    for_stmt.range = p.curr_token
    #partial switch peek(p) {
    case .Range_Incl:
        expect(p, .Range_Incl)
    case .Range_Excl:
        expect(p, .Range_Excl)
    case:
        parser_error(p, p.curr_token, "Invalid range")
    }
    for_stmt.end = parse_expr(p)

    for_stmt.body = parse_block(p, ignore_scope = true)

    ast.variant = for_stmt

    parse_end_curr_scope(p)
    return ast
}

parse_stmt_list :: proc(p: ^Parser, end: Token_Kind) -> []^Ast {
    result: [dynamic]^Ast
    for peek(p) != end {
        stmt := parse_stmt(p)
        append(&result, stmt)
        if !allow(p, .Semicolon) {
            break
        }
    }
    return result[:]
}

parse_begin_scope :: proc(p: ^Parser) {
    scope := new(Scope)
    scope.parent = p.curr_scope
    scope.depth = p.curr_scope.depth + 1

    append(&p.curr_scope.children, scope)

    p.curr_scope = scope
}

parse_end_curr_scope :: proc(p: ^Parser) {
    p.curr_scope = p.curr_scope.parent
}

parse_block :: proc(p: ^Parser, ignore_scope := false) -> ^Ast {
    result := create_ast()
    this_scope := p.curr_scope
    block_stmt: Ast_Block_Stmt

    if !ignore_scope {
        parse_begin_scope(p)
    }

    expect(p, .Open_Brace)

    block_stmt.scope = p.curr_scope

    block_stmt.statements = parse_stmt_list(p, .Close_Brace)
    expect(p, .Close_Brace)

    result.variant = block_stmt

    if !ignore_scope {
        parse_end_curr_scope(p)
    }

    return result
}

register_value_entity :: proc(
    p:          ^Parser,
    mut:        Value_Mutablity,
    vector:     bool,
    private:    bool,
    ident_tok:  Token,
    name:       ^Ast,
    type:       ^Ast,
    value:      ^Ast,
) -> ^Ast {
    ast := create_ast()
    value_decl: Ast_Value_Decl

    value_decl.mut = mut
    value_decl.private = private
    value_decl.vector = vector
    value_decl.scope = p.curr_scope
    value_decl.name = name
    value_decl.type = type
    value_decl.value = value

    name := ident_tok.text

    switch mut {
    case .Constant:
        ast.order_index = -1
    case .Invalid, .Mutable, .Immutable:
    }

    entity := register_entity(p,
        scope   = value_decl.scope,
        name    = name,
        ast     = ast,
        variant = Entity_Variable{},
    )

    ast.variant = value_decl
    return ast
}

parse_type :: proc(p: ^Parser) -> (result: ^Ast) {
    #partial switch peek(p) {
    case .Open_Bracket:
        expect(p, .Open_Bracket)

        #partial switch peek(p) {
        case .Bit_Xor:
            tok := expect(p, .Bit_Xor)
            ast := create_ast()
            expect(p, .Close_Bracket)
            ast.variant = Ast_Multi_Pointer_Type{
                token = tok,
                type = parse_type(p),
            }
            result = ast

        case .Close_Bracket:
            unimplemented("slice")

        case:
            ast := create_ast()
            l := parse_expr(p)
            expect(p, .Close_Bracket)
            ast.variant = Ast_Array_Type{
                kind = .Fixed_Array,
                len = l,
                type = parse_type(p),
            }
            result = ast
        }


    case .Bit_Xor:
        tok := expect(p, .Bit_Xor)
        ast := create_ast()
        ast.variant = Ast_Pointer_Type{
            token = tok,
            type = parse_type(p),
        }
        result = ast

    case .Vector:
        expect(p, .Vector)

        ast := create_ast()
        vector := Ast_Array_Type{
            kind = .Vector,
        }
        expect(p, .Open_Paren)
        vector.len = parse_factor(p)
        expect(p, .Comma)
        vector.type = parse_type(p)
        expect(p, .Close_Paren)

        ast.variant = vector
        result = ast

    case:
        result = parse_ident(p, expect(p, .Ident))
    }

    return result
}

parse_value_decl :: proc(p: ^Parser, mut: Value_Mutablity) -> ^Ast {
    private := allow(p, .Private)
    vector := allow(p, .Vector)

    ident_tok := next(p)
    name := parse_ident(p, ident_tok)
    type := parse_type(p)
    value: ^Ast
    if allow(p, .Assign) {
        value = parse_expr(p)
    }

    return register_value_entity(p,
        mut         = mut,
        vector      = vector,
        private     = private,
        ident_tok   = ident_tok,
        name        = name,
        type        = type,
        value       = value,
    )
}

parse_field :: proc(p: ^Parser) -> ^Ast {
    ast := create_ast()
    ident_tok := expect(p, .Ident)
    name := parse_ident(p, ident_tok)
    type := parse_type(p)
    value: ^Ast
    if allow(p, .Assign) {
        value = parse_expr(p)
    }

    ast.variant = Ast_Field {
        name = name,
        type = type,
        value = value,
    }

    register_entity(p,
        scope = p.curr_scope,
        name = ident_tok.text,
        ast = ast,
        variant = Entity_Struct_Field{},
    )

    return ast
}

parse_field_list :: proc(p: ^Parser, end: Token_Kind) -> [dynamic]^Ast {
    result: [dynamic]^Ast
    for peek(p) != end {
        field := parse_field(p)
        append(&result, field)
        if !allow(p, .Semicolon) {
            break
        }
    }
    return result
}

parse_proc_type :: proc(p: ^Parser) -> ^Ast {
    ast := create_ast()
    proc_type: Ast_Proc_Type
    expect(p, .Open_Paren)
    params: [dynamic]^Ast
    for peek(p) != .Close_Paren {
        mut := value_mutability_from_token(next(p).kind) or_break
        field := parse_value_decl(p, mut = mut)
        append(&params, field)
        if !allow(p, .Semicolon) {
            break
        }
    }
    proc_type.params = params[:]
    expect(p, .Close_Paren)

    if peek(p) != .Open_Brace {
        proc_type.result = parse_type(p)
    }

    ast.variant = proc_type
    return ast
}

parse_proc_decl :: proc(p: ^Parser) -> ^Ast {
    ast := create_ast()
    proc_decl: Ast_Proc_Decl

    expect(p, .Proc)

    proc_decl.export = allow(p, .Export)
    proc_decl.private = allow(p, .Private)

    if proc_decl.export && proc_decl.private {
        parser_error(p, p.prev_token, "Export and Private qualifiers cannot be used both at once")
    }

    ident_tok := expect(p, .Ident)

    fmt.println("PARSE PROC DECL", ident_tok)


    name := ident_tok.text

    ast.order_index = -1

    entity := register_entity(p,
        scope = p.curr_scope,
        name = name,
        ast = ast,
        variant = Entity_Proc{},
    )

    parse_begin_scope(p)

    proc_decl.entity = entity
    proc_decl.scope = p.curr_scope
    proc_decl.type = parse_proc_type(p)
    proc_decl.body = parse_block(p, ignore_scope = true)

    parse_end_curr_scope(p)

    ast.variant = proc_decl
    return ast
}

parse_struct_decl :: proc(p: ^Parser) -> ^Ast {
    expect(p, .Struct)

    ast := create_ast()

    ident_tok := expect(p, .Ident)
    name := ident_tok.text

    entity := register_entity(p,
        scope = p.curr_scope,
        name = name,
        ast = ast,
        variant = Entity_Struct{},
    )

    ast.variant = Ast_Struct_Decl{
        entity = entity,
        type = parse_struct_type(p),
    }

    return ast
}


register_entity :: proc(
    p: ^Parser,
    scope: ^Scope,
    name: string,
    ast: ^Ast,
    variant: Entity_Variant,
) -> ^Entity {
    if strings.has_prefix(name, "vecc_") {
        parser_error(p, p.curr_token, "'vecc_' identifier prefix is reserved for the compiler.")
    }

    if _, _, ok := find_entity(scope, name); ok {
        parser_error(p, p.curr_token, "Duplicate entity name: {}", name)
    }

    entity := create_entity(
        scope = scope,
        name = name,
        ast = ast,
        variant = variant
    )

    return entity
}

create_entity :: proc(scope: ^Scope, name: string, ast: ^Ast, variant: Entity_Variant) -> ^Entity {
    entity := new(Entity)

    fmt.println("Create entity", name)

    entity.ast = ast
    entity.name = name
    entity.variant = variant

    entity.order_index = g_entity_order_counter
    g_entity_order_counter += 1

    scope.entities[name] = entity

    return entity
}

parse_struct_type :: proc(p: ^Parser) -> ^Ast {
    ast := create_ast()
    expect(p, .Open_Brace)
    parse_begin_scope(p)
    ast.variant = Ast_Struct_Type{
        fields = parse_field_list(p, .Close_Brace)
    }
    parse_end_curr_scope(p)
    expect(p, .Close_Brace)
    return ast
}

create_ast :: proc() -> ^Ast {
    ast := new(Ast)
    ast.order_index = g_ast_order_counter
    g_ast_order_counter += 1
    return ast
}

parse_file :: proc(p: ^Parser) {
    p.file_scope = new(Scope)

    p.curr_scope = p.file_scope
    defer assert(p.curr_scope == p.file_scope)

    for peek(p) != .EOF {
        curr := p.curr_token

        stmt := parse_stmt(p)

        #partial switch _ in stmt.variant {
        case Ast_Value_Decl, Ast_Proc_Decl, Ast_Struct_Decl:
        // case nil:
        //     break
        case:
            parser_error(p, curr, "Invalid top level declaration, expected value/procedure/struct, got: {}", stmt.variant)
        }


        allow(p, .Semicolon)
    }
}