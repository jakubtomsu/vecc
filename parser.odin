package vecc

import "core:fmt"
import "core:os"
import "core:strings"
import "core:reflect"

Parser :: struct {
    filename:               string,
    tokenizer:              Tokenizer,
    prev_token:             Token,
    curr_token:             Token,
    curr_scope:             ^Scope,
    entity_order_counter:   int,
}

Ast :: struct {
    variant: Ast_Variant,
}

Ast_Variant :: union {
    Ast_Ident,

    Ast_Proc_Decl,
    Ast_Value_Decl,
    Ast_Basic_Literal,
    Ast_Field,
    
    Ast_Proc_Type,

    Ast_Block_Stmt,
    Ast_Lanes_Stmt,
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
}

Ast_Ident :: struct {
    token: Token,
}

Ast_Basic_Literal :: struct {
    token: Token,
}

Ast_Value_Decl :: struct {
    export:     bool,
    private:    bool,
    scope:      ^Scope,
    entity:     ^Entity,
    name:       ^Ast,
    type:       ^Ast,
    value:      ^Ast,
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

Ast_Block_Stmt :: struct {
    scope:      ^Scope,
    statements: []^Ast,
}

Ast_Lanes_Stmt :: struct {
    num:    ^Ast,
    body:   ^Ast,
}

Ast_If_Stmt :: struct {
    cond:       ^Ast,
    if_body:    ^Ast,
    else_body:  ^Ast,
}

Ast_For_Stmt :: struct {
    init: ^Ast,
    cond: ^Ast,
    post: ^Ast,
    body: ^Ast,
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
    expr:   ^Ast,
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
    args:       []^Ast,
}

Ast_Cast_Expr :: struct {
    op:     Token, // conv or reinterpret
    type:   ^Ast,
    value:  ^Ast,
}

ast_print :: proc(ast: ^Ast, name: string, depth: int) {
    depth := depth
    for i in 0..<depth {
        fmt.print("|   ")
    }

    
    if name != "" {
        if ast == nil {
            fmt.print(name, ":", "nil node")
            return
        }
        if ast.variant == nil {
            fmt.print(name, ":", "nil variant")
        } else {
            fmt.print(name, ":", reflect.union_variant_typeid(ast.variant))
        }
    } else {
        if ast == nil {
            fmt.print("nil node")
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
    case Ast_Value_Decl:    fmt.printf(" : export={} private={}", v.export, v.private)
    case Ast_Proc_Decl:     fmt.printf(" : export={} private={}", v.export, v.private)
    }
    
    fmt.println()

    depth += 1
    #partial switch v in ast.variant {
    case Ast_Block_Stmt:
        for it in v.statements {
            ast_print(it, "stmt", depth)
        }
    
    case Ast_Lanes_Stmt:
        ast_print(v.num, "num", depth)
        ast_print(v.body, "body", depth)
    
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
    result := new(Ast)
    result.variant = Ast_Ident{
        token = token,
    }
    return result
}

parse_basic_literal :: proc(p: ^Parser, token: Token) -> ^Ast {
    result := new(Ast)
    result.variant = Ast_Basic_Literal{
        token = token,
    }
    return result
}

parse_factor :: proc(p: ^Parser, loc := #caller_location) -> ^Ast {
    tok := next(p)
    #partial switch tok.kind {
    case .Ident:
        #partial switch peek(p) {
        case .Open_Paren:
            return parse_call_expr(p, tok)
        case .Colon:
            return parse_self_call_expr(p, tok)
        case:
            return parse_ident(p, tok)
        }
        
    case .Conv, .Reinterpret:
        return parse_cast_expr(p, tok)
        
    case .Integer, .Float, .String:
        return parse_basic_literal(p, tok)

    case .Open_Paren:
        result := parse_expr(p)
        expect(p, .Close_Paren)
        return result
    }
    parser_error(p, tok.pos, "Invalid factor, got {}", tok.kind, loc = loc)
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

parse_call_expr :: proc(p: ^Parser, ident_tok: Token) -> ^Ast {
    expect(p, .Open_Paren)
    allow(p, .Semicolon)

    result := new(Ast)
    
    call_expr: Ast_Call_Expr
    call_expr.procedure = parse_ident(p, ident_tok)
    
    args: [dynamic]^Ast
    parse_call_args(p, &args)
    call_expr.args = args[:]
    
    expect(p, .Close_Paren)
    
    result.variant = call_expr
    return result
}

parse_self_call_expr :: proc(p: ^Parser, ident_tok: Token) -> ^Ast {
    result := new(Ast)
    
    expect(p, .Colon)
    
    call_expr: Ast_Call_Expr
    call_expr.procedure = parse_ident(p, expect(p, .Ident))

    args: [dynamic]^Ast

    self := new(Ast)
    self.variant = Ast_Ident{
        token = ident_tok,
    }
    append(&args, self)

    if allow(p, .Open_Paren) {
        parse_call_args(p, &args)
        expect(p, .Close_Paren)
    }
    
    call_expr.args = args[:]
    
    result.variant = call_expr
    
    return result
}

parse_cast_expr :: proc(p: ^Parser, op: Token) -> ^Ast {
    ast := new(Ast)
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

parse_expr :: proc(p: ^Parser) -> ^Ast {
    left := parse_factor(p)

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

    right := parse_factor(p)
    
    result := new(Ast)
    result.variant = Ast_Binary_Expr{
        left    = left,
        right   = right,
        op      = op_tok,
    }
    
    return result
}

find_entity :: proc(scope: ^Scope, name: string) -> Maybe(^Entity) {
    for s := scope; s != nil; s = s.parent {
        ent := s.entities[name] or_continue
        return ent
    }
    return nil
}

parse_stmt :: proc(p: ^Parser) -> ^Ast {
    result := new(Ast)
    first := next(p)
    #partial switch first.kind {
    case .Ident:
        #partial switch peek(p) {
        case .Ident:
            result = parse_value_decl(p, first)

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

            assign_stmt.left = parse_ident(p, first)
            assign_stmt.right = parse_expr(p)
            
            result.variant = assign_stmt

        case .Open_Paren:
            result = parse_call_expr(p, first)
            
        case .Colon:
            result = parse_self_call_expr(p, first)
        }

    case .Return:
        return_stmt: Ast_Return_Stmt
        return_stmt.expr = parse_expr(p)
        result.variant = return_stmt

    case .If:
        if_stmt: Ast_If_Stmt
        if_stmt.cond = parse_expr(p)
        if_stmt.if_body = parse_block(p)
        if allow(p, .Else) {
            if_stmt.else_body = parse_stmt(p)
        }
        result.variant = if_stmt
    
    case .For:
        for_stmt: Ast_For_Stmt

        for_stmt.init = parse_value_decl(p, expect(p, .Ident))
        
        expect(p, .Semicolon)
        for_stmt.cond = parse_expr(p)
        expect(p, .Semicolon)
        for_stmt.post = parse_stmt(p)
        for_stmt.body = parse_block(p)
    
        result.variant = for_stmt
        
    case .Range:
        for_stmt: Ast_For_Range_Stmt

        ident_tok := expect(p, .Ident)
        for_stmt.ident = parse_ident(p, ident_tok)
        
        type := new(Ast)
        type.variant = Ast_Ident{token = {text = "i64"}}
        register_value_entity(p, ident_tok, for_stmt.ident, type, nil)
        
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

        for_stmt.body = parse_block(p)
    
        result.variant = for_stmt
    
    case .Break:
        result.variant = Ast_Break_Stmt{
            token = first,
        }
    
    case .Continue:
        result.variant = Ast_Continue_Stmt{
            token = first,
        }
    
    case .Open_Brace:
        result = parse_block(p, ignore_begin = true)
        
    case .Lanes:
        result.variant = Ast_Lanes_Stmt{
            num = parse_basic_literal(p, expect(p, .Integer)),
            body = parse_block(p),
        }
    }
    
    return result
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

parse_block :: proc(p: ^Parser, ignore_begin := false, loc := #caller_location) -> ^Ast {
    result := new(Ast)
    this_scope := p.curr_scope
    block_stmt: Ast_Block_Stmt
    block_stmt.scope = new(Scope)
    block_stmt.scope.parent = this_scope
    block_stmt.scope.depth = this_scope.depth + 1
    p.curr_scope = block_stmt.scope

    if !ignore_begin {
        expect(p, .Open_Brace)
    }
    
    block_stmt.statements = parse_stmt_list(p, .Close_Brace)
    expect(p, .Close_Brace, loc = loc)
    
    result.variant = block_stmt
    
    p.curr_scope = this_scope
    
    return result
}

parse_type :: proc(p: ^Parser) -> ^Ast {
    return parse_ident(p, expect(p, .Ident))
}

register_value_entity :: proc(p: ^Parser, ident_tok: Token, name: ^Ast, type: ^Ast, value: ^Ast) -> ^Ast {
    result := new(Ast)
    value_decl: Ast_Value_Decl
    
    value_decl.export = allow(p, .Export)
    value_decl.private = allow(p, .Private)
    
    if value_decl.export && value_decl.private {
        parser_error(p, p.prev_token, "Export and Private qualifiers cannot be used both at once")
    }
    
    value_decl.scope = p.curr_scope

    value_decl.name = name
    value_decl.type = type
    value_decl.value = value
    
    name := ident_tok.text
    
    if strings.has_prefix(name, "vecc_") {
        parser_error(p, ident_tok, "'vecc_' prefix is reserved for the compiler.")
    }

    if _, ok := find_entity(p.curr_scope, name).?; ok {
        parser_error(p, ident_tok, "Duplicate entity name: {}", name)
    }
    
    entity := new(Entity)
    entity.ast = result
    entity.order_index = p.entity_order_counter
    p.entity_order_counter += 1
    entity.variant = Entity_Variable{}
    p.curr_scope.entities[name] = entity

    
    result.variant = value_decl
    return result
}

parse_value_decl :: proc(p: ^Parser, ident_tok: Token) -> ^Ast {
    name := parse_ident(p, ident_tok)
    type := parse_type(p)
    value: ^Ast
    if allow(p, .Assign) {
        value = parse_expr(p)
    }
    
    return register_value_entity(p,
        ident_tok   = ident_tok,
        name        = name,
        type        = type,
        value       = value,
    )
}

parse_field :: proc(p: ^Parser) -> ^Ast {
    ast := new(Ast)
    name := parse_ident(p, expect(p, .Ident))
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
    return ast
}

parse_field_list :: proc(p: ^Parser, end: Token_Kind) -> []^Ast {
    result: [dynamic]^Ast
    for peek(p) != end {
        field := parse_field(p)
        append(&result, field)
        if !allow(p, .Comma) {
            break
        }
    }
    return result[:]
}

parse_proc_type :: proc(p: ^Parser) -> ^Ast {
    ast := new(Ast)
    proc_type: Ast_Proc_Type
    expect(p, .Proc)
    expect(p, .Open_Paren)
    proc_type.params = parse_field_list(p, .Close_Paren)
    expect(p, .Close_Paren)
    
    if peek(p) == .Ident {
        proc_type.result = parse_type(p)
    }
    
    ast.variant = proc_type
    return ast
}

parse_proc_decl :: proc(p: ^Parser, ident_tok: Token) -> ^Ast {
    result := new(Ast)
    proc_decl: Ast_Proc_Decl
    
    proc_decl.export = allow(p, .Export)
    proc_decl.private = allow(p, .Private)
    
    if proc_decl.export && proc_decl.private {
        parser_error(p, p.prev_token, "Export and Private qualifiers cannot be used both at once")
    }
    
    name := ident_tok.text
    if _, ok := find_entity(p.curr_scope, name).?; ok {
        parser_error(p, ident_tok, "Duplicate entity name: {}", name)
    }
    
    entity := new(Entity)
    entity.ast = result
    entity.order_index = p.entity_order_counter
    p.entity_order_counter += 1
    entity.variant = Entity_Proc{}
    
    p.curr_scope.entities[name] = entity
    proc_decl.entity = entity
    proc_decl.scope = p.curr_scope

    proc_decl.type = parse_proc_type(p)
    
    proc_decl.body = parse_block(p)
    allow(p, .Semicolon)
    
    result.variant = proc_decl
    return result
}

parse_file :: proc(p: ^Parser) {
    global_scope := new(Scope)
    p.curr_scope = global_scope
    defer assert(p.curr_scope == global_scope)

    for peek(p) != .EOF {
        ast: ^Ast
        
        ident_tok := expect(p, .Ident)
        #partial switch peek(p) {
        case .Proc, .Export:
            ast = parse_proc_decl(p, ident_tok)

        case .Ident:
            ast = parse_value_decl(p, ident_tok)

        case:
            parser_error(p, p.curr_token, "Invalid declaration token: {}", p.curr_token.kind)
        }
        
        allow(p, .Semicolon)
    }
}