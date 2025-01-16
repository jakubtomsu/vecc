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
    scope:  ^Scope,
    entity: ^Entity,
    name:   ^Ast,
    type:   ^Ast,
    value:  ^Ast,
}

Ast_Proc_Decl :: struct {
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
    cond: ^Ast,
    body: ^Ast,
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
        fmt.print("  ")
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
        ast_print(v.body, "body", depth)

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


parser_error :: proc(c: ^Parser, pos: Pos, format: string, args: ..any, loc := #caller_location) -> ! {
	fmt.eprintf("[{}] %s(%d:%d) ", loc, c.filename, pos.line, pos.column)
	fmt.eprintf(format, ..args)
	fmt.eprintln()
	os.exit(1)
}

// Grammar

next :: proc(c: ^Parser) -> Token {
    token, err := get_token(&c.tokenizer)
    if err != nil && token.kind != .EOF {
        parser_error(c, token.pos, "Found invalid token: {}", err)
    }
    fmt.println(token)
    c.prev_token, c.curr_token = c.curr_token, token
    return c.prev_token
}

expect :: proc(c: ^Parser, kind: Token_Kind, loc := #caller_location) -> Token {
    token := next(c)
    if token.kind != kind {
        parser_error(c, token.pos, "Expected {}, got {} ({})", kind, token.kind, token.text, loc = loc)
    }
    return token
}

allow :: proc(c: ^Parser, kind: Token_Kind) -> bool {
    if c.curr_token.kind == kind {
        next(c)
        return true
    }
    return false
}

peek :: proc(c: ^Parser) -> Token_Kind {
    return c.curr_token.kind
}

parse_ident :: proc(c: ^Parser, token: Token) -> ^Ast {
    result := new(Ast)
    result.variant = Ast_Ident{
        token = token,
    }
    return result
}

parse_basic_literal :: proc(c: ^Parser, token: Token) -> ^Ast {
    result := new(Ast)
    result.variant = Ast_Basic_Literal{
        token = token,
    }
    return result
}

parse_factor :: proc(c: ^Parser, loc := #caller_location) -> ^Ast {
    tok := next(c)
    #partial switch tok.kind {
    case .Ident:
        #partial switch peek(c) {
        case .Open_Paren:
            return parse_call_expr(c, tok)
        case .Colon:
            return parse_self_call_expr(c, tok)
        case:
            return parse_ident(c, tok)
        }
        
    case .Conv, .Reinterpret:
        return parse_cast_expr(c, tok)
        
    case .Integer, .Float, .String:
        return parse_basic_literal(c, tok)

    case .Open_Paren:
        result := parse_expr(c)
        expect(c, .Close_Paren)
        return result
    }
    parser_error(c, tok.pos, "Invalid factor, got {}", tok.kind, loc = loc)
}

parse_call_args :: proc(c: ^Parser, args: ^[dynamic]^Ast) {
    for peek(c) != .Close_Paren {
        arg := parse_expr(c)
        append(args, arg)
        if !allow(c, .Comma) {
            break
        }
        allow(c, .Semicolon)
    }
}

parse_call_expr :: proc(c: ^Parser, ident_tok: Token) -> ^Ast {
    expect(c, .Open_Paren)
    allow(c, .Semicolon)

    result := new(Ast)
    
    call_expr: Ast_Call_Expr
    call_expr.procedure = parse_ident(c, ident_tok)
    
    args: [dynamic]^Ast
    parse_call_args(c, &args)
    call_expr.args = args[:]
    
    expect(c, .Close_Paren)
    
    result.variant = call_expr
    return result
}

parse_self_call_expr :: proc(c: ^Parser, ident_tok: Token) -> ^Ast {
    result := new(Ast)
    
    expect(c, .Colon)
    
    call_expr: Ast_Call_Expr
    call_expr.procedure = parse_ident(c, expect(c, .Ident))

    args: [dynamic]^Ast

    self := new(Ast)
    self.variant = Ast_Ident{
        token = ident_tok,
    }
    append(&args, self)

    expect(c, .Open_Paren)
    parse_call_args(c, &args)
    expect(c, .Close_Paren)
    
    call_expr.args = args[:]
    
    result.variant = call_expr
    
    return result
}

parse_cast_expr :: proc(c: ^Parser, op: Token) -> ^Ast {
    ast := new(Ast)
    expect(c, .Open_Paren)
    type := parse_type(c)
    expect(c, .Comma)
    value := parse_expr(c)
    expect(c, .Close_Paren)
    
    ast.variant = Ast_Cast_Expr{
        op = op,
        type = type,
        value = value,
    }
    return ast
}

parse_expr :: proc(c: ^Parser) -> ^Ast {
    left := parse_factor(c)

    op_tok := c.curr_token
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
        parser_error(c, op_tok, "Invalid operator {}", op_tok.kind)

    case:
        return left
    }
    next(c)

    right := parse_factor(c)
    
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

parse_stmt :: proc(c: ^Parser) -> ^Ast {
    result := new(Ast)
    first := next(c)
    #partial switch first.kind {
    case .Ident:
        #partial switch peek(c) {
        case .Ident:
            result = parse_value_decl(c, first)

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
            assign_stmt.op = next(c)

            assign_stmt.left = parse_ident(c, first)
            assign_stmt.right = parse_expr(c)
            
            result.variant = assign_stmt

        case .Open_Paren:
            result = parse_call_expr(c, first)
            
        case .Colon:
            result = parse_self_call_expr(c, first)
        }

    case .Return:
        return_stmt: Ast_Return_Stmt
        return_stmt.expr = parse_expr(c)
        result.variant = return_stmt

    case .If:
        if_stmt: Ast_If_Stmt
        if_stmt.cond = parse_expr(c)
        if_stmt.body = parse_block(c)
        result.variant = if_stmt
    
    case .For:
        for_stmt: Ast_For_Stmt

        for_stmt.init = parse_value_decl(c, expect(c, .Ident))
        
        expect(c, .Semicolon)
        for_stmt.cond = parse_expr(c)
        expect(c, .Semicolon)
        for_stmt.post = parse_stmt(c)
        for_stmt.body = parse_block(c)
    
        result.variant = for_stmt
        
    case .Range:
        for_stmt: Ast_For_Range_Stmt

        ident_tok := expect(c, .Ident)
        for_stmt.ident = parse_ident(c, ident_tok)
        
        type := new(Ast)
        type.variant = Ast_Ident{token = {text = "i64"}}
        register_value_entity(c, ident_tok, for_stmt.ident, type, nil)
        
        expect(c, .Colon)
        
        for_stmt.start = parse_expr(c)
        for_stmt.range = c.curr_token
        #partial switch peek(c) {
        case .Range_Incl:
            expect(c, .Range_Incl)
        case .Range_Excl:
            expect(c, .Range_Excl)
        case:
            parser_error(c, c.curr_token, "Invalid range")
        }
        for_stmt.end = parse_expr(c)

        for_stmt.body = parse_block(c)
    
        result.variant = for_stmt
    
    case .Open_Brace:
        result = parse_block(c, ignore_begin = true)
        
    case .Lanes:
        result.variant = Ast_Lanes_Stmt{
            num = parse_basic_literal(c, expect(c, .Integer)),
            body = parse_block(c),
        }
    }
    
    return result
}

parse_stmt_list :: proc(c: ^Parser, end: Token_Kind) -> []^Ast {
    result: [dynamic]^Ast
    for peek(c) != end {
        stmt := parse_stmt(c)
        append(&result, stmt)
        if !allow(c, .Semicolon) {
            break
        }
    }
    return result[:]
}

parse_block :: proc(c: ^Parser, ignore_begin := false, loc := #caller_location) -> ^Ast {
    result := new(Ast)
    this_scope := c.curr_scope
    block_stmt: Ast_Block_Stmt
    block_stmt.scope = new(Scope)
    block_stmt.scope.parent = this_scope
    c.curr_scope = block_stmt.scope

    if !ignore_begin {
        expect(c, .Open_Brace)
    }
    
    block_stmt.statements = parse_stmt_list(c, .Close_Brace)
    expect(c, .Close_Brace, loc = loc)
    
    result.variant = block_stmt
    
    c.curr_scope = this_scope
    
    return result
}

parse_type :: proc(c: ^Parser) -> ^Ast {
    return parse_ident(c, expect(c, .Ident))
}

register_value_entity :: proc(c: ^Parser, ident_tok: Token, name: ^Ast, type: ^Ast, value: ^Ast) -> ^Ast {
    result := new(Ast)
    value_decl: Ast_Value_Decl
    value_decl.scope = c.curr_scope

    value_decl.name = name
    value_decl.type = type
    value_decl.value = value
    
    name := ident_tok.text
    
    if strings.has_prefix(name, "vecc_") {
        parser_error(c, ident_tok, "'vecc_' prefix is reserved for the compiler.")
    }

    if _, ok := find_entity(c.curr_scope, name).?; ok {
        parser_error(c, ident_tok, "Duplicate entity name: {}", name)
    }
    
    entity := new(Entity)
    entity.ast = result
    entity.order_index = c.entity_order_counter
    c.entity_order_counter += 1
    entity.variant = Entity_Variable{}
    c.curr_scope.entities[name] = entity

    
    result.variant = value_decl
    return result
}

parse_value_decl :: proc(c: ^Parser, ident_tok: Token) -> ^Ast {
    name := parse_ident(c, ident_tok)
    type := parse_type(c)
    value: ^Ast
    if allow(c, .Assign) {
        value = parse_expr(c)
    }
    
    return register_value_entity(c,
        ident_tok   = ident_tok,
        name        = name,
        type        = type,
        value       = value,
    )
}

parse_field :: proc(c: ^Parser) -> ^Ast {
    ast := new(Ast)
    name := parse_ident(c, expect(c, .Ident))
    type := parse_type(c)
    value: ^Ast
    if allow(c, .Assign) {
        value = parse_expr(c)
    }
    
    ast.variant = Ast_Field {
        name = name,
        type = type,
        value = value,
    }
    return ast
}

parse_field_list :: proc(c: ^Parser, end: Token_Kind) -> []^Ast {
    result: [dynamic]^Ast
    for peek(c) != end {
        field := parse_field(c)
        append(&result, field)
        if !allow(c, .Comma) {
            break
        }
    }
    return result[:]
}

parse_proc_type :: proc(c: ^Parser) -> ^Ast {
    ast := new(Ast)
    proc_type: Ast_Proc_Type
    expect(c, .Proc)
    expect(c, .Open_Paren)
    proc_type.params = parse_field_list(c, .Close_Paren)
    expect(c, .Close_Paren)
    
    if peek(c) == .Ident {
        proc_type.result = parse_type(c)
    }
    
    ast.variant = proc_type
    return ast
}

parse_proc_decl :: proc(c: ^Parser, ident_tok: Token) -> ^Ast {
    result := new(Ast)
    proc_decl: Ast_Proc_Decl
    
    name := ident_tok.text
    if _, ok := find_entity(c.curr_scope, name).?; ok {
        parser_error(c, ident_tok, "Duplicate entity name: {}", name)
    }
    
    entity := new(Entity)
    entity.ast = result
    entity.order_index = c.entity_order_counter
    c.entity_order_counter += 1
    entity.variant = Entity_Proc{}
    
    c.curr_scope.entities[name] = entity
    proc_decl.entity = entity
    proc_decl.scope = c.curr_scope

    proc_decl.type = parse_proc_type(c)
    
    proc_decl.body = parse_block(c)
    allow(c, .Semicolon)
    
    result.variant = proc_decl
    return result
}

parse_file :: proc(c: ^Parser) {
    global_scope := new(Scope)
    c.curr_scope = global_scope
    defer assert(c.curr_scope == global_scope)

    for peek(c) != .EOF {
        ast: ^Ast
        
        ident_tok := expect(c, .Ident)
        #partial switch peek(c) {
        case .Proc:
            ast = parse_proc_decl(c, ident_tok)

        case .Ident:
            ast = parse_value_decl(c, ident_tok)

        case:
            parser_error(c, c.curr_token, "Invalid declaration token: {}", c.curr_token.kind)
        }
        
        allow(c, .Semicolon)
    }
}