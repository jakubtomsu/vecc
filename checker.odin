package vecc

import "core:fmt"
import "core:strings"
import "core:strconv"
import "core:slice"

Checker :: struct {
    filename:               string,
    curr_scope:             ^Scope,
    curr_lanes:             int,
    source:                 strings.Builder,
    indent:                 int,
}

Entity :: struct {
    ast:            ^Ast,
    order_index:    int,
    variant:        Entity_Variant,
}

Entity_Variant :: union {
    Entity_Proc,
    Entity_Variable,
}

Entity_Proc :: struct {
    
}

Entity_Variable :: struct {
    
}

Scope :: struct {
    ast:        ^Ast,
    parent:     ^Scope,
    entities:   map[string]^Entity,
}

Type :: struct {
    name:       string,
    size:       int,
    variant:    Type_Variant,
}

Type_Variant :: union {
    Type_Integer,
    Type_Struct,
}

Type_Integer :: struct {}
Type_Struct :: struct {
    
}

gen_indent :: proc(g: ^Checker) {
    ind := "\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t"
    fmt.sbprint(&g.source, ind[:clamp(g.indent, 0, len(ind) - 1)])
}

check_ident :: proc(c: ^Checker, ast: ^Ast) {
    ident := ast.variant.(Ast_Ident)
    name := ident.token.text
    if ent, ok := find_entity(c.curr_scope, name).?; ok {
        fmt.sbprint(&c.source, name)
    } else {
        fmt.sbprint(&c.source, name)
        // assert(false, "Ident not found")
    }
}

check_type :: proc(c: ^Checker, ast: ^Ast) {
    if ast == nil {
        fmt.sbprint(&c.source, "void")
    } else {
        ident := ast.variant.(Ast_Ident)
        fmt.sbprint(&c.source, ident.token.text)
    }
}

check_basic_literal :: proc(c: ^Checker, ast: ^Ast) {
    lit := ast.variant.(Ast_Basic_Literal)
    switch c.curr_lanes {
    case 1:
        fmt.sbprint(&c.source, lit.token.text)
    
    case 8:
        #partial switch lit.token.kind {
        case .Integer:
            if lit.token.text == "0" {
                fmt.sbprint(&c.source, "_mm256_setzero_si256()")
            } else {
                fmt.sbprintf(&c.source, "_mm256_set1_epi32({})", lit.token.text)
            }

        case .Float:
            if lit.token.text == "0" {
                fmt.sbprint(&c.source, "_mm256_setzero_ps()")
            } else {
                fmt.sbprintf(&c.source, "_mm256_set1_ps({})", lit.token.text)
            }
        }
    }
}

check_call_expr :: proc(c: ^Checker, ast: ^Ast) {
    call_expr := ast.variant.(Ast_Call_Expr)

    #partial switch v in call_expr.procedure.variant {
    case Ast_Ident:
        fmt.sbprint(&c.source, v.token.text)
    }
    
    fmt.sbprint(&c.source, "(")
    
    for arg, i in call_expr.args {
        check_expr(c, arg)
        if i + 1 < len(call_expr.args) do fmt.sbprint(&c.source, ", ")
    }
    
    fmt.sbprint(&c.source, ")")
}

check_cast_expr :: proc(c: ^Checker, ast: ^Ast) {
    expr := ast.variant.(Ast_Cast_Expr)
    
    fmt.sbprint(&c.source, "(")
    
    #partial switch expr.op.kind {
    case .Conv:
        fmt.sbprint(&c.source, "(")
        check_type(c, expr.type)
        fmt.sbprint(&c.source, ")")
        check_expr(c, expr.value)
    
    case .Reinterpret:
        fmt.sbprint(&c.source, "*(")
        check_type(c, expr.type)
        fmt.sbprint(&c.source, "*)&")
        check_expr(c, expr.value)

    case:
        assert(false)
    }
    
    
    fmt.sbprint(&c.source, ")")
}

check_binary_op :: proc(c: ^Checker, left: ^Ast, right: ^Ast, op: Token_Kind) {
    switch c.curr_lanes {
    case 1:
        fmt.sbprint(&c.source, "(")
        check_expr(c, left)
        #partial switch op {
        case .Add:                  fmt.sbprint(&c.source, " + ")
        case .Sub:                  fmt.sbprint(&c.source, " - ")
        case .Mul:                  fmt.sbprint(&c.source, " * ")
        case .Div:                  fmt.sbprint(&c.source, " / ")
        case .Mod:                  fmt.sbprint(&c.source, " % ")
        case .Less_Than:            fmt.sbprint(&c.source, " < ")
        case .Less_Than_Equal:      fmt.sbprint(&c.source, " <= ")
        case .Greater_Than:         fmt.sbprint(&c.source, " > ")
        case .Greater_Than_Equal:   fmt.sbprint(&c.source, " >= ")
        case .Equal:                fmt.sbprint(&c.source, " == ")
        case .Not_Equal:            fmt.sbprint(&c.source, " != ")           
        case .Bit_And:              fmt.sbprint(&c.source, " & ")
        case .Bit_Or:               fmt.sbprint(&c.source, " | ")
        case .Bit_Xor:              fmt.sbprint(&c.source, " ^ ")
        case .Bit_Shift_Left:       fmt.sbprint(&c.source, " << ")
        case .Bit_Shift_Right:      fmt.sbprint(&c.source, " >> ")
        case:
            assert(false)
        }
        check_expr(c, right)
        fmt.sbprint(&c.source, ")")
    
    case:
        switch c.curr_lanes {
        case 8:
            #partial switch op {
            case .Add:                  fmt.sbprint(&c.source, "_mm256_add_ps")
            case .Sub:                  fmt.sbprint(&c.source, "_mm256_sub_ps")
            case .Mul:                  fmt.sbprint(&c.source, "_mm256_mul_ps")
            case .Div:                  fmt.sbprint(&c.source, "_mm256_div_ps")
            case .Bit_And:              fmt.sbprint(&c.source, "_mm256_and_si256")
            case .Bit_Or:               fmt.sbprint(&c.source, "_mm256_or_ps")
            case .Bit_Xor:              fmt.sbprint(&c.source, "_mm256_xor_ps")
            case .Bit_Shift_Left:       fmt.sbprint(&c.source, "_mm256_sll_epi32")
            case .Bit_Shift_Right:      fmt.sbprint(&c.source, "_mm256_srl_epi32")
            case .Equal:                fmt.sbprint(&c.source, "_mm256_cmpeq_epi32")
            case:
                fmt.println(op)
                assert(false)
            }
        
        case:
            assert(false)
        }
        fmt.sbprint(&c.source, "(")
        check_expr(c, left)
        fmt.sbprint(&c.source, ", ")
        check_expr(c, right)
        fmt.sbprint(&c.source, ")")
    }
}

check_expr :: proc(c: ^Checker, ast: ^Ast) {
    if ast == nil do return
    
    #partial switch v in ast.variant {
    case Ast_Basic_Literal:
        check_basic_literal(c, ast)

    case Ast_Ident:
        check_ident(c, ast)
    
    case Ast_Call_Expr:
        check_call_expr(c, ast)
    
    case Ast_Unary_Expr:
        
    case Ast_Cast_Expr:
        check_cast_expr(c, ast)
    
    case Ast_Binary_Expr:
        check_binary_op(c, v.left, v.right, v.op.kind)
    }
}

check_value_decl :: proc(c: ^Checker, ast: ^Ast) {
    value := ast.variant.(Ast_Value_Decl)
    check_type(c, value.type)
    fmt.sbprint(&c.source, " ")
    check_ident(c, value.name)
    if value.value != nil {
        fmt.sbprint(&c.source, " = ")
        check_expr(c, value.value)
    } else {
        fmt.sbprint(&c.source, " = 0")
    }
}

check_proc_param_field :: proc(c: ^Checker, ast: ^Ast) {
    field := ast.variant.(Ast_Field)
    check_type(c, field.type)
    fmt.sbprint(&c.source, " ")
    check_type(c, field.name)
}

check_stmt :: proc(c: ^Checker, ast: ^Ast) {
    #partial switch v in ast.variant {
    case Ast_Value_Decl:
        check_value_decl(c, ast)
        
    case Ast_Assign_Stmt:
        check_ident(c, v.left)
        fmt.sbprint(&c.source, " = ")
    
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
            return
        }
        
        switch c.curr_lanes {
        case 1:
            check_binary_op(c, v.left, v.right, op)

        case 8:
            fmt.sbprintf(&c.source, "_mm256_blendv_ps({}, ", v.left.variant.(Ast_Ident).token.text)
            check_binary_op(c, v.left, v.right, op)
            fmt.sbprint(&c.source, ", _mm256_castsi256_ps(vecc_mask))")
        
        case:
        }
    
    
    case Ast_Return_Stmt:
        fmt.sbprint(&c.source, "return ")
        check_expr(c, v.expr)
    
    case Ast_Call_Expr:
        check_call_expr(c, ast)
        
    case Ast_For_Stmt:
        fmt.sbprint(&c.source, "for (")
        check_value_decl(c, v.init)
        fmt.sbprint(&c.source, "; ")
        check_expr(c, v.cond)
        fmt.sbprint(&c.source, "; ")
        check_stmt(c, v.post)
        fmt.sbprint(&c.source, ") ")
        check_block_stmt(c, v.body)
        
    case Ast_For_Range_Stmt:
        iter := v.ident.variant.(Ast_Ident).token.text
        
        assert(c.curr_lanes == 1)

        fmt.sbprintfln(&c.source, "{{ // range {}", iter)
        c.indent += 1
        gen_indent(c)
        
        fmt.sbprintf(&c.source, "i64 {} = ", iter)
        check_expr(c, v.start)
        fmt.sbprintln(&c.source, ";")

        gen_indent(c)
        fmt.sbprintf(&c.source, "i64 vecc_{}_end = ", iter)
        check_expr(c, v.end)
        fmt.sbprintln(&c.source, ";")

        gen_indent(c)
        fmt.sbprintf(&c.source, "v8i32 vecc_mask = _mm256_set1_epi32(0xffffffff);\n")

        // Main loop
        gen_indent(c)
        fmt.sbprint(&c.source, "for (;")
        fmt.sbprintf(&c.source, "{} < vecc_{}_end - 7", iter, iter)
        fmt.sbprint(&c.source, "; ")
        fmt.sbprintf(&c.source, "{} += 8", iter)
        fmt.sbprint(&c.source, ") ")
        {
            this_scope := c.curr_scope
            defer c.curr_scope = this_scope
            c.curr_scope = v.body.variant.(Ast_Block_Stmt).scope
            fmt.sbprintln(&c.source, "{")
            c.indent += 1
            
            gen_indent(c)
            fmt.sbprintf(&c.source, "__m256i vecc_{} = _mm256_add_epi32(\n", iter)
            c.indent += 1
            gen_indent(c)
            fmt.sbprintf(&c.source, "_mm256_set1_epi32({}),\n", iter)
            gen_indent(c)
            fmt.sbprintf(&c.source, "_mm256_set_epi32(7, 6, 5, 4, 3, 2, 1, 0));\n")
            c.indent -= 1
            
            c.curr_lanes = 8
            check_block_stmt(c, v.body, skip_scope = true)
            c.curr_lanes = 1
            
            // gen_indent(c); fmt.sbprint(&c.source, "for(int j = 0; j < 8; j++) printf(\"i %i\\n\", _mm256_extract_epi32(_mm256_permutevar8x32_epi32(vecc_i, _mm256_set1_epi32(j)), 0));\n")
            
            c.indent -= 1
            gen_indent(c)
            fmt.sbprint(&c.source, "}")
            
        }
        fmt.sbprintln(&c.source, "")
        
        // Tail loop
        // gen_indent(c)
        // fmt.sbprint(&c.source, "for (;")
        // fmt.sbprintf(&c.source, "{} < vecc_{}_end", iter, iter)
        // fmt.sbprint(&c.source, "; ")
        // fmt.sbprintf(&c.source, "{} += 1", iter)
        // fmt.sbprint(&c.source, ") ")
        // check_block_stmt(c, v.body)
        // fmt.sbprintln(&c.source, "")
        
        c.indent -= 1
        gen_indent(c)
        fmt.sbprint(&c.source, "}")
        
    
    case Ast_If_Stmt:
        switch c.curr_lanes {
        case 1:
            fmt.sbprint(&c.source, "if (")
            check_expr(c, v.cond)
            fmt.sbprint(&c.source, ") ")
            check_block_stmt(c, v.body)
        
        case 8:
            fmt.sbprint(&c.source, "{ // if\n")
            c.indent += 1

            gen_indent(c)
            fmt.sbprint(&c.source, "v8i32 vecc_prevmask = vecc_mask;\n")

            gen_indent(c)
            fmt.sbprint(&c.source, "vecc_mask = _mm256_and_si256(vecc_mask, ")
            check_expr(c, v.cond)
            fmt.sbprint(&c.source, ");\n")

            gen_indent(c)
            check_block_stmt(c, v.body)
            fmt.sbprint(&c.source, "\n")
            
            gen_indent(c)
            fmt.sbprint(&c.source, "vecc_mask = vecc_prevmask;\n")
            
            c.indent -= 1

            gen_indent(c)
            fmt.sbprint(&c.source, "}\n")
        
        case:
            assert(false)
        }
    
    case Ast_Lanes_Stmt:
        check_lanes_stmt(c, ast)
    
    case Ast_Block_Stmt:
        check_block_stmt(c, ast)
    }
}

check_block_stmt :: proc(c: ^Checker, ast: ^Ast, skip_scope := false) {
    block := ast.variant.(Ast_Block_Stmt)

    this_scope := c.curr_scope
    defer c.curr_scope = this_scope
    if !skip_scope {
        c.curr_scope = block.scope
        fmt.sbprintln(&c.source, "{")
        c.indent += 1
    }
            
    for stmt in block.statements {
        gen_indent(c)
        check_stmt(c, stmt)
        
        #partial switch _ in stmt.variant {
        // case Ast_For_Range_Stmt, Ast_For_Stmt, Ast_Block_Stmt:
            // fmt.sbprint(&c.source, "\n\n")
        
        case:
            fmt.sbprint(&c.source, ";\n")
        }
    }

    if !skip_scope {
        c.indent -= 1
        gen_indent(c)
        fmt.sbprint(&c.source, "}")
    }
}

check_lanes_stmt :: proc(c: ^Checker, ast: ^Ast) {
    lanes := ast.variant.(Ast_Lanes_Stmt)
    prev_lanes := c.curr_lanes
    defer c.curr_lanes = prev_lanes
    num := lanes.num.variant.(Ast_Basic_Literal).token
    assert(num.kind == .Integer)
    c.curr_lanes = strconv.parse_int(num.text) or_else panic("")
    check_block_stmt(c, lanes.body)
}

check_proc_type :: proc(c: ^Checker, ast: ^Ast, name: string) {
    type := ast.variant.(Ast_Proc_Type)
    
    fmt.sbprintf(&c.source, "static ")

    check_type(c, type.result)

    fmt.sbprintf(&c.source, " {}(", name)
    
    for param, i in type.params {
        check_proc_param_field(c, param)
        if i + 1 < len(type.params) {
            fmt.sbprint(&c.source, ", ")
        }
    }
    
    fmt.sbprint(&c.source, ")")
}

// Check and codegen C
check_program :: proc(c: ^Checker) {
    fmt.sbprintfln(&c.source, "#pragma once")
    fmt.sbprintfln(&c.source, "")
    
    fmt.sbprintfln(&c.source, "// WARNING: this file has been generated by the vecc compiler.")
    fmt.sbprintfln(&c.source, "")
    
    fmt.sbprintfln(&c.source, "#include <stdint.h>")
    fmt.sbprintfln(&c.source, "#include <stdio.h>")
    fmt.sbprintfln(&c.source, "#include <immintrin.h>")
    
    fmt.sbprintfln(&c.source, "\n// VECC built-in type definitions\n")
    
    fmt.sbprintfln(&c.source, "typedef int32_t i32;")
    fmt.sbprintfln(&c.source, "typedef int64_t i64;")
    fmt.sbprintfln(&c.source, "typedef float f32;")
    fmt.sbprintfln(&c.source, "typedef __m128  v4f32;")
    fmt.sbprintfln(&c.source, "typedef __m128i v4i32;")
    fmt.sbprintfln(&c.source, "typedef __m256  v8f32;")
    fmt.sbprintfln(&c.source, "typedef __m256i v8i32;")
    
    // for decl in c.decls {
    //     #partial switch v in decl.variant {
    //     case Ast_Proc_Decl:
    //     case:
    //     }
    // }
    
    Sorted_Entity :: struct {
        order_index:    int,
        name:           string,
    }
    
    // only some!
    sorted_entities: [dynamic]Sorted_Entity
    for name, ent in c.curr_scope.entities {
        #partial switch _ in ent.variant {
        case Entity_Proc:
            append(&sorted_entities, Sorted_Entity{name = name, order_index = ent.order_index})
        }
    }
    
    slice.sort_by(sorted_entities[:], proc(a, b: Sorted_Entity) -> bool {
        return a.order_index < b.order_index,
    })
    
    // Now all static...
    fmt.sbprintfln(&c.source, "\n#ifdef VECC_IMPL")
    
    fmt.sbprintfln(&c.source, "\n// VECC function declarations\n")
    
    for sorted in sorted_entities {
        ent := c.curr_scope.entities[sorted.name]

        #partial switch v in ent.variant {
        case Entity_Proc:
            decl := ent.ast.variant.(Ast_Proc_Decl)
            check_proc_type(c, decl.type, sorted.name)
            fmt.sbprint(&c.source, ";\n")
        }
    }
        
    fmt.sbprintfln(&c.source, "\n// VECC function definitions\n")
    
    for sorted in sorted_entities {
        ent := c.curr_scope.entities[sorted.name]

        #partial switch v in ent.variant {
        case Entity_Proc:
            decl := ent.ast.variant.(Ast_Proc_Decl)
            check_proc_type(c, decl.type, sorted.name)
            fmt.sbprint(&c.source, " ")
            check_block_stmt(c, decl.body)
            fmt.sbprint(&c.source, "\n\n")
        }
    }
    
    fmt.sbprintfln(&c.source, "#endif // VECC_IMPL")
}