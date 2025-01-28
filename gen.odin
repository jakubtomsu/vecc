// C codegen backend
package vecc

import "core:fmt"
import "core:strings"
import "core:slice"

Gen :: struct {
    source:             strings.Builder,
    curr_scope:         ^Scope,
    curr_file_scope:    ^Scope,
    curr_entity:        ^Entity,
    depth:              int,
}

gen_indent :: proc(g: ^Gen) {
    ind := "\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t"
    fmt.sbprint(&g.source, ind[:clamp(g.depth, 0, len(ind) - 1)])
}

gen_begin_scope :: proc(g: ^Gen, scope: ^Scope) {
    assert(scope.parent == g.curr_scope)
    g.depth += 1
    g.curr_scope = scope
    gen_print(g, "{\n")
}

gen_end_scope :: proc(g: ^Gen) {
    gen_print(g, "}\n")
    g.depth -= 1
    g.curr_scope = g.curr_scope.parent
}

gen_print :: proc(g: ^Gen, args: ..any) {
    fmt.sbprint(&g.source, args = args)
}

gen_printf :: proc(g: ^Gen, format: string, args: ..any) {
    fmt.sbprintf(&g.source, fmt = format, args = args)
}

gen_basic_literal :: proc(g: ^Gen, ast: ^Ast) {
    lit := ast.variant.(Ast_Basic_Literal)
    switch v in ast.value {
    case bool:
        gen_print(g, v)

    case i128:
        gen_print(g, v)

    case f64:
        gen_print(g, v)
        #partial switch v in ast.type.variant {
        case Type_Basic:
            #partial switch v.kind {
            case .F32:
                gen_print(g, ".0f") // TEMP HACK
            }
        case:
            assert(false)
        }

    case nil:
        assert(false)
    }
}

gen_ident :: proc(g: ^Gen, ast: ^Ast) {
    ident := ast.variant.(Ast_Ident)
    gen_print(g, ident.token.text)
}

gen_call_expr :: proc(g: ^Gen, ast: ^Ast) {
    call_expr := ast.variant.(Ast_Call_Expr)

    gen_print(g, "(")

    for arg, i in call_expr.args {
        gen_expr(g, arg)
        if i + 1 < len(call_expr.args) {
            gen_print(g, ", ")
        }
    }

    gen_print(g, ")")
}

gen_urnary_expr :: proc(g: ^Gen, ast: ^Ast) {
    expr := ast.variant.(Ast_Unary_Expr)
    unimplemented()
}

gen_cast_expr :: proc(g: ^Gen, ast: ^Ast) {
    expr := ast.variant.(Ast_Cast_Expr)
    unimplemented()
}

gen_expr :: proc(g: ^Gen, ast: ^Ast) {
    #partial switch v in ast.variant {
    case Ast_Basic_Literal:
        gen_basic_literal(g, ast)

    case Ast_Ident:
        gen_ident(g, ast)

    case Ast_Call_Expr:
        gen_call_expr(g, ast)

    case Ast_Cast_Expr:
        gen_cast_expr(g, ast)

    case Ast_Unary_Expr:
        gen_urnary_expr(g, ast)

    case Ast_Binary_Expr:
        gen_binary_expr(g, ast)

    case:
        assert(false)
    }
}

gen_binary_expr :: proc(g: ^Gen, ast: ^Ast) {
    expr := ast.variant.(Ast_Binary_Expr)
    unimplemented()
}

gen_proc_param_field :: proc(g: ^Gen, ast: ^Ast) {
    field := ast.variant.(Ast_Field)
    gen_ast_type(g, field.type)
    gen_print(g, " ")
    gen_ident(g, field.name)
}

gen_proc_type :: proc(g: ^Gen, ast: ^Ast, name: string) {
    type := ast.variant.(Ast_Proc_Type)
    gen_ast_type(g, type.result)
    gen_print(g, " ")
    gen_print(g, name)
    gen_print(g, "(")

    for param, i in type.params {
        gen_proc_param_field(g, param)
        if i + 1 < len(type.params) {
            gen_print(g, ", ")
        }
    }

    gen_print(g, ")")
}

gen_ast_type :: proc(g: ^Gen, ast: ^Ast) {
    if ast == nil {
        gen_print(g, "void")
    } else {
        gen_type(g, ast.type)
    }
}

gen_type :: proc(g: ^Gen, type: ^Type) {
    if type == nil {
        gen_print(g, "void")
    } else {
        gen_print(g, type.cname)
    }
}

gen_type_decl :: proc(g: ^Gen, type: ^Type) {
    #partial switch v in type.variant {
    case Type_Basic, Type_Pointer:
        // Nothing

    case Type_Array:
        gen_printf(g, "typedef struct {} {{", type.cname)
        gen_printf(g, "\t{} data[{}],\n", v.type.cname, v.len)
        gen_printf(g, "}} {};", type.cname)
    }
}

gen_type_generate_cname :: proc(g: ^Gen, type: ^Type) -> string {
    #partial switch v in type.variant {
    case Type_Basic:
        switch v.kind {
        case .B8:   return "bool8_t"
        case .B16:  return "bool16_t"
        case .B32:  return "bool32_t"
        case .B64:  return "bool64_t"
        case .I8:   return "int8_t"
        case .I16:  return "int16_t"
        case .I32:  return "int32_t"
        case .I64:  return "int64_t"
        case .U8:   return "uint8_t"
        case .U16:  return "uint16_t"
        case .U32:  return "uint32_t"
        case .U64:  return "uint64_t"
        case .F32:  return "float"
        case .F64:  return "double"
        }

    case Type_Array:
        name := gen_type_generate_cname(g, v.type)

        #partial switch v.kind {
        case .Vector:
            return fmt.tprintf("Vec{}_{}", v.len, name)
        case .Fixed_Array:
            return fmt.tprintf("Aos{}_{}", v.len, name)
        }

    case Type_Pointer:
        name := gen_type_generate_cname(g, v.type)
        #partial switch v.kind {
        case .Single:
            return fmt.tprintf("{}*", name)
        case .Multi:
            return fmt.tprintf("{}[]", name)
        }
    }
    assert(false)
    return ""
}

gen_value_decl :: proc(g: ^Gen, ast: ^Ast) {
    decl := ast.variant.(Ast_Value_Decl)
    if decl.const {
        gen_print(g, "const ")
    }
    gen_ast_type(g, decl.type)
    gen_print(g, " ")
    gen_ident(g, decl.name)
    gen_print(g, " ")
    if decl.value != nil {
        gen_print(g, "= ")
        gen_expr(g, decl.value)
    } else {
        gen_print(g, "= ")
        #partial switch v in decl.type.type.variant {
        case Type_Basic:
            switch v.kind {
            case .B8,
                 .B16,
                 .B32,
                 .B64:
                gen_print(g, "false")

            case .I8,
                 .I16,
                 .I32,
                 .I64,
                 .U8,
                 .U16,
                 .U32,
                 .U64:
                gen_print(g, "0")

            case .F32:
                gen_print(g, "0.0f")

            case .F64:
                gen_print(g, "0.0")
            }

        case:
            gen_print(g, "= {}")
        }
    }
}

gen_stmt :: proc(g: ^Gen, ast: ^Ast) {
    #partial switch v in ast.variant {
    case Ast_Block_Stmt:        gen_block_stmt(g, ast)
    case Ast_If_Stmt:           gen_if_stmt(g, ast)
    case Ast_For_Stmt:          gen_for_stmt(g, ast)
    case Ast_Break_Stmt:        gen_break_stmt(g, ast)
    case Ast_Continue_Stmt:     gen_continue_stmt(g, ast)
    case Ast_For_Range_Stmt:    gen_for_range_stmt(g, ast)
    case Ast_Assign_Stmt:       gen_assign_stmt(g, ast)
    case Ast_Return_Stmt:       gen_return_stmt(g, ast)
    case Ast_Value_Decl:        gen_value_decl(g, ast)
    }
}

gen_block_stmt :: proc(g: ^Gen, ast: ^Ast) {
    stmt := ast.variant.(Ast_Block_Stmt)

    gen_begin_scope(g, stmt.scope)
    defer gen_end_scope(g)

    for s in stmt.statements {
        gen_indent(g)
        gen_stmt(g, s)
        gen_print(g, ";\n")
    }
}

gen_if_stmt :: proc(g: ^Gen, ast: ^Ast) {
    stmt := ast.variant.(Ast_If_Stmt)
}

gen_for_stmt :: proc(g: ^Gen, ast: ^Ast) {
    stmt := ast.variant.(Ast_For_Stmt)
}

gen_break_stmt :: proc(g: ^Gen, ast: ^Ast) {
    stmt := ast.variant.(Ast_Break_Stmt)
}

gen_continue_stmt :: proc(g: ^Gen, ast: ^Ast) {
    stmt := ast.variant.(Ast_Continue_Stmt)
}

gen_for_range_stmt :: proc(g: ^Gen, ast: ^Ast) {
    stmt := ast.variant.(Ast_For_Range_Stmt)
}

gen_assign_stmt :: proc(g: ^Gen, ast: ^Ast) {
    stmt := ast.variant.(Ast_Assign_Stmt)
    gen_ident(g, stmt.left)
    #partial switch stmt.op.kind {
    case .Assign_Add:               gen_print(g, " += ")
    case .Assign_Sub:               gen_print(g, " -= ")
    case .Assign_Mul:               gen_print(g, " *= ")
    case .Assign_Div:               gen_print(g, " /= ")
    case .Assign_Mod:               gen_print(g, " %= ")
    case .Assign_Bit_And:           gen_print(g, " &= ")
    case .Assign_Bit_Or:            gen_print(g, " |= ")
    case .Assign_Bit_Xor:           gen_print(g, " ^= ")
    case .Assign_Bit_Shift_Left:    gen_print(g, " <<= ")
    case .Assign_Bit_Shift_Right:   gen_print(g, " >>= ")

    case .Assign:
        gen_print(g, " = ")
    }
    gen_expr(g, stmt.right)
}

gen_return_stmt :: proc(g: ^Gen, ast: ^Ast) {
    stmt := ast.variant.(Ast_Return_Stmt)
    gen_print(g, "return")
    gen_print(g, " ")
    gen_expr(g, stmt.value)
}


gen_program :: proc(g: ^Gen) {
    gen_print(g, "#ifndef VECC_DEFINED\n")
    gen_print(g, "#define VECC_DEFINED 1\n")
    gen_print(g, "\n")

    gen_print(g, "// WARNING: this file was generated by the VecC compiler.\n")
    gen_print(g, "\n")

    gen_print(g, "#include <stdint.h>\n")
    gen_print(g, "#include <stdio.h>\n")
    gen_print(g, "#include <immintrin.h>\n")

    Sorted_Entity :: struct {
        order_index:    int,
        name:           string,
        entity:         ^Entity,
    }

    // only some!
    sorted_entities: [dynamic]Sorted_Entity
    for name, ent in g.curr_file_scope.entities {
        #partial switch _ in ent.variant {
        case Entity_Proc:
        case Entity_Variable:
        case Entity_Type:
        case:
            continue
        }
        append(&sorted_entities, Sorted_Entity{name = name, order_index = ent.order_index, entity = ent})
    }

    slice.sort_by(sorted_entities[:], proc(a, b: Sorted_Entity) -> bool {
        return a.order_index < b.order_index,
    })

    fmt.println(sorted_entities)


    gen_print(g, "\n// VECC type definitions\n")

    gen_print(g, "typedef int8_t bool8_t;\n")
    gen_print(g, "typedef int16_t bool16_t;\n")
    gen_print(g, "typedef int32_t bool32_t;\n")
    gen_print(g, "typedef int64_t bool64_t;\n")

    for name, ent in g.curr_file_scope.entities {
        #partial switch v in ent.variant {
        case Entity_Type:
            v.type.cname = gen_type_generate_cname(g, v.type)
        }
    }

    for sorted in sorted_entities {
        #partial switch v in sorted.entity.variant {
        case Entity_Type:
            gen_type_decl(g, v.type)
        }
    }

    gen_print(g, "\n// VECC exported function declarations\n")

    for sorted in sorted_entities {
        #partial switch v in sorted.entity.variant {
        case Entity_Proc:
            decl := sorted.entity.ast.variant.(Ast_Proc_Decl)
            if !decl.export do continue
            gen_proc_type(g, decl.type, sorted.name)
            gen_print(g, ";\n")
        }
    }

    gen_print(g, "\n// VECC exported global variable declarations\n")

    for sorted in sorted_entities {
        #partial switch v in sorted.entity.variant {
        case Entity_Variable:
            decl := sorted.entity.ast.variant.(Ast_Value_Decl)
            if decl.export do continue
        }
    }

    gen_print(g, "#endif // VECC_DEFINED\n\n")

    // FIXME: make sure the impl expands only once
    gen_print(g, "\n#ifdef VECC_IMPL")

    gen_print(g, "\n// VECC private function declarations\n")

    for sorted in sorted_entities {
        #partial switch v in sorted.entity.variant {
        case Entity_Proc:
            decl := sorted.entity.ast.variant.(Ast_Proc_Decl)
            if decl.export do continue
            gen_print(g, "static ")
            gen_proc_type(g, decl.type, sorted.name)
            gen_print(g, ";\n")
        }
    }

    gen_print(g, "\n// VECC private global variable declarations\n")

    for sorted in sorted_entities {
        #partial switch v in sorted.entity.variant {
        case Entity_Variable:
            decl := sorted.entity.ast.variant.(Ast_Value_Decl)
            if !decl.export do continue
        }
    }

    gen_print(g, "\n// VECC function definitions\n")

    for sorted in sorted_entities {
        ast_print(sorted.entity.ast, sorted.name, 0)

        #partial switch v in sorted.entity.variant {
        case Entity_Proc:
            decl := sorted.entity.ast.variant.(Ast_Proc_Decl)
            if !decl.export {
                fmt.sbprintf(&g.source, "static ")
            }
            gen_proc_type(g, decl.type, sorted.name)
            gen_print(g, " ")
            gen_block_stmt(g, decl.body)
            gen_print(g, "\n\n")
        }
    }

    gen_print(g, "#endif // VECC_IMPL")
}