// C codegen backend
package vecc

import "core:fmt"
import "core:strings"
import "core:slice"
import "core:math"

Gen :: struct {
    source:             strings.Builder,
    curr_scope:         ^Scope,
    curr_file_scope:    ^Scope,
    curr_entity:        ^Entity,
    depth:              int,
    indent:             int,
}

gen_indent :: proc(g: ^Gen) {
    ind := "\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t"
    fmt.sbprint(&g.source, ind[:clamp(g.indent, 0, len(ind) - 1)])
}

gen_begin_scope :: proc(g: ^Gen, scope: ^Scope, visible := true) {
    assert(scope.parent == g.curr_scope)
    g.depth += 1
    g.curr_scope = scope
    if visible {
        g.indent += 1
        gen_print(g, "{\n")
    }
}

gen_end_scope :: proc(g: ^Gen, visible := true) {
    g.depth -= 1
    if visible {
        g.indent -= 1
        gen_indent(g)
        gen_print(g, "}")
    }
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
        #partial switch tv in ast.type.variant {
        case Type_Basic:
            #partial switch tv.kind {
            case .F32:
                val := fmt.tprint(f32(v))
                gen_printf(g, val)
                // HACK this is silly
                if slice.contains(transmute([]u8)val, '.') {
                    gen_print(g, "f")
                } else {
                    gen_print(g, ".0f")
                }

            case .F64:
                gen_print(g, v)

            case:
                assert(false)
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

    gen_print(g, call_expr.procedure.variant.(Ast_Ident).token.text)
    gen_print(g, "(")

    for arg, i in call_expr.args {
        gen_expr(g, arg, top_level = true)
        if i + 1 < len(call_expr.args) {
            gen_print(g, ", ")
        }
    }

    gen_print(g, ")")
}

gen_urnary_expr :: proc(g: ^Gen, ast: ^Ast) {
    expr := ast.variant.(Ast_Unary_Expr)
    #partial switch expr.op.kind {
    case .Sub:
        gen_print(g, "-")

    case .Not:
        gen_print(g, "!")

    case:
        assert(false)
    }

    gen_expr(g, expr.expr)
}

gen_cast_expr :: proc(g: ^Gen, ast: ^Ast) {
    expr := ast.variant.(Ast_Cast_Expr)
    gen_print(g, "(")
    gen_type(g, expr.type.type)
    gen_print(g, ")")
    gen_expr(g, expr.value)
}

gen_expr :: proc(g: ^Gen, ast: ^Ast, top_level := false) {
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
        gen_binary_expr(g, ast, top_level = top_level)

    case Ast_Selector_Expr:
        gen_selector_expr(g, ast)

    case Ast_Index_Expr:
        gen_index_expr(g, ast)

    case Ast_Address_Expr:
        expr := ast.variant.(Ast_Address_Expr)
        gen_print(g, "&")
        gen_expr(g, expr.expr)

    case Ast_Deref_Expr:
        expr := ast.variant.(Ast_Deref_Expr)
        gen_print(g, "*")
        gen_expr(g, expr.expr)

    case:
        assert(false)
    }
}

gen_binary_expr :: proc(g: ^Gen, ast: ^Ast, top_level := false) {
    expr := ast.variant.(Ast_Binary_Expr)

    if !top_level {
        gen_print(g, "(")
    }

    #partial switch v in ast.type.variant {
    case Type_Basic:
        op := ""
        #partial switch expr.op.kind {
        case .Equal:                op = "=="
        case .Less_Than:            op = "<"
        case .Less_Than_Equal:      op = "<="
        case .Greater_Than:         op = ">"
        case .Greater_Than_Equal:   op = ">="
        case .Not_Equal:            op = "!="

        case .Add: op = "+"
        case .Sub: op = "-"
        case .Mul: op = "*"
        case .Div: op = "/"
        case .Mod: op = "%"

        case .Bit_And:          op = "&"
        case .Bit_Or:           op = "|"
        case .Bit_Xor:          op = "^"
        case .Bit_Shift_Left:   op = "<<"
        case .Bit_Shift_Right:  op = ">>"
        }
        gen_expr(g, expr.left)
        gen_printf(g, " {} ", op)
        gen_expr(g, expr.right)

    case Type_Array:
        op_name := ""
        #partial switch expr.op.kind {
        case .Equal:                op_name = "eq"
        case .Less_Than:            op_name = "lt"
        case .Less_Than_Equal:      op_name = "le"
        case .Greater_Than:         op_name = "gt"
        case .Greater_Than_Equal:   op_name = "ge"
        case .Not_Equal:            op_name = "neq"

        case .Add: op_name = "add"
        case .Sub: op_name = "sub"
        case .Mul: op_name = "mul"
        case .Div: op_name = "div"
        case .Mod: op_name = "mod"

        case .Bit_And:          op_name = "and"
        case .Bit_Or:           op_name = "or"
        case .Bit_Xor:          op_name = "xor"
        case .Bit_Shift_Left:   op_name = "shl"
        case .Bit_Shift_Right:  op_name = "shr"
        }
        gen_printf(g, "%s_%s", ast.type.cname_lower, op_name)
        gen_print(g, "(")
        gen_expr(g, expr.left)
        gen_print(g, ", ")
        gen_expr(g, expr.right)
        gen_print(g, ")")

    case:
        unimplemented()
    }

    if !top_level {
        gen_print(g, ")")
    }
}

gen_selector_expr :: proc(g: ^Gen, ast: ^Ast) {
    expr := ast.variant.(Ast_Selector_Expr)
    gen_expr(g, expr.left)
    #partial switch v in expr.left.type.variant {
    case Type_Array:
        name := expr.right.variant.(Ast_Ident).token.text

        assert(len(name) == 1)
        index := -1
        switch name[0] {
        case 'x', 'r': index = 0
        case 'y', 'g': index = 1
        case 'z', 'b': index = 2
        case 'w', 'a': index = 3
        }

        assert(index >= 0)

        gen_print(g, ".data[")
        gen_print(g, index)
        gen_print(g, "]")

    case Type_Pointer:
        // TODO: pointers to pointers etc
        gen_print(g, "->")
        gen_expr(g, expr.right)

    case:
        gen_print(g, ".")
        gen_expr(g, expr.right)
    }
}

gen_index_expr :: proc(g: ^Gen, ast: ^Ast) {
    expr := ast.variant.(Ast_Index_Expr)
    gen_expr(g, expr.left)
    gen_print(g, "[")
    gen_expr(g, expr.index)
    gen_print(g, "]")
}

gen_proc_param_field :: proc(g: ^Gen, ast: ^Ast) {
    decl := ast.variant.(Ast_Value_Decl)
    gen_ast_type(g, decl.type)
    gen_print(g, " ")
    gen_ident(g, decl.name)
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
        switch v.kind {
        case .Vector:
            gen_printf(g, "typedef struct {} {{ ", type.cname)
            assert(math.is_power_of_two(v.len))
            basic := v.type.variant.(Type_Basic)

            elem_bytes := 0
            switch basic.kind {
            case .B8 : elem_bytes = 1
            case .B16: elem_bytes = 2
            case .B32: elem_bytes = 4
            case .B64: elem_bytes = 8

            case .I8 : elem_bytes = 1
            case .I16: elem_bytes = 2
            case .I32: elem_bytes = 4
            case .I64: elem_bytes = 8

            case .U8 : elem_bytes = 1
            case .U16: elem_bytes = 2
            case .U32: elem_bytes = 4
            case .U64: elem_bytes = 8

            case .F32: elem_bytes = 4
            case .F64: elem_bytes = 8
            }

            // simd_len :=

            num_simds := v.len / (128 / 8)
            if num_simds <= 0 do num_simds = 1
            simd_type_name := "__m128i"
            gen_printf(g, "{} data[{}]; ", simd_type_name, num_simds)
            gen_printf(g, "}} {};\n", type.cname)

        case .Fixed_Array:
            gen_printf(g, "typedef struct {} {{ ", type.cname)
            gen_printf(g, "{} data[{}]; ", v.type.cname, v.len)
            gen_printf(g, "}} {};\n", type.cname)
        }

    case Type_Struct:
        gen_printf(g, "typedef struct {} {{\n", type.cname)
        for field in v.fields {
            gen_printf(g, "\t{} {};\n", field.type.cname, field.name)
        }
        gen_printf(g, "}} {};\n", type.cname)
    }
}

_Gen_Named_Op_Proc :: struct {
    name:       string,
    op:         string,
    boolean:    bool, // HACK
    integer:    bool, // HACK
}

@(rodata)
_gen_named_binary_ops := #partial[Token_Kind]_Gen_Named_Op_Proc{
    .Equal              = {name = "eq" , op = "==", boolean = true},
    .Less_Than          = {name = "lt" , op = "<",  boolean = true},
    .Less_Than_Equal    = {name = "le" , op = "<=", boolean = true},
    .Greater_Than       = {name = "gt" , op = ">",  boolean = true},
    .Greater_Than_Equal = {name = "ge" , op = ">=", boolean = true},
    .Not_Equal          = {name = "neq", op = "!=", boolean = true},
    .Add                = {name = "add", op = "+"},
    .Sub                = {name = "sub", op = "-"},
    .Mul                = {name = "mul", op = "*"},
    .Div                = {name = "div", op = "/"},
    .Mod                = {name = "mod", op = "%",  integer = true},
    .Bit_And            = {name = "and", op = "&",  integer = true},
    .Bit_Or             = {name = "or" , op = "|",  integer = true},
    .Bit_Xor            = {name = "xor", op = "^",  integer = true},
    .Bit_Shift_Left     = {name = "shl", op = "<<", integer = true},
    .Bit_Shift_Right    = {name = "shr", op = ">>", integer = true},
}

@(rodata)
_gen_named_unary_ops := #partial[Token_Kind]_Gen_Named_Op_Proc{
    .Not = {name = "not", op = "!", boolean = true},
    .Sub = {name = "neg", op = "-"},
}

gen_type_procs_decls :: proc(g: ^Gen, type: ^Type) {
    #partial switch v in type.variant {
    case Type_Array:
        for op in _gen_named_binary_ops {
            if op.name == "" do continue
            if type_is_boolean(v.type) != op.boolean do continue
            if type_is_integer(v.type) != op.integer do continue
            gen_printf(g, "static {0} {1}_{2}({0} a, {0} b);\n", type.cname, type.cname_lower, op.name)
        }
        for op in _gen_named_unary_ops {
            if op.name == "" do continue
            if type_is_boolean(v.type) != op.boolean do continue
            if type_is_integer(v.type) != op.integer do continue
            gen_printf(g, "static {0} {1}_{2}({0} a);\n", type.cname, type.cname_lower, op.name)
        }
    }
}

gen_type_procs_defs :: proc(g: ^Gen, type: ^Type) {
    #partial switch v in type.variant {
    case Type_Array:
        for op in _gen_named_binary_ops {
            if op.name == "" do continue
            if type_is_boolean(v.type) != op.boolean do continue
            if type_is_integer(v.type) != op.integer do continue
            gen_printf(g, "static {0} {1}_{2}({0} a, {0} b) {{\n", type.cname, type.cname_lower, op.name)
            gen_printf(g, "\t{0} result;\n", type.cname)
            for i in 0..<v.len {
                gen_printf(g, "\tresult.data[{0}] = a.data[{0}] {1} b.data[{0}];\n", i, op.op)
            }
            gen_printf(g, "\treturn result;\n")
            gen_printf(g, "}}\n")
        }
        for op in _gen_named_unary_ops {
            if op.name == "" do continue
            if type_is_boolean(v.type) != op.boolean do continue
            if type_is_integer(v.type) != op.integer do continue
            gen_printf(g, "static {0} {1}_{2}({0} a) {{\n", type.cname, type.cname_lower, op.name)
            for i in 0..<v.len {
                gen_printf(g, "\ta.data[{0}] = {1}a.data[{0}];\n", i, op.op)
            }
            gen_printf(g, "}}\n")
        }
    }
}

gen_type_generate_cname :: proc(g: ^Gen, type: ^Type, ent_name := "") -> string {
    if type.cname != "" {
        return type.cname
    }

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

        switch v.kind {
        case .Vector:
            return fmt.tprintf("Vec{}_{}", v.len, name)
        case .Fixed_Array:
            return fmt.tprintf("Aos{}_{}", v.len, name)
        }

    case Type_Pointer:
        name := gen_type_generate_cname(g, v.type)
        switch v.kind {
        case .Single, .Multi:
            return fmt.tprintf("{}*", name)
        }

    case Type_Struct:
        return ent_name // HACK
    }

    assert(false)
    return ""
}

gen_value_decl :: proc(g: ^Gen, ast: ^Ast) {
    decl := ast.variant.(Ast_Value_Decl)

    switch decl.mut {
    case .Invalid:
        assert(false)
    // TODO: generate macro defines for compile time constants? What about shadowing?
    case .Constant, .Immutable:
        gen_print(g, "const ")
    case .Mutable:
    }

    gen_ast_type(g, decl.type)
    gen_print(g, " ")
    gen_ident(g, decl.name)
    gen_print(g, " ")
    if decl.value != nil {
        gen_print(g, "= ")
        gen_expr(g, decl.value, top_level = true)
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
            gen_print(g, "{0}")
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
    case Ast_Call_Expr:         gen_call_expr(g, ast)

    case Ast_Value_Decl:        gen_value_decl(g, ast)
    case Ast_Struct_Decl:       gen_struct_decl(g, ast)
    // case Ast_Proc_Decl:         gen_proc_decl(g, ast)
    }
}

gen_block_stmt :: proc(g: ^Gen, ast: ^Ast) {
    stmt := ast.variant.(Ast_Block_Stmt)

    gen_begin_scope(g, stmt.scope)
    defer gen_end_scope(g)

    for s in stmt.statements {
        #partial switch _ in s.variant {
        case Ast_Proc_Decl, Ast_Struct_Decl:
        case:
            gen_indent(g)
            gen_stmt(g, s)
            gen_print(g, ";\n")
        }
    }
}

gen_if_stmt :: proc(g: ^Gen, ast: ^Ast) {
    stmt := ast.variant.(Ast_If_Stmt)
    gen_print(g, "if (")
    gen_expr(g, stmt.cond, top_level = true)
    gen_print(g, ") ")
    gen_block_stmt(g, stmt.if_body)
    if stmt.else_body != nil {
        gen_print(g, " else ")
        gen_block_stmt(g, stmt.else_body)
    }
}

gen_for_stmt :: proc(g: ^Gen, ast: ^Ast) {
    stmt := ast.variant.(Ast_For_Stmt)
    gen_begin_scope(g, stmt.scope, visible = false)
    defer gen_end_scope(g, visible = false)
    gen_print(g, "for (")
    gen_value_decl(g, stmt.init)
    gen_print(g, "; ")
    gen_expr(g, stmt.cond, top_level = true)
    gen_print(g, "; ")
    gen_stmt(g, stmt.post)
    gen_print(g, ") ")
    gen_block_stmt(g, stmt.body)
}

gen_break_stmt :: proc(g: ^Gen, ast: ^Ast) {
    stmt := ast.variant.(Ast_Break_Stmt)
    gen_print(g, "break")
}

gen_continue_stmt :: proc(g: ^Gen, ast: ^Ast) {
    stmt := ast.variant.(Ast_Continue_Stmt)
    gen_print(g, "continue")
}

gen_for_range_stmt :: proc(g: ^Gen, ast: ^Ast) {
    stmt := ast.variant.(Ast_For_Range_Stmt)
}

gen_assign_stmt :: proc(g: ^Gen, ast: ^Ast) {
    stmt := ast.variant.(Ast_Assign_Stmt)
    gen_expr(g, stmt.left)
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
    gen_expr(g, stmt.right, top_level = true)
}

gen_return_stmt :: proc(g: ^Gen, ast: ^Ast) {
    stmt := ast.variant.(Ast_Return_Stmt)
    gen_print(g, "return")
    gen_print(g, " ")
    gen_expr(g, stmt.value, top_level = true)
}



gen_proc_decl :: proc(g: ^Gen, ast: ^Ast) {
    decl := ast.variant.(Ast_Proc_Decl)
    // gen_proc_type(g, decl.type, sorted.name)
    // gen_print(g, " ")
    // gen_block_stmt(g, decl.body)
    // gen_print(g, "\n\n")
}

gen_struct_decl :: proc(g: ^Gen, ast: ^Ast) {
    decl := ast.variant.(Ast_Struct_Decl)
}

// HACK, set struct cnames to one of the associated entities.
// Two exact same structs are rare but possible
gen_struct_entity_cnames_recursive :: proc(g: ^Gen, scope: ^Scope) {
    for name, ent in scope.entities {
        #partial switch v in ent.variant {
        case Entity_Struct:
            v.type.cname = name
        }
    }

    for child in scope.children {
        g.curr_scope = child
        gen_struct_entity_cnames_recursive(g, child)
        g.curr_scope = g.curr_scope.parent
    }
}

gen_type_entity_cnames_recursive :: proc(g: ^Gen, scope: ^Scope) {
    for name, ent in scope.entities {
        #partial switch v in ent.variant {
        case Entity_Type:
            v.type.cname = gen_type_generate_cname(g, v.type, name)
            v.type.cname_lower = strings.to_lower(v.type.cname)
        }
    }

    for child in scope.children {
        g.curr_scope = child
        gen_type_entity_cnames_recursive(g, child)
        g.curr_scope = g.curr_scope.parent
    }
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

    // Flatten scope hierarchy.
    // Ignore local variables, gen only nested procs.

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


    for e in sorted_entities do fmt.println(e)

    gen_print(g, "#ifdef __cplusplus\n")
    gen_print(g, "#else // __cplusplus\n")
    gen_print(g, "#define false 0\n")
    gen_print(g, "#define true 1\n")
    gen_print(g, "#endif // __cplusplus\n")

    gen_print(g, "\n// VECC type definitions\n")

    gen_print(g, "typedef int8_t bool8_t;\n")
    gen_print(g, "typedef int16_t bool16_t;\n")
    gen_print(g, "typedef int32_t bool32_t;\n")
    gen_print(g, "typedef int64_t bool64_t;\n")

    gen_struct_entity_cnames_recursive(g, g.curr_file_scope)

    gen_type_entity_cnames_recursive(g, g.curr_file_scope)

    for sorted in sorted_entities {
        #partial switch v in sorted.entity.variant {
        case Entity_Type:
            gen_type_decl(g, v.type)
            // gen_type_procs_decls(g, v.type)
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

    gen_print(g, "#endif // VECC_DEFINED\n\n")

    // FIXME: make sure the impl expands only once
    gen_print(g, "\n#ifdef VECC_IMPL\n")

    gen_print(g, "\n// VECC private function declarations\n\n")

    for sorted in sorted_entities {
        #partial switch v in sorted.entity.variant {
        case Entity_Type:
            // gen_type_procs_defs(g, v.type)
        }
    }

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

    gen_print(g, "\n// VECC global variable declarations\n\n")

    for sorted in sorted_entities {
        #partial switch v in sorted.entity.variant {
        case Entity_Variable:
            decl := sorted.entity.ast.variant.(Ast_Value_Decl)
            gen_value_decl(g, sorted.entity.ast)
            gen_print(g, ";\n")
        }
    }

    gen_print(g, "\n// VECC function definitions\n\n")

    for sorted in sorted_entities {
        // ast_print(sorted.entity.ast, sorted.name, 0)

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

    gen_print(g, "#endif // VECC_IMPL\n")
}