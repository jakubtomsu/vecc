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
    types:              []^Type,
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

gen_value :: proc(g: ^Gen, value: Value, type: ^Type) {
    switch v in value {
    case bool:
        gen_print(g, v)

    case i128:
        gen_print(g, v)

    case f64:
        #partial switch tv in type.variant {
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

    case [8]i32:
        gen_printf(g, "_mm256_set_epi32(%i, %i, %i, %i, %i, %i, %i, %i)",
            v[7],
            v[6],
            v[5],
            v[4],
            v[3],
            v[2],
            v[1],
            v[0],
        )

    case [8]f32:
    gen_printf(g, "_mm256_set_ps(%i, %i, %i, %i, %i, %i, %i, %i)",
            v[7],
            v[6],
            v[5],
            v[4],
            v[3],
            v[2],
            v[1],
            v[0],
        )

    case nil:
        assert(false)
    }
}

gen_basic_literal :: proc(g: ^Gen, ast: ^Ast) {
    lit := ast.variant.(Ast_Basic_Literal)
    gen_value(g, ast.value, ast.type)
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

// Left is assumed to be already checked
// THIS CONVERSION THING IS A HACK
gen_possible_auto_conv_expr :: proc(g: ^Gen, left, right: ^Ast) {
    emitted_conv := false
    #partial switch lv in left.type.variant {
    case Type_Array:
        #partial switch lr in right.type.variant {
        case Type_Basic:
            assert(lv.type == right.type)
            gen_printf(g, "{}_broadcast(", left.type.cname_lower)
            emitted_conv = true
        }
    }

    gen_expr(g, right, top_level = true)

    if emitted_conv {
        gen_print(g, ")")
    }
}

gen_binary_expr :: proc(g: ^Gen, ast: ^Ast, top_level := false) {
    expr := ast.variant.(Ast_Binary_Expr)
    gen_binary_op(g,
        left = expr.left,
        right = expr.right,
        op = expr.op.kind,
        top_level = top_level,
    )
}

gen_binary_op :: proc(g: ^Gen, left: ^Ast, right: ^Ast, op: Token_Kind, top_level := false) {
    if !top_level {
        gen_print(g, "(")
    }

    #partial switch v in left.type.variant {
    case Type_Basic:
        op_name := _token_str[op]
        gen_expr(g, left)
        gen_printf(g, " {} ", op_name)
        gen_possible_auto_conv_expr(g, left, right)

    case Type_Array:
        op_name := ""

        #partial switch op {
        case .Equal:                op_name = "eq"
        case .Less_Than:            op_name = "lt"
        case .Less_Than_Equal:      op_name = "le"
        case .Greater_Than:         op_name = "gt"
        case .Greater_Than_Equal:   op_name = "ge"
        case .Not_Equal:            op_name = "neq"
        case .Add:                  op_name = "add"
        case .Sub:                  op_name = "sub"
        case .Mul:                  op_name = "mul"
        case .Div:                  op_name = "div"
        case .Mod:                  op_name = "mod"
        case .Bit_And:              op_name = "and"
        case .Bit_Or:               op_name = "or"
        case .Bit_Xor:              op_name = "xor"
        case .Bit_Shift_Left:       op_name = "shl"
        case .Bit_Shift_Right:      op_name = "shr"
        }

        gen_printf(g, "%s_%s", left.type.cname_lower, op_name)
        gen_print(g, "(")
        gen_expr(g, left)
        gen_print(g, ", ")
        gen_possible_auto_conv_expr(g, left, right)
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
            // gen_printf(g, "typedef struct {} {{ ", type.cname)
            // assert(math.is_power_of_two(v.len))
            // basic := v.type.variant.(Type_Basic)

            // elem_bytes := 0
            // switch basic.kind {
            // case .B8 : elem_bytes = 1
            // case .B16: elem_bytes = 2
            // case .B32: elem_bytes = 4
            // case .B64: elem_bytes = 8

            // case .I8 : elem_bytes = 1
            // case .I16: elem_bytes = 2
            // case .I32: elem_bytes = 4
            // case .I64: elem_bytes = 8

            // case .U8 : elem_bytes = 1
            // case .U16: elem_bytes = 2
            // case .U32: elem_bytes = 4
            // case .U64: elem_bytes = 8

            // case .F32: elem_bytes = 4
            // case .F64: elem_bytes = 8
            // }

            // // simd_len :=

            // num_simds := v.len / (128 / 8)
            // if num_simds <= 0 do num_simds = 1
            // simd_type_name := "__m128i"
            // gen_printf(g, "{} data[{}]; ", simd_type_name, num_simds)
            // gen_printf(g, "}} {};\n", type.cname)

        case .Fixed_Array:
            fmt.println(type_to_string(type), ":", type_to_string(v.type))
            assert(v.type.cname != "")
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

Gen_Op_Proc :: struct {
    name:       string,
    op:         Token_Kind, // C!
    flags:      bit_set[Gen_Op_Proc_Flag],
}

Gen_Op_Proc_Flag :: enum u8 {
    Integer,
    Float,
    Boolean,
}

@(rodata)
_gen_named_binary_ops := [?]Gen_Op_Proc{
    // {name = "eq" , op = .Equal,              flags = {.Integer, .Float}},
    // {name = "lt" , op = .Less_Than,          flags = {.Integer, .Float}},
    // {name = "le" , op = .Less_Than_Equal,    flags = {.Integer, .Float}},
    // {name = "gt" , op = .Greater_Than,       flags = {.Integer, .Float}},
    // {name = "ge" , op = .Greater_Than_Equal, flags = {.Integer, .Float}},
    // {name = "neq", op = .Not_Equal,          flags = {.Integer, .Float}},
    {name = "add", op = .Add,                flags = {.Integer, .Float}},
    {name = "sub", op = .Sub,                flags = {.Integer, .Float}},
    {name = "mul", op = .Mul,                flags = {.Integer, .Float}},
    {name = "div", op = .Div,                flags = {.Integer, .Float}},
    {name = "mod", op = .Mod,                flags = {.Integer}},
    {name = "and", op = .Bit_And,            flags = {.Integer}},
    {name = "or" , op = .Bit_Or,             flags = {.Integer}},
    {name = "xor", op = .Bit_Xor,            flags = {.Integer}},
    // {name = "shl", op = .Bit_Shift_Left,     flags = {.Integer}},
    // {name = "shr", op = .Bit_Shift_Right,    flags = {.Integer}},
}

@(rodata)
_gen_named_unary_ops := [?]Gen_Op_Proc{
    {name = "not", op = .Not, flags = {.Integer, .Boolean}},
    {name = "neg", op = .Sub, flags = {.Integer, .Float}},
}

_gen_type_matches_op_proc :: proc(type: ^Type, op: Gen_Op_Proc) -> bool {
    if      type_is_integer(type) && .Integer in op.flags do return true
    else if type_is_float  (type) && .Float   in op.flags do return true
    else if type_is_boolean(type) && .Boolean in op.flags do return true
    return false
}

gen_type_procs_decls :: proc(g: ^Gen, type: ^Type) {
    #partial switch v in type.variant {
    case Type_Array:
        for op in _gen_named_binary_ops {
            if !_gen_type_matches_op_proc(v.type, op) do continue
            gen_printf(g, "static {0} {1}_{2}({0} a, {0} b);\n", type.cname, type.cname_lower, op.name)
        }
        for op in _gen_named_unary_ops {
            if !_gen_type_matches_op_proc(v.type, op) do continue
        }
        gen_printf(g, "static {} {}_{}({} a);\n", type.cname, type.cname_lower, "broadcast", v.type.cname)
    }
}

gen_type_procs_defs :: proc(g: ^Gen, type: ^Type) {
    #partial switch v in type.variant {
    case Type_Array:
        for op in _gen_named_binary_ops {
            if !_gen_type_matches_op_proc(v.type, op) do continue
            func := fmt.tprintf("static {0} {1}_{2}({0} a, {0} b) {{", type.cname, type.cname_lower, op.name)

            switch v.kind {
            case .Fixed_Array:
                gen_print(g, func)
                gen_print(g, "\n")
                gen_printf(g, "\t{0} result;\n", type.cname)
                for i in 0..<v.len {
                    gen_printf(g, "\tresult.data[{0}] = a.data[{0}] {1} b.data[{0}];\n", i, _token_str[op.op])
                }
                gen_printf(g, "\treturn result;\n")
                gen_printf(g, "}}\n")

            case .Vector:
                intrinsic := ""

                basic := v.type.variant.(Type_Basic)

                #partial switch op.op {
                case .Equal:
                case .Less_Than:
                case .Less_Than_Equal:
                case .Greater_Than:
                case .Greater_Than_Equal:
                case .Not_Equal:

                case .Add:
                    switch v.len {
                    case 8:
                        switch basic.kind {
                        case .B8 , .I8 , .U8    : intrinsic = "_mm256_add_epi8"
                        case .B16, .I16, .U16   : intrinsic = "_mm256_add_epi16"
                        case .B32, .I32, .U32   : intrinsic = "_mm256_add_epi32"
                        case .B64, .I64, .U64   : intrinsic = "_mm256_add_epi64"
                        case .F32               : intrinsic = "_mm256_add_ps"
                        case .F64               : intrinsic = "_mm256_add_pd"
                        }
                    }

                case .Sub:
                    switch v.len {
                    case 8:
                        switch basic.kind {
                        case .B8 , .I8 , .U8    : intrinsic = "_mm256_sub_epi8"
                        case .B16, .I16, .U16   : intrinsic = "_mm256_sub_epi16"
                        case .B32, .I32, .U32   : intrinsic = "_mm256_sub_epi32"
                        case .B64, .I64, .U64   : intrinsic = "_mm256_sub_epi64"
                        case .F32               : intrinsic = "_mm256_sub_ps"
                        case .F64               : intrinsic = "_mm256_sub_pd"
                        }
                    }

                case .Mul:
                    switch v.len {
                    case 8:
                        switch basic.kind {
                        case .B8 , .I8 , .U8    : intrinsic = "_mm256_mul_epi8"
                        case .B16, .I16, .U16   : intrinsic = "_mm256_mul_epi16"
                        case .B32, .I32, .U32   : intrinsic = "_mm256_mul_epi32"
                        case .B64, .I64, .U64   : intrinsic = "_mm256_mul_epi64"
                        case .F32               : intrinsic = "_mm256_mul_ps"
                        case .F64               : intrinsic = "_mm256_mul_pd"
                        }
                    }

                case .Div:
                    switch v.len {
                    case 8:
                        switch basic.kind {
                        case .B8 , .I8 , .U8 ,
                             .B16, .I16, .U16,
                             .B32, .I32, .U32,
                             .B64, .I64, .U64   :

                        case .F32: intrinsic = "_mm256_div_ps"
                        case .F64: intrinsic = "_mm256_div_pd"
                        }
                    }

                case .Mod:
                    // unimplemented("fuck modulo")



                case .Bit_And:
                    switch v.len {
                    case 8:
                        switch basic.kind {
                        case .B8 , .I8 , .U8 ,
                             .B16, .I16, .U16,
                             .B32, .I32, .U32,
                             .B64, .I64, .U64   : intrinsic = "_mm256_and_si256"
                        case .F32               : intrinsic = "_mm256_and_ps"
                        case .F64               : intrinsic = "_mm256_and_pd"
                        }
                    }

                case .Bit_Or:
                    switch v.len {
                    case 8:
                        switch basic.kind {
                        case .B8 , .I8 , .U8 ,
                             .B16, .I16, .U16,
                             .B32, .I32, .U32,
                             .B64, .I64, .U64   : intrinsic = "_mm256_or_si256"
                        case .F32               : intrinsic = "_mm256_or_ps"
                        case .F64               : intrinsic = "_mm256_or_pd"
                        }
                    }

                case .Bit_Xor:
                    switch v.len {
                    case 8:
                        switch basic.kind {
                        case .B8 , .I8 , .U8 ,
                             .B16, .I16, .U16,
                             .B32, .I32, .U32,
                             .B64, .I64, .U64   : intrinsic = "_mm256_xor_si256"
                        case .F32               : intrinsic = "_mm256_xor_ps"
                        case .F64               : intrinsic = "_mm256_xor_pd"
                        }
                    }



                case .Bit_Shift_Left:
                case .Bit_Shift_Right:
                }

                // fmt.println(op.op, v.len, basic.kind)
                // assert(intrinsic != "")

                if intrinsic == "" do break

                gen_print(g, func)
                gen_printf(g, " return {}(a, b);", intrinsic)
                gen_printf(g, " }}\n")
            }

        }

        // for op in _gen_named_unary_ops {
        //     if !_gen_type_matches_op_proc(v.type, op) do continue
        //     gen_printf(g, "static {0} {1}_{2}({0} a) {{\n", type.cname, type.cname_lower, op.name)
        //     for i in 0..<v.len {
        //         gen_printf(g, "\ta.data[{0}] = {1}a.data[{0}];\n", i, op.op)
        //     }
        //     gen_printf(g, "}}\n")
        // }

        gen_printf(g, "static {} {}_{}({} a) {{\n", type.cname, type.cname_lower, "broadcast", v.type.cname)

        switch v.kind {
        case .Vector:
            intrinsic := ""
            switch v.len {
            case 8:
                switch v.type.variant.(Type_Basic).kind {
                case .B8,  .I8,  .U8 : intrinsic = "_mm256_set1_epi8"
                case .B16, .I16, .U16: intrinsic = "_mm256_set1_epi16"
                case .B32, .I32, .U32: intrinsic = "_mm256_set1_epi32"
                case .B64, .I64, .U64: intrinsic = "_mm256_set1_epi64x"
                case .F32:  intrinsic = "_mm256_set1_ps"
                case .F64:  intrinsic = "_mm256_set1_pd"
                }
            }

            assert(intrinsic != "")

            gen_printf(g, "\treturn {}(a);\n", intrinsic)

        case .Fixed_Array:
            gen_printf(g, "\t{0} result;\n", type.cname)
            for i in 0..<v.len {
                gen_printf(g, "\tresult.data[{0}] = a;\n", i)
            }
        }

        gen_printf(g, "}}\n")
    }
}

gen_type_generate_cname :: proc(g: ^Gen, type: ^Type) -> string {
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
        switch v.kind {
        case .Vector:
            fmt.println("GENERATING CNAME:", type_to_string(type))
            elem := v.type.variant.(Type_Basic)

            backing: string
            switch v.len {
            case 8:
                #partial switch elem.kind {
                case .B8 : backing = "__m128i"
                case .B16: backing = "__m128i"
                case .B32: backing = "__m256i"
                case .I8 : backing = "__m128i"
                case .I16: backing = "__m128i"
                case .I32: backing = "__m256i"
                case .U8 : backing = "__m128i"
                case .U16: backing = "__m128i"
                case .U32: backing = "__m256i"
                case .F32: backing = "__m256"
                case:
                    assert(false)
                }
            }

            assert(backing != "")

            return backing

        case .Fixed_Array:
            name := gen_type_generate_cname(g, v.type)
            return fmt.tprintf("Aos{}_{}", v.len, name)
        }

    case Type_Pointer:
        name := gen_type_generate_cname(g, v.type)
        switch v.kind {
        case .Single, .Multi:
            return fmt.tprintf("{}*", name)
        }
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
    gen_print(g, " = ")

    op, op_ok := token_normalize_assign_op(stmt.op.kind)
    if op != .Assign && op_ok {
        gen_binary_op(g,
            left = stmt.left,
            right = stmt.right,
            op = op,
            top_level = true,
        )
    } else {
        gen_possible_auto_conv_expr(g, stmt.left, stmt.right)
    }
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
gen_type_entity_cnames_recursive :: proc(g: ^Gen, scope: ^Scope) {
    for name, ent in scope.entities {
        #partial switch v in ent.variant {
        case Entity_Struct:
            v.type.cname = name
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
        case Entity_Builtin:
        case:
            continue
        }
        append(&sorted_entities, Sorted_Entity{name = name, order_index = ent.order_index, entity = ent})
    }

    slice.sort_by(sorted_entities[:], proc(a, b: Sorted_Entity) -> bool {
        return a.order_index < b.order_index,
    })


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

    gen_type_entity_cnames_recursive(g, g.curr_file_scope)

    for type in g.types {
        type.cname = gen_type_generate_cname(g, type)
        type.cname_lower = strings.to_lower(type.cname)
    }

    for type in g.types {
        gen_type_decl(g, type)
        gen_type_procs_decls(g, type)
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

    for type in g.types {
        gen_type_procs_defs(g, type)
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

        case Entity_Builtin:
            gen_print(g, "const")
            gen_print(g, " ")
            gen_type(g, v.type)
            gen_print(g, " ")
            gen_print(g, sorted.name)
            gen_print(g, " = ")
            gen_value(g, v.value, v.type)
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