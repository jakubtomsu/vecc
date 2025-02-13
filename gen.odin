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
    basic_types:        [Type_Basic_Kind]^Type,
    curr_entity:        ^Entity,
    depth:              int,
    indent:             int,
    unique_counter:     int,
}

gen_indent :: proc(g: ^Gen) {
    ind := "\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t"
    fmt.sbprint(&g.source, ind[:clamp(g.indent, 0, len(ind) - 1)])
}

gen_begin_scope :: proc(g: ^Gen, scope: ^Scope, visible := true, msg := "") {
    assert(scope.parent == g.curr_scope)
    g.depth += 1
    g.curr_scope = scope
    if visible {
        g.indent += 1
        gen_printf(g, "{{{}\n", msg)
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

/*
Here is the general idea with masked scopes.
First is the pseudo code which is followed by what the generated code semantics roughly look like.

for i in 0..<100 {
    if a {
        x = 1
        if b {
            x = 2
            if c {
                break
                x = 10000 // unreachable, compile time error
            }
        } else {
            x = 3
            continue
        }
    }
}

mask0 := 0xffffffff

for i in 0..<100 {
    mask1 := mask0 & a
    {
        x = blend(x, 1, mask1)
        mask2 := mask1 & b
        {
            x = blend(x, 2, mask2)
            {
                mask3 := mask2 & c
                // break:
                // remove mask3 bits from all affected parent masks,
                // including the persistent loop mask
                mask2 &= ~mask3
                mask1 &= ~mask3
                mask0 &= ~mask3
            }
        }
        // mask4 := mask1 & ~b
        mask4 := mask1 & ~mask2
        {
            x = blend(x, 3, mask4)
            // continue:
            // remove mask4 bits from all affected parent masks,
            // except the persistent loop mask
            mask1 &= ~mask4
        }
    }
}

*/

gen_value :: proc(g: ^Gen, value: Value, type: ^Type) {
    switch v in value {
    case bool:
        #partial switch tv in type.variant {
        case Type_Basic:
            gen_printf(g, "{}_{}", type.cname_lower, v)

        case Type_Array:
            elem := type_elem_basic_type(type)
            data := gen_cast_op_data(g,
                type = type,
                value = elem,
                op = .Conv,
            )

            gen_print(g, data.prefix)
            if data.sep != "" {
                gen_type(g, type)
                gen_print(g, data.sep)
            }
            gen_printf(g, "{}_{}", elem.cname_lower, v)
            gen_print(g, data.suffix)

        case:
            assert(false)
        }

    case i128:
        #partial switch tv in type.variant {
        case Type_Basic:
            gen_print(g, v)

        case Type_Array:
            switch tv.kind {
            case .Vector:
                // HACK, use conversion code
                gen_printf(g, "v{}{}_set1(", tv.len, tv.type.cname_lower)
                gen_print(g, v)
                gen_print(g, ")")

            case .Fixed_Array:
                unimplemented()
            }

        case:
            assert(false)
        }

    case f64:
        #partial switch tv in type.variant {
        case Type_Basic:
            _print_float(g, tv.kind, v)

        case Type_Array:
            elem := type_elem_basic_type(type)
            data := gen_cast_op_data(g,
                type = type,
                value = elem,
                op = .Conv,
            )

            gen_print(g, data.prefix)
            if data.sep != "" {
                gen_type(g, type)
                gen_print(g, data.sep)
            }
            _print_float(g, elem.variant.(Type_Basic).kind, v)
            gen_print(g, data.suffix)

        case:
            assert(false)
        }

    // HACK

    case [8]i32:
        gen_printf(g, "v8i32_set(%i, %i, %i, %i, %i, %i, %i, %i)",
            v[0],
            v[1],
            v[2],
            v[3],
            v[4],
            v[5],
            v[6],
            v[7],
        )

    case [8]f32:
        gen_printf(g, "v8f32_set(%i, %i, %i, %i, %i, %i, %i, %i)",
            v[0],
            v[1],
            v[2],
            v[3],
            v[4],
            v[5],
            v[6],
            v[7],
        )

    case nil:
        assert(false)
    }

    _print_float :: proc(g: ^Gen, basic: Type_Basic_Kind, v: f64) {
        #partial switch basic {
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
    }
}

gen_basic_literal :: proc(g: ^Gen, ast: ^Ast) {
    lit := ast.variant.(Ast_Basic_Literal)
    gen_value(g, ast.value, ast.type)
}

gen_compound_literal :: proc(g: ^Gen, ast: ^Ast) {
    lit := ast.variant.(Ast_Compound_Literal)

    assert(ast.type != nil)

    gen_printf(g, "{}_set(", ast.type.cname_lower)

    for elem, i in lit.elems {
        gen_expr(g, elem, top_level = true)
        if i + 1 < len(lit.elems) {
            gen_print(g, ", ")
        }
    }

    gen_print(g, ")")
}

gen_ident :: proc(g: ^Gen, ast: ^Ast) {
    ident := ast.variant.(Ast_Ident)
    gen_print(g, ident.token.text)
}

gen_call_expr :: proc(g: ^Gen, ast: ^Ast) {
    call_expr := ast.variant.(Ast_Call_Expr)

    #partial switch v in call_expr.entity.variant {
    case Entity_Proc:
        gen_print(g, call_expr.procedure.variant.(Ast_Ident).token.text)

    case Entity_Builtin:
        gen_printf(g, "{}_{}", call_expr.args[0].type.cname_lower, strings.to_lower(fmt.tprint(v.kind)))
    }

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

    prefix := ""
    suffix := ""
    #partial switch v in ast.type.variant {
    case Type_Basic:
        #partial switch expr.op.kind {
        case .Sub:
            prefix = "-"

        case .Not:
            prefix = "!"

        case:
            assert(false)
        }

    case:
        op_name := ""
        #partial switch expr.op.kind {
        case .Sub: op_name = "neg"
        case .Not: op_name = "not"
        case:
            assert(false)
        }

        prefix = fmt.tprintf("{}_{}(", ast.type.cname_lower, op_name)
        suffix = ")"
    }

    gen_print(g, prefix)
    gen_expr(g, expr.expr)
    gen_print(g, suffix)
}

gen_cast_expr :: proc(g: ^Gen, ast: ^Ast) {
    expr := ast.variant.(Ast_Cast_Expr)
    gen_cast_op(g, expr.type.type, expr.value, expr.op.kind)
}

// TODO: auto cast op
gen_cast_op :: proc(g: ^Gen, type: ^Type, value: ^Ast, op: Token_Kind) {
    data := gen_cast_op_data(g,
        type = type,
        value = value.type,
        op = op,
    )

    gen_print(g, data.prefix)
    if data.sep != "" {
        gen_type(g, type)
        gen_print(g, data.sep)
    }
    gen_expr(g, value)
    gen_print(g, data.suffix)
}

gen_cast_op_data :: proc(
    g:      ^Gen,
    type:   ^Type,
    value:  ^Type,
    op:     Token_Kind,
) -> (result: Gen_Op_Data) {
    if type == value {
        return {}
    }

    fmt.println("CAST OP:", type_to_string(type), "<-", type_to_string(value))

    #partial switch op {
    case .Reinterpret:
        result.prefix = "(*("
        result.sep = "*)&"
        result.suffix = ")"

    case .Conv:
        #partial switch v in value.variant {
        case Type_Basic:
            #partial switch tv in type.variant {
            case Type_Basic:
                if v.kind == tv.kind {
                    return {}
                }

                result.prefix = "("
                result.sep = ")"

            case Type_Array:
                sub: Gen_Op_Data
                fmt.print("\t")
                sub = gen_cast_op_data(g,
                    type = tv.type,
                    value = value,
                    op = op,
                )

                result.prefix = fmt.tprintf("{}_set1({}", type.cname_lower, sub.prefix)

                result.sep = sub.sep

                result.suffix = fmt.tprintf("{})", sub.suffix)
            }


        case Type_Array:
            dst := type.variant.(Type_Basic)
            src := v.type.variant.(Type_Basic)

            // HACK
            prefix := ""
            switch v.kind {
            case .Vector: prefix = "v"
            case .Fixed_Array: prefix = "aos"
            }

            result.prefix = fmt.tprintf("{}_to_{}{}{}(",
                value.cname_lower, prefix, v.len, type.cname_lower)
            result.suffix = ")"
        }
    }

    return result
}


gen_expr :: proc(g: ^Gen, ast: ^Ast, top_level := false) {
    #partial switch v in ast.variant {
    case Ast_Ident:
        gen_ident(g, ast)

    case Ast_Basic_Literal:
        gen_basic_literal(g, ast)

    case Ast_Compound_Literal:
        gen_compound_literal(g, ast)

    case Ast_Call_Expr:
        gen_call_expr(g, ast)

    case Ast_Cast_Expr:
        gen_cast_expr(g, ast)

    case Ast_Unary_Expr:
        gen_urnary_expr(g, ast)

    case Ast_Binary_Expr:
        gen_binary_expr(g, ast)

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

gen_find_root_assignable_expr_entity :: proc(scope: ^Scope, ast: ^Ast) -> (^Entity, ^Scope, bool) {
    #partial switch v in ast.variant {
    case Ast_Ident:
        return find_entity(scope, v.token.text)

    // case Ast_Call_Expr:

    case Ast_Selector_Expr:
        return gen_find_root_assignable_expr_entity(scope, v.left)

    case Ast_Index_Expr:
        return gen_find_root_assignable_expr_entity(scope, v.left)

    case Ast_Address_Expr:
        return gen_find_root_assignable_expr_entity(scope, v.expr)

    case Ast_Deref_Expr:
        return gen_find_root_assignable_expr_entity(scope, v.expr)
    }

    return nil, nil, false
}

// Left is assumed to be already checked
// THIS CONVERSION THING IS A HACK
// TODO: refactor to use gen_cast_op_data
gen_possible_auto_conv_expr :: proc(g: ^Gen, type: ^Type, expr: ^Ast, top_level: bool) {
    conv_prefix := ""
    conv_suffix := ""
    // this also does not spark joy
    #partial switch lv in type.variant {
    case Type_Array:

        #partial switch lvt in lv.type.variant {
        case Type_Basic:
            #partial switch lr in expr.type.variant {
            case Type_Basic:
                if lv.type == expr.type {
                    conv_prefix = fmt.tprintf("{}_set1(", type.cname_lower)
                    conv_suffix = ")"
                }
            }

        case Type_Array:
            if lvt.kind != .Vector do break

            #partial switch lr in expr.type.variant {
            case Type_Basic:
                if lvt.type == expr.type {
                    // conv_prefix = fmt.tprintf("{}_set1({}_set1(", type.cname_lower, lv.type.cname_lower)
                    // conv_suffix = "))"
                    conv := gen_cast_op_data(g,
                        type = type,
                        value = expr.type,
                        op = .Conv,
                    )
                    conv_prefix = conv.prefix
                    conv_suffix = conv.suffix
                }

            case Type_Array:
                switch lr.kind {
                case .Vector:
                    if lvt.type == lr.type {
                        conv_prefix = fmt.tprintf("{}_set1(", type.cname_lower)
                        conv_suffix = ")"
                    }

                case .Fixed_Array:
                    if lvt.type == lr.type {
                        conv_prefix = fmt.tprintf("{}_set_scalar(", type.cname_lower)
                        conv_suffix = ")"
                    }
                }
            }
        }

        // #partial switch lr in expr.type.variant {
        // case Type_Basic:
        //     if lv.type == expr.type {
        //         gen_printf(g, "{}_set1(", type.cname_lower)
        //         emitted_conv = true
        //     }
        // }
    }

    gen_print(g, conv_prefix)
    gen_expr(g, expr, top_level = (conv_prefix != "") || top_level)
    gen_print(g, conv_suffix)
}

gen_binary_expr :: proc(g: ^Gen, ast: ^Ast, top_level := false) {
    expr := ast.variant.(Ast_Binary_Expr)

    gen_binary_op(g,
        type = expr.left.type, // HACK
        left = expr.left,
        right = expr.right,
        op = expr.op.kind,
        top_level = top_level,
    )
}

gen_binary_op :: proc(
    g:          ^Gen,
    type:       ^Type,
    left:       ^Ast,
    right:      ^Ast,
    op:         Token_Kind,
    top_level   := false,
) {
    data := gen_binary_op_data(
        type = type,
        left = left.type,
        right = right.type,
        op = op,
        top_level = top_level,
    )

    assert(data != {})

    gen_print(g, data.prefix)
    gen_possible_auto_conv_expr(g, type, left, top_level = top_level)
    gen_print(g, data.sep)
    gen_possible_auto_conv_expr(g, type, right, top_level = top_level)
    gen_print(g, data.suffix)
}

Gen_Op_Data :: struct {
    prefix: string,
    sep:    string,
    suffix: string,
}

gen_binary_op_data :: proc(
    type:       ^Type,
    left:       ^Type,
    right:      ^Type,
    op:         Token_Kind,
    top_level:  bool,
) -> (result: Gen_Op_Data) {
    #partial switch v in type.variant {
    case Type_Basic:
        if !top_level {
            result.prefix = "("
            result.suffix = ")"
        }

        op_name := _token_str[op]
        result.sep = fmt.tprintf(" {} ", op_name)

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
        case .Bit_Shift_Left:       op_name = "sl"
        case .Bit_Shift_Right:      op_name = "sr"
        }

        result.prefix = fmt.tprintf("%s_%s(", left.cname_lower, op_name)
        result.sep = ", "
        result.suffix = ")"

    case:
        unimplemented()
    }

    return result
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
    #partial switch v in expr.left.type.variant {
    case Type_Array:
        switch v.kind {
        case .Vector:
            index := 0
            // switch val in expr.index.value {
            // case i128:
            //     index = int(val)
            // case:
            //     panic("currently the index has to be a constant")
            // }


            op_name := ""

            assert(op_name != "")


        case .Fixed_Array:
            gen_expr(g, expr.left)
            gen_print(g, "[")
            gen_expr(g, expr.index)
            gen_print(g, "]")
        }

    case Type_Pointer:
        gen_expr(g, expr.left)
        gen_print(g, "[")
        gen_expr(g, expr.index)
        gen_print(g, "]")

    case:
        assert(false)
    }
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
            // None, defined in builtin

        case .Fixed_Array:
            fmt.println(type_to_string(type), ":", type_to_string(v.type))
            assert(v.type.cname != "")
            gen_printf(g, "typedef struct {{ ")
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
    {name = "div", op = .Div,                flags = {.Float}},
    {name = "mod", op = .Mod,                flags = {}},
    {name = "and", op = .Bit_And,            flags = {.Integer}},
    {name = "or" , op = .Bit_Or,             flags = {.Integer}},
    {name = "xor", op = .Bit_Xor,            flags = {.Integer}},
    // {name = "sl", op = .Bit_Shift_Left,     flags = {.Integer}},
    // {name = "sr", op = .Bit_Shift_Right,    flags = {.Integer}},
}

@(rodata)
_gen_named_unary_ops := [?]Gen_Op_Proc{
    {name = "not", op = .Not, flags = {.Boolean}},
    {name = "neg", op = .Sub, flags = {.Integer, .Float}},
}

_gen_type_matches_op_proc :: proc(type: ^Type, op: Gen_Op_Proc) -> bool {
    if      type_is_integer(type) && .Integer in op.flags do return true
    else if type_is_float  (type) && .Float   in op.flags do return true
    else if type_is_boolean(type) && .Boolean in op.flags do return true
    return false
}

// TEMP just so we don't generate arithmetic procs for big fixed array buffers etc
MAX_ARITMETIC_ARRAY_WIDTH :: 64

// TODO: scalar 2nd arg versions?
gen_type_procs :: proc(g: ^Gen, type: ^Type, defs := false) {
    #partial switch v in type.variant {
    case Type_Array:
        if v.kind != .Fixed_Array do return
        if v.len > MAX_ARITMETIC_ARRAY_WIDTH do return

        #partial switch vt in v.type.variant {
        case Type_Basic:
        case Type_Array:
            if vt.kind != .Vector {
                return
            }

            gen_printf(g, "static {} {}_set_scalar(Aos{}{} a)",
                type.cname, type.cname_lower, v.len, vt.type.cname)
            if defs {
                gen_print(g, " { ")
                gen_print(g, "return {{", )
                dst_elem := v.type
                for i in 0..<v.len {
                    data := gen_cast_op_data(g,
                        type = dst_elem,
                        value = vt.type,
                        op = .Conv,
                    )

                    gen_print(g, data.prefix)
                    if data.sep != "" {
                        gen_type(g, dst_elem)
                        gen_print(g, data.sep)
                    }

                    gen_printf(g, "a.data[%i]", i)
                    gen_print(g, data.suffix)

                    if i + 1 < v.len {
                        gen_print(g, ", ")
                    }
                }
                gen_print(g, "}};")
                gen_print(g, " }\n")
            } else {
                gen_print(g, ";\n")
            }

        case:
            return
        }

        gen_printf(g, "static {} {}_set1({} a)", type.cname, type.cname_lower, v.type.cname)
        if defs {
            gen_print(g, " { ")
            gen_print(g, "return {{", )
            for i in 0..<v.len {
                gen_printf(g, "a")
                if i + 1 < v.len {
                    gen_print(g, ", ")
                }
            }
            gen_print(g, "}};")
            gen_print(g, " }\n")
        } else {
            gen_print(g, ";\n")
        }

        gen_printf(g, "static {} {}_set(", type.cname, type.cname_lower)
        for i in 0..<v.len {
            gen_printf(g, "{} v{}", v.type.cname, i)
            if i + 1 < v.len {
                gen_print(g, ", ")
            }
        }
        gen_printf(g, ")")
        if defs {
            gen_print(g, " { ")
            gen_print(g, "return {{", )
            for i in 0..<v.len {
                gen_printf(g, "v%i", i)
                if i + 1 < v.len {
                    gen_print(g, ", ")
                }
            }
            gen_print(g, "}};")
            gen_print(g, " }\n")
        } else {
            gen_print(g, ";\n")
        }

        // THIS IS VERY SILLY
        // Generating all possible conversion procs is not fun and it generates a shit ton of code.
        // Alternative is remembering which ones get used during type checking? I don't like that either tho.
        if basic, ok := v.type.variant.(Type_Basic); ok {
            numeric := type_is_numeric(v.type)
            for kind in Type_Basic_Kind {
                if kind == basic.kind {
                    continue
                }

                dst_elem := g.basic_types[kind]

                if v.type.size != dst_elem.size {
                    continue
                }

                if numeric != type_is_numeric(dst_elem) {
                    continue
                }

                // HACK
                dst_type := type_clone(type)
                dst_arr := &dst_type.variant.(Type_Array)
                dst_arr.type = dst_elem
                dst_type = gen_find_type(g, dst_type)
                if dst_type == nil {
                    continue
                }

                gen_printf(g, "static Aos{2}{3} {1}_to_aos{2}{3}({0} a)",
                    type.cname, type.cname_lower, v.len, dst_elem.cname_lower)
                if defs {
                    gen_print(g, " { ")
                    gen_print(g, "return {{", )
                    for i in 0..<v.len {
                        data := gen_cast_op_data(g,
                            type = dst_elem,
                            value = v.type,
                            op = .Conv,
                        )
                        gen_print(g, data.prefix)
                        if data.sep != "" {
                            gen_type(g, dst_elem)
                            gen_print(g, data.sep)
                        }
                        gen_printf(g, "a.data[%i]", i)
                        gen_print(g, data.suffix)

                        if i + 1 < v.len {
                            gen_print(g, ", ")
                        }
                    }
                    gen_print(g, "}};")
                    gen_print(g, " }\n")
                } else {
                    gen_printf(g, ";\n")
                }
            }
        }

        for op in _gen_named_binary_ops {
            if !_gen_type_matches_op_proc(type_elem_basic_type(v.type), op) do continue
            gen_printf(g, "static {0} {1}_{2}({0} a, {0} b)", type.cname, type.cname_lower, op.name)
            if defs {
                gen_printf(g, " {{")
                gen_printf(g, "return {{{{")

                data := gen_binary_op_data(
                    type = v.type,
                    left = v.type,
                    right = v.type,
                    op = op.op,
                    top_level = true,
                )

                for i in 0..<v.len {
                    // gen_printf(g, "\t\t")
                    gen_print(g, data.prefix)
                    gen_printf(g, "a.data[{0}]", i)
                    gen_print(g, data.sep)
                    gen_printf(g, "b.data[{0}]", i)
                    gen_print(g, data.suffix)
                    gen_printf(g, ", ")
                }

                gen_printf(g, "}}}};")
                gen_printf(g, "}}\n")
            } else {
                gen_printf(g, ";\n")
            }
        }

        for op in _gen_named_unary_ops {
            if !_gen_type_matches_op_proc(v.type, op) do continue
            gen_printf(g, "static {0} {1}_{2}({0} a)", type.cname, type.cname_lower, op.name)
            if defs {
                gen_print(g, " { return {{")
                for i in 0..<v.len {
                    gen_printf(g, "{1}a.data[{0}]", i, _token_str[op.op])
                    if i + 1 < v.len {
                        gen_print(g, ", ")
                    }
                }
                gen_print(g, "}}; }\n")
            } else {
                gen_print(g, ";\n")
            }
        }
    }
}

gen_type_generate_cname :: proc(g: ^Gen, type: ^Type) -> string {
    if type.cname != "" {
        return type.cname
    }

    #partial switch v in type.variant {
    case Type_Basic:
        switch v.kind {
        case .B8:   return "b8"
        case .B16:  return "b16"
        case .B32:  return "b32"
        case .B64:  return "b64"
        case .I8:   return "i8"
        case .I16:  return "i16"
        case .I32:  return "i32"
        case .I64:  return "i64"
        case .U8:   return "u8"
        case .U16:  return "u16"
        case .U32:  return "u32"
        case .U64:  return "u64"
        case .F32:  return "f32"
        case .F64:  return "f64"
        }

    case Type_Array:
        switch v.kind {
        case .Vector:
            return fmt.tprintf("v{}{}", v.len, gen_type_generate_cname(g, v.type))

        case .Fixed_Array:
            name := gen_type_generate_cname(g, v.type)
            return fmt.tprintf("Aos{}{}", v.len, name)
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
        gen_possible_auto_conv_expr(g, decl.type.type, decl.value, top_level = true)
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

gen_block_stmt :: proc(g: ^Gen, ast: ^Ast, scope := true) {
    stmt := ast.variant.(Ast_Block_Stmt)

    if scope {
        gen_begin_scope(g, stmt.scope)
    }

    for s in stmt.statements {
        #partial switch _ in s.variant {
        case Ast_Proc_Decl, Ast_Struct_Decl:
        case:
            gen_indent(g)
            gen_stmt(g, s)
            gen_print(g, ";\n")
        }
    }

    if scope {
        gen_end_scope(g)
    }
}

gen_if_stmt :: proc(g: ^Gen, ast: ^Ast) {
    stmt := ast.variant.(Ast_If_Stmt)

    #partial switch v in stmt.cond.type.variant {
    case Type_Basic:
        gen_print(g, "if (")
        gen_expr(g, stmt.cond, top_level = true)
        gen_print(g, ") ")
        gen_block_stmt(g, stmt.if_body)
        if stmt.else_body != nil {
            gen_print(g, " else ")
            gen_block_stmt(g, stmt.else_body)
        }

    case Type_Array:
        assert(v.kind == .Vector)

        if_block := stmt.if_body.variant.(Ast_Block_Stmt)
        {

            assert(.Masked in if_block.scope.flags)
            assert(if_block.scope.vector_width > 1)

            parent_masked_id := -1
            for s := if_block.scope.parent; s != nil; s = s.parent {
                if .Masked in s.flags {
                    parent_masked_id = s.local_id
                }
            }
            if parent_masked_id == -1 {
                gen_printf(g,
                    "v{1}{2} vecc_mask{0} = ",
                    if_block.scope.local_id,
                    v.len,
                    v.type.cname_lower,
                )
                gen_expr(g, stmt.cond, top_level = true)
                gen_print(g, "; ")
            } else {
                gen_printf(g,
                    "v{1}{2} vecc_mask{0} = v{1}{2}_and(vecc_mask{3}, ",
                    if_block.scope.local_id,
                    v.len,
                    v.type.cname_lower,
                    parent_masked_id,
                )
                gen_expr(g, stmt.cond, top_level = true)
                gen_print(g, "); ")
            }

            gen_begin_scope(g, if_block.scope, msg = " // vector if")

            gen_block_stmt(g, stmt.if_body, scope = false)

            gen_end_scope(g)
        }

        if stmt.else_body != nil {
            block := stmt.else_body.variant.(Ast_Block_Stmt)

            assert(.Masked in block.scope.flags)
            assert(block.scope.vector_width > 1)

            parent_masked_id := -1
            for s := block.scope.parent; s != nil; s = s.parent {
                if .Masked in s.flags {
                    parent_masked_id = s.local_id
                }
            }
            if parent_masked_id == -1 {
                gen_printf(g,
                    "v{1}{2} vecc_mask{0} = v{1}{2}_not(vecc_mask{3}); ",
                    block.scope.local_id,
                    v.len,
                    v.type.cname_lower,
                    if_block.scope.local_id,
                )
            } else {
                gen_printf(g,
                    "v{1}{2} vecc_mask{0} = v{1}{2}_andnot(vecc_mask{3}, vecc_mask{4}); ",
                    block.scope.local_id,
                    v.len,
                    v.type.cname_lower,
                    parent_masked_id,
                    if_block.scope.local_id,
                )
            }

            gen_begin_scope(g, block.scope, msg = " // vector else")

            gen_block_stmt(g, stmt.else_body, scope = false)

            gen_end_scope(g)
        }

    case:
        assert(false)
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

    suffix := ""
    #partial switch v in  stmt.left.type.variant {
    case Type_Array:
        masked_id := -1
        masked_scope := g.curr_scope
        for ; masked_scope != nil; masked_scope = masked_scope.parent {
            if .Masked in masked_scope.flags {
                masked_id = masked_scope.local_id
                break
            }
        }

        if masked_id == -1 {
            break
        }

        root_entity, root_scope, ok := gen_find_root_assignable_expr_entity(g.curr_scope, stmt.left)
        assert(ok)

        is_root_parent := root_scope.depth < g.curr_scope.depth

        if is_root_parent {
            gen_printf(g, "v{}{}_blend(", v.len, v.type.cname_lower)
            gen_expr(g, stmt.left)
            gen_print(g, ", ")
            suffix = fmt.tprintf(", vecc_mask{})", masked_id)
        }
    }

    op, op_ok := token_normalize_assign_op(stmt.op.kind)
    if op != .Assign && op_ok {
        gen_binary_op(g,
            type = ast.type,
            left = stmt.left,
            right = stmt.right,
            op = op,
            top_level = true,
        )
    } else {
        gen_possible_auto_conv_expr(g, stmt.left.type, stmt.right, top_level = true)
    }

    gen_print(g, suffix)

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

// HACK
// releated to find_or_create_type in checker
gen_find_type :: proc(g: ^Gen, type: ^Type) -> ^Type {
    switch &v in type.variant {
    case Type_Basic:
        return g.basic_types[v.kind]

    case Type_Array:
        v.type = gen_find_type(g, v.type)

    case Type_Pointer:
        v.type = gen_find_type(g, v.type)

    case Type_Struct:
        for &field in v.fields {
            field.type = gen_find_type(g, field.type)
        }
    }

    // Linear search, bit dumb. Could hash some type info to split into buckets.
    for t in g.types {
        if types_equal(t, type) {
            return t
        }
    }

    return nil
}

gen_program :: proc(g: ^Gen) {
    gen_print(g, "#ifndef VECC_DEFINED\n")
    gen_print(g, "#define VECC_DEFINED 1\n")
    gen_print(g, "\n")

    gen_print(g, "// WARNING: this file was generated by the VecC compiler.\n")
    gen_print(g, "\n")

    gen_print(g, "#include <stdint.h>\n")
    gen_print(g, "#include <stdio.h>\n")
    gen_print(g, "#include \"vecc_builtin.h\"\n")
    gen_print(g, "\n")

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

    gen_type_entity_cnames_recursive(g, g.curr_file_scope)

    for type in g.types {
        type.cname = gen_type_generate_cname(g, type)
        type.cname_lower = strings.to_lower(type.cname)
    }

    for type in g.types {
        gen_type_decl(g, type)
    }
    for type in g.types {
        gen_type_procs(g, type, defs = true)
    }

    gen_print(g, "\n// VECC exported constants\n\n")

    for sorted in sorted_entities {
        #partial switch v in sorted.entity.variant {
        case Entity_Variable:
            decl := sorted.entity.ast.variant.(Ast_Value_Decl)
            if !decl.export do continue
            gen_value_decl(g, sorted.entity.ast)
            gen_print(g, ";\n")
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

    // for type in g.types {
    //     gen_type_procs(g, type, defs = true)
    // }

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
            if decl.export do continue
            gen_value_decl(g, sorted.entity.ast)
            gen_print(g, ";\n")

        case Entity_Builtin:
            if v.type != nil && v.value != nil {
                // gen_print(g, "const")
                // gen_print(g, " ")
                // gen_type(g, v.type)
                // gen_print(g, " ")
                // gen_print(g, sorted.name)
                // gen_print(g, " = ")
                // gen_value(g, v.value, v.type)
                // gen_print(g, ";\n")
            }
        }
    }

    gen_print(g, "\n// VECC function definitions\n\n")

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

    gen_print(g, "#endif // VECC_IMPL\n")
}