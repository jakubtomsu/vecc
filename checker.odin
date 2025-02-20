// Assign types to AST nodes, resolve identifiers.
package vecc

import "core:fmt"
import "core:strings"
import "core:strconv"
import "core:slice"
import "core:os"
import "core:reflect"
import "core:unicode/utf8"

Checker :: struct {
    filename:           string,
    curr_scope:         ^Scope,
    curr_lanes:         int,
    curr_entity:        ^Entity,
    curr_file_scope:    ^Scope,
    basic_types:        [Type_Basic_Kind]^Type,
    types:              [dynamic]^Type,
}

Entity :: struct {
    ast:            ^Ast,
    name:           string,
    order_index:    int,
    variant:        Entity_Variant,
    // TODO
    depends:        map[string]struct{},
    checked:        bool,
}

Entity_Variant :: union {
    Entity_Proc,
    Entity_Variable,
    Entity_Struct,
    Entity_Struct_Field,
    Entity_Builtin,
}

Entity_Proc :: struct {

}

Entity_Variable :: struct {

}

Entity_Builtin :: struct {
    kind:   Entity_Builtin_Kind,
    type:   ^Type,
    value:  Value,
}

Entity_Builtin_Kind :: enum u8 {
    // Values
    Vector_Width,
    Vector_Index,
    Vector_Mask,
    // Procs
    Print,
    Println,
    Min,
    Max,
    Clamp,
    Abs,
    Sign,
    Blend,
    Trunc,
    Floor,
    Round,
    Ceil,
    Fract,
    Rcp,
    Sqrt,
    RSqrt,
    Sin,
    Cos,
    Tan,
    Pow,
    Exp,
    Exp2,
    Log,
    Log2,
    Reduce_Add,
    Reduce_Mul,
    Reduce_And,
    Reduce_All,
    Reduce_Any,
    To_Bitmask,
    To_Vector,
    To_Scalar,
    To_String,
    Count_Leading_Zeros,
    Count_Trailing_Zeros,
}

Entity_Struct :: struct {
    type:   ^Type,
}

Entity_Struct_Field :: struct {
}

Entity_Alias :: struct {
    entity: ^Entity,
}

Scope :: struct {
    ast:            ^Ast,
    parent:         ^Scope,
    flags:          bit_set[Scope_Flag],
    vector_width:   int,
    local_id:       int,
    children:       [dynamic]^Scope,
    depth:          int,
    entities:       map[string]^Entity,
}

Scope_Flag :: enum u8 {
    Conditional,
    Loop,
    Masked,
    Global,
}

checker_error :: proc(c: ^Checker, pos: Pos, format: string, args: ..any, loc := #caller_location) -> ! {
    fmt.eprintf("[{}] %s(%d:%d) ", loc, c.filename, pos.line, pos.column)
	fmt.eprintf(format, ..args)
	fmt.eprintln()
	os.exit(1)
}

check_ident :: proc(c: ^Checker, ast: ^Ast) -> ^Entity {
    ident := ast.variant.(Ast_Ident)
    name := ident.token.text

    if ent, scope, ok := find_entity(c.curr_scope, name); ok {
        if !ent.checked {
            check_entity(c, ent)
        }

        #partial switch v in ent.variant {
        case Entity_Builtin:
            ast.type = v.type
            ast.value = v.value

        case:
            ast.type = ent.ast.type
            ast.value = ent.ast.value
            if scope.parent != nil && ast.order_index < ent.ast.order_index {
                checker_error(c, ast.token, "Entity is used before declaration: {}", name)
            }
        }

        return ent
    }

    checker_error(c, ast.token, "Ident not found: {}", name)
}

check_type :: proc(c: ^Checker, ast: ^Ast) {
    if ast.type != nil {
        return
    }

    #partial switch v in ast.variant {
    case Ast_Array_Type:
        check_expr(c, v.len)
        check_type(c, v.type)

        #partial switch l in v.len.value {
        case i128:
            assert(l > 0)
            if v.kind == .Vector {
                assert(l <= 64)
            }
        case:
            checker_error(c, ast.token,
                "Array length has to be a constant integer, got {}",
                reflect.union_variant_typeid(v.len.value),
            )
        }

    case Ast_Struct_Type:
        for field in v.fields {
            field := field.variant.(Ast_Field)
            check_type(c, field.type)
        }

    case Ast_Pointer_Type:
        check_type(c, v.type)

    case Ast_Multi_Pointer_Type:
        check_type(c, v.type)
    }

    ast.type = find_or_create_type_ast(c, ast)
}

check_basic_literal :: proc(c: ^Checker, ast: ^Ast, type_hint: ^Type = nil) {
    assert(ast.type == nil)

    lit := ast.variant.(Ast_Basic_Literal)
    ast.type = find_or_create_type_ast(c, ast, type_hint = type_hint)

    #partial switch v in ast.type.variant {
    case Type_Basic:
        ast.value = _to_value(c, ast.token, v.kind, lit)

    case Type_Array:
        basic := type_elem_basic_type(v.type)
        if basic == nil do break
        ast.value = _to_value(c, ast.token, basic.variant.(Type_Basic).kind, lit)

    case:
        assert(false)
    }

    return

    _to_value :: proc(c: ^Checker, pos: Pos, basic: Type_Basic_Kind, lit: Ast_Basic_Literal) -> Value {
        switch basic {
        case .Invalid:
            assert(false)

        case .B8,
             .B16,
             .B32,
             .B64:
            #partial switch lit.token.kind {
            case .True:
                return true
            case .False:
                return false
            case:
                checker_error(c, pos, "Invalid boolean literal")
            }

        case .I8,
             .I16,
             .I32,
             .I64,
             .U8,
             .U16,
             .U32,
             .U64:
            #partial switch lit.token.kind {
            case .Integer:
                val, ok := strconv.parse_i128_maybe_prefixed(lit.token.text)
                assert(ok)
                return i128(val)

            case .Char:
                return i128(lit.token.text[1])
                // ch, _, _, _ := strconv.unquote_char(lit.token.text, '\'')
                // return i128(ch)
            }

        case .F32:
            val, ok := strconv.parse_f32(lit.token.text)
            assert(ok)
            return f64(val)

        case .F64:
            val, ok := strconv.parse_f64(lit.token.text)
            assert(ok)
            return f64(val)

        case .String:
            res, alloc, succ := strconv.unquote_string(lit.token.text)
            assert(succ)
            return res
        }
        return nil
    }
}

check_compound_literal :: proc(c: ^Checker, ast: ^Ast, type_hint: ^Type) {
    lit := ast.variant.(Ast_Compound_Literal)
    if lit.type != nil {
        check_type(c, lit.type)
        ast.type = lit.type.type
    } else {
        ast.type = type_hint

    }

    if ast.type == nil {
        checker_error(c, ast.token, "Invalid compound type literal, failed to infer type")
    }

    #partial switch v in ast.type.variant {
    case Type_Array:
        if len(lit.elems) != v.len {
            checker_error(c, ast.token,
                "Comopound literal contains wrong number of elements, expected {}, got {}",
                v.len,
                len(lit.elems)
            )
        }

    case Type_Struct:
        if len(lit.elems) != len(v.fields) {
            checker_error(c, ast.token,
                "Comopound literal contains wrong number of elements, expected {}, got {}",
                len(v.fields),
                len(lit.elems),
            )
        }

    case:
        checker_error(c, ast.token,
            "Comopound literal is not valid for {}, expected array or struct",
            type_to_string(ast.type),
        )
    }

    for elem, i in lit.elems {
        hint: ^Type
        #partial switch v in ast.type.variant {
        case Type_Array:
            hint = v.type
        case Type_Struct:
            hint = v.fields[i].type
        }

        check_expr(c, elem, hint)
    }
}

check_call_expr :: proc(c: ^Checker, ast: ^Ast) {
    assert(ast.type == nil)

    call_expr := &ast.variant.(Ast_Call_Expr)

    ent := check_ident(c, call_expr.procedure)
    call_expr.entity = ent

    #partial switch v in ent.variant {
    case Entity_Proc:
        proc_decl := ent.ast.variant.(Ast_Proc_Decl)
        proc_type := proc_decl.type.variant.(Ast_Proc_Type)

        if len(call_expr.args) != len(proc_type.params) {
            checker_error(c, ast.token,
                "Wrong number of call arguments, expected {}, got {}",
                len(proc_type.params),
                len(call_expr.args),
            )
        }

        for arg, i in call_expr.args {
            hint := proc_type.params[i].type
            check_expr(c, arg, hint)
        }

        for arg, i in call_expr.args {
            if arg.type != proc_type.params[i].type {
                checker_error(c, ast.token,
                    "Invalid argument type, expected {}, got {}",
                    type_to_string(proc_type.params[i].type),
                    type_to_string(arg.type),
                )
            }
        }

        if proc_type.result != nil {
            ast.type = proc_type.result.type
        }

    case Entity_Builtin:
        for arg, i in call_expr.args {
            check_expr(c, arg, type_hint = call_expr.args[0].type) // hint is a lil hack
        }

        #partial switch v.kind {
        case .Print, .Println:
            assert(len(call_expr.args) == 1)

        case .Min,
             .Max:
            assert(len(call_expr.args) == 2) // Allow also 3 params? or any number > 1?
            ast.type = call_expr.args[0].type
            for arg, i in call_expr.args {
                if ast.type != arg.type {
                    checker_error(c, ast.token,
                        "{} expected argument %i to be {}, got {}",
                        v.kind,
                        i,
                        type_to_string(ast.type),
                        type_to_string(arg.type),
                    )
                }
            }

        case .Clamp:
            assert(len(call_expr.args) == 3)
            ast.type = call_expr.args[0].type
            assert(type_is_numeric(type_elem_basic_type(ast.type)))
            for arg, i in call_expr.args {
                if ast.type != arg.type {
                    checker_error(c, ast.token,
                        "{} expected argument %i to be {}, got {}",
                        v.kind,
                        i,
                        type_to_string(ast.type),
                        type_to_string(arg.type),
                    )
                }
            }

        case .Abs,
             .Sign,
             .Trunc,
             .Floor,
             .Round,
             .Ceil,
             .Fract,
             .Rcp,
             .Sqrt,
             .RSqrt,
             .Sin,
             .Cos,
             .Tan,
             .Exp,
             .Exp2,
             .Log,
             .Log2:
            assert(len(call_expr.args) == 1)
            ast.type = call_expr.args[0].type

        case .Reduce_Add,
             .Reduce_Mul,
             .Reduce_And:
            assert(len(call_expr.args) == 1)
            ast.type = type_elem_basic_type(call_expr.args[0].type)
            assert(type_is_numeric(ast.type))

        case .Reduce_All,
             .Reduce_Any:
            assert(len(call_expr.args) == 1)
            ast.type = type_elem_basic_type(call_expr.args[0].type)
            assert(type_is_boolean(ast.type))

        case .Pow:
            assert(len(call_expr.args) == 2)
            ast.type = call_expr.args[0].type
            assert(type_elem_basic_type(ast.type) == type_elem_basic_type(call_expr.args[1].type))

        case .Blend:
            assert(len(call_expr.args) == 3)
            ast.type = call_expr.args[0].type
            assert(ast.type == call_expr.args[1].type)
            assert(type_is_boolean(type_elem_basic_type(call_expr.args[2].type)))
            // assert(call_expr.args[2].type.size * 8 >= len)

        case .Count_Leading_Zeros, .Count_Trailing_Zeros:
            assert(len(call_expr.args) == 1)
            assert(type_is_integer(call_expr.args[0].type))
            ast.type = c.basic_types[.I32]

        case .To_Bitmask:
            assert(len(call_expr.args) == 1)
            assert(type_is_vector(call_expr.args[0].type))
            assert(type_is_boolean(type_elem_basic_type(call_expr.args[0].type)))

            vec := call_expr.args[0].type.variant.(Type_Array)
            switch vec.len {
            case 8:
                ast.type = c.basic_types[.U8]
            case:
                unimplemented()
            }

        case .To_Scalar:

        case .To_Vector:

        case .To_String:
            assert(len(call_expr.args) == 1)

        case:
            checker_error(c, ast.token, "Builtin '{}' cannot be called", v.kind)
        }

    case:
        checker_error(c, ast.token,
            "Invalid call entity, expected procedure or builtin, got {}",
            reflect.union_variant_typeid(ent.variant),
        )
    }
}

check_cast_expr :: proc(c: ^Checker, ast: ^Ast) {
    assert(ast.type == nil)

    expr := ast.variant.(Ast_Cast_Expr)

    check_type(c, expr.type)
    check_expr(c, expr.value)

    #partial switch expr.op.kind {
    case .Conv:
        // lil hack :P
        #partial switch v in expr.value.type.variant {
        case Type_Array:
            basic := expr.type.type.variant.(Type_Basic)

            temp := type_clone(expr.value.type)
            temp_arr := &temp.variant.(Type_Array)
            temp_arr.type = expr.type.type
            ast.type = find_or_create_type(c, temp)

        case:
            ast.type = expr.type.type
        }

    case .Reinterpret:
        if expr.type.type.size != expr.value.type.size {
            checker_error(c, ast.token,
                "Cannot reinterpret between types of varying size: target type {} is {} bytes, value type {} is {} bytes)",
                type_to_string(expr.type.type),
                expr.type.type.size,
                type_to_string(expr.value.type),
                expr.value.type.size,
            )
        }

        ast.type = expr.type.type
    }
}

check_urnary_expr :: proc(c: ^Checker, ast: ^Ast, type_hint: ^Type) {
    assert(ast.type == nil)

    expr := ast.variant.(Ast_Unary_Expr)

    #partial switch expr.op.kind {
    case .Sub:
        check_expr(c, expr.expr, type_hint = type_hint)
        elem := type_elem_basic_type(expr.expr.type)
        ast.type = expr.expr.type

        if !type_is_numeric(elem) {
            checker_error(c, ast.token, "Unary 'Not' operator can be only applied to numeric values")
        }

    case .Not:
        check_expr(c, expr.expr, type_hint = type_hint)
        elem := type_elem_basic_type(expr.expr.type)
        ast.type = expr.expr.type

        if !type_is_boolean(elem) && !type_is_integer(elem) {
            checker_error(c, ast.token, "Unary 'Not' operator can be only applied to boolean and integer values")
        }

    case:
        checker_error(c, ast.token, "Invalid unary operator: ", expr.op)
    }
}

check_binary_op :: proc(
    c:          ^Checker,
    left:       ^Ast,
    right:      ^Ast,
    op:         Token_Kind,
    type_hint:  ^Type,
) -> (result: ^Type, dominant: ^Type) {
    check_expr(c, left, type_hint = type_hint)

    hint: ^Type
    #partial switch op {
    case .Bit_Shift_Left, .Bit_Shift_Right:
        hint = c.basic_types[.I32]
    case:
        hint = left.type
    }

    check_expr(c, right, type_hint = hint)

    left_elem := type_elem_basic_type(left.type)
    #partial switch op {
    case .Bit_Shift_Left, .Bit_Shift_Right:
        if !type_is_integer(left_elem) {
            checker_error(c, left.token, "Shift operand must be an integer")
        }

        if !type_is_integer(type_elem_basic_type(right.type)) {
            checker_error(c, right.token, "Shift amount must be an integer")
        }

        // HACK
        #partial switch v in right.type.variant {
        case Type_Array:
            assert(v.kind == .Vector)
            result = right.type
            dominant = right.type

        case:
            result = left.type
            dominant = left.type
        }


    case:

        if left.type == right.type {
            result = left.type
        } else {
            // this does not spark joy :(
            // Allow ops between basic/array/vector/vector_array types with same element type
            #partial switch vl in left.type.variant {
            case Type_Basic:
                #partial switch vr in right.type.variant {
                case Type_Array:
                    if left.type == vr.type {
                        result = right.type
                    }

                    #partial switch vrt in vr.type.variant {
                    case Type_Array:
                        if vrt.kind != .Vector do break
                        if vrt.type == left.type {
                            result = right.type
                        }
                    }
                }

            case Type_Array:
                #partial switch vr in right.type.variant {
                case Type_Basic:
                    if right.type == vl.type {
                        result = left.type
                    }

                    #partial switch vlt in vl.type.variant {
                    case Type_Array:
                        if vlt.kind != .Vector do break
                        if vlt.type == right.type {
                            result = left.type
                        }
                    }

                case Type_Array:
                    #partial switch vlt in vl.type.variant {
                    case Type_Basic:
                        vrt := vr.type.variant.(Type_Array) or_break
                        if vrt.kind != .Vector do break
                        result = right.type

                    case Type_Array:
                        if vlt.kind != .Vector do break
                        vrt := vr.type.variant.(Type_Basic) or_break
                        result = left.type
                    }
                }
            }
        }

        dominant = result

        if result == nil {
            checker_error(c, left.token, "Incompatible types in binary op '{}': {} vs {}",
                _token_str[op],
                type_to_string(left.type),
                type_to_string(right.type),
            )
        }
    }


    #partial switch op {
    case .Equal,
         .Less_Than,
         .Less_Than_Equal,
         .Greater_Than,
         .Greater_Than_Equal,
         .Not_Equal:

        #partial switch v in left.type.variant {
        case Type_Basic:
            result = c.basic_types[.B32] // TODO: type hint
        case Type_Array:
            result = type_clone(left.type)
            arr := &result.variant.(Type_Array)
            arr.type = c.basic_types[.B32] // HACK
            result = find_or_create_type(c, result)
        }

        if op != .Equal && op != .Not_Equal && !type_is_numeric(left_elem) {
            checker_error(c, left.token, "{} operator can be only applied to numeric values", op)
        }

    case .Add,
         .Sub,
         .Mul,
         .Div:
        if !type_is_numeric(left_elem) {
            checker_error(c, left.token, "{} operator can be only applied to numeric values", op)
        }

    case .Bit_And,
         .Bit_Or,
         .Bit_Xor,
         .Bit_Shift_Left,
         .Bit_Shift_Right,
         .Mod:
    }

    assert(result != nil)
    assert(dominant != nil)

    return result, dominant
}

// check_type_internal, check_type, add_type_info_internal, check_ident
// https://github.com/odin-lang/Odin/blob/master/src/check_type.cpp#L827

check_expr :: proc(c: ^Checker, ast: ^Ast, type_hint: ^Type = nil) {
    if ast == nil do return

    assert(ast.type == nil)

    #partial switch v in ast.variant {
    case Ast_Ident:
        check_ident(c, ast)

    case Ast_Basic_Literal:
        check_basic_literal(c, ast, type_hint = type_hint)

    case Ast_Compound_Literal:
        check_compound_literal(c, ast, type_hint = type_hint)

    case Ast_Call_Expr:
        check_call_expr(c, ast)

    case Ast_Unary_Expr:
        check_urnary_expr(c, ast, type_hint = type_hint)

    case Ast_Cast_Expr:
        check_cast_expr(c, ast)

    case Ast_Binary_Expr:
        check_binary_expr(c, ast, type_hint = type_hint)

    case Ast_Selector_Expr:
        check_selector_expr(c, ast)

    case Ast_Index_Expr:
        check_index_expr(c, ast)

    case Ast_Address_Expr:
        check_address_expr(c, ast)

    case Ast_Deref_Expr:
        check_deref_expr(c, ast)

    case:
        assert(false)
    }
}

check_selector_expr :: proc(c: ^Checker, ast: ^Ast) {
    assert(ast.type == nil)

    expr := ast.variant.(Ast_Selector_Expr)
    check_expr(c, expr.left)

    {
        ident := expr.right.variant.(Ast_Ident)
        name := ident.token.text

        base_type := expr.left.type

        loop: for {
            #partial switch v in base_type.variant {
            case Type_Pointer:
                base_type = v.type

            case:
                break loop
            }
        }

        switch name {
        case "len":
            #partial switch v in base_type.variant {
            case Type_Basic:
                #partial switch v.kind {
                case .String:
                    expr.right.type = c.basic_types[.I32]
                }

            case Type_Array:
                expr.right.type = c.basic_types[.I32]
            }

        case:

            #partial switch v in base_type.variant {
            case Type_Struct:
                for field in v.fields {
                    if ident.token.text == field.name {
                        expr.right.type = field.type
                        break
                    }
                }

            case Type_Array:
                // Swizzling later?
                if v.len > 4 {
                    break
                }

                all_fields := [?][2]u8 {
                    0 = {'x', 'r'},
                    1 = {'y', 'g'},
                    2 = {'z', 'b'},
                    3 = {'w', 'a'},
                }
                fields := all_fields[:v.len]

                if len(name) != 1 {
                    break
                }

                field_loop: for field in fields {
                    for ch in field {
                        if ch == name[0] {
                            expr.right.type = v.type
                            break field_loop
                        }
                    }
                }
            }
        }

        if expr.right.type == nil {
            checker_error(c, ast.token, "Invalid field {}", name)
        }
    }

    ast.type = expr.right.type
}

check_index_expr :: proc(c: ^Checker, ast: ^Ast) {
    assert(ast.type == nil)

    expr := ast.variant.(Ast_Index_Expr)
    check_expr(c, expr.left)
    check_expr(c, expr.index)

    if !type_is_integer(expr.index.type) {
        checker_error(c, ast.token,
            "Index must be of integer type, got: {}",
            type_to_string(expr.index.type),
        )
    }

    #partial switch v in expr.left.type.variant {
    case Type_Array:
        ast.type = v.type

    case Type_Pointer:
        ast.type = v.type
        if v.kind == .Single {
            checker_error(c, ast.token,
                "Cannot index a single pointer. Did you want to use multi-pointer instead? (^ vs [^])",
            )
        }

    case Type_Basic:
        #partial switch v.kind {
        case .String:
            ast.type = c.basic_types[.U8]

        case:
            checker_error(c, ast.token, "Cannot index {}", type_to_string(expr.left.type))
        }

    case Type_Struct:
        ast.type = type_scalarize(expr.left.type)

    case:
        assert(false)
    }

    #partial switch v in expr.index.value {
    case i128:
        #partial switch lv in expr.left.type.variant {
        case Type_Array:
            if v < 0 || int(v) > lv.len {
                checker_error(c, ast.token, "Constant index is out of bounds")
            }
        }

    case nil:
        break
    case:
        assert(false)
    }
}

check_address_expr :: proc(c: ^Checker, ast: ^Ast) {
    assert(ast.type == nil)

    expr := ast.variant.(Ast_Address_Expr)
    check_expr(c, expr.expr)

    ptr_type := new(Type)
    // HACK
    ptr_type.size = 8
    ptr_type.variant = Type_Pointer{
        kind = .Single,
        type = expr.expr.type,
    }

    ast.type = find_or_create_type(c, ptr_type)
}

check_deref_expr :: proc(c: ^Checker, ast: ^Ast) {
    assert(ast.type == nil)

    expr := ast.variant.(Ast_Deref_Expr)
    check_expr(c, expr.expr)
    #partial switch v in expr.expr.type.variant {
    case Type_Pointer:
    case:
        checker_error(c, ast.token, "Only pointer types can be de-referenced, got: {}", type_to_string(expr.expr.type))
    }
}

check_binary_expr :: proc(c: ^Checker, ast: ^Ast, type_hint: ^Type = nil) {
    assert(ast.type == nil)

    expr := &ast.variant.(Ast_Binary_Expr)
    ast.type, expr.dominant = check_binary_op(c,
        expr.left,
        expr.right,
        expr.op.kind,
        type_hint = type_hint,
    )

    assert(expr.dominant != nil)

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
                checker_error(c, ast.token,
                    "Invalid operator in constant integer expression: {}",
                    _token_str[expr.op.kind],
                )
            }

        case nil:

        case:
            checker_error(c, ast.token,
                "Invalid constant binary operation, got {} and {}",
                value_type_name(l),
                value_type_name(r),
            )
        }

    case f64:
        #partial switch r in expr.right.value {
        case f64:
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

            case:
                assert(false)
            }

        case nil:

        case:
            checker_error(c, ast.token,
                "Invalid constant binary operation, got {} and {}",
                value_type_name(l),
                value_type_name(r),
            )
        }

    case nil:
    }
}

check_value_decl :: proc(c: ^Checker, ast: ^Ast) {
    assert(ast.type == nil)

    decl := ast.variant.(Ast_Value_Decl)
    check_type(c, decl.type)
    // check_ident(c, value.name)

    if decl.vector == .Default {
        // TODO
    }

    if decl.vector == .Vector {
        // NOTE: things like this could be cached on the scalar type, but idgaf for now
        vec := type_vectorize(decl.type.type)
        decl.type.type = find_or_create_type(c, vec)
    }

    if decl.value != nil {
        check_expr(c, decl.value, type_hint = decl.type.type)
        // TODO check same as in assign stmt
        if !check_are_types_assignable(c, decl.type.type, decl.value.type) {
            checker_error(c, ast.token,
                "Invalid initializer value, types aren't assignable: {} vs {}",
                type_to_string(decl.type.type),
                type_to_string(decl.value.type),
            )
        }
        if decl.mut == .Constant {
            ast.value = decl.value.value
        }
    }
    ast.type = decl.type.type
}

check_proc_param_field :: proc(c: ^Checker, ast: ^Ast) {
    decl := ast.variant.(Ast_Value_Decl)
    // check_type(c, decl.type)
    check_value_decl(c, ast)
    ast.type = decl.type.type
}

check_type_is_expandible :: proc(c: ^Checker, dst: ^Type, src: ^Type) -> bool {
    if dst == src {
        return true
    }

    #partial switch vdst in dst.variant {
    case Type_Array:
        if vdst.type == src {
            return true
        }

        #partial switch vdst_elem in vdst.type.variant {
        case Type_Array:
            if vdst_elem.type == src {
                return true
            }

            #partial switch vsrc in src.variant {
            case Type_Array:
                if vsrc.type == vdst_elem.type {
                    return true
                }
            }

        case Type_Basic:
            #partial switch vsrc in src.variant {
            case Type_Array:
                if vsrc.type == vdst.type {
                    return true
                }
            }
        }
    }

    return false
}

check_are_types_assignable :: proc(c: ^Checker, left: ^Type, right: ^Type) -> bool {
    return check_type_is_expandible(c, dst = left, src = right)
    // if left == right {
    //     return true
    // }

    // if arr, ok := left.variant.(Type_Array); ok {
    //     if arr.kind == .Vector && arr.type == right {
    //         return true
    //     }

    //     if elem, ok := arr.type.variant.(Type_Array); ok &&
    //        elem.kind == .Vector &&
    //        elem.type == right {
    //         return true
    //     }
    // }

    // return false
}

check_stmt :: proc(c: ^Checker, ast: ^Ast) {
    #partial switch &v in ast.variant {
    case Ast_Value_Decl:
        check_value_decl(c, ast)

    case Ast_Assign_Stmt:
        op, op_ok := token_normalize_assign_op(v.op.kind)
        if !op_ok {
            checker_error(c, ast.token, "Invalid assign statement op: {}", _token_str[op])
        }

        // TODO: handle mutability

        #partial switch v.op.kind {
        case .Assign:
            check_expr(c, v.left)
            check_expr(c, v.right, type_hint = v.left.type)

            if !check_are_types_assignable(c, v.left.type, v.right.type) {
                checker_error(c, ast.token,
                    "Types aren't assignable: {} vs {}",
                    type_to_string(v.left.type),
                    type_to_string(v.right.type),
                )
            }

        case:
            dominant: ^Type
            ast.type, dominant = check_binary_op(c, v.left, v.right, op, type_hint = nil)
            assert(dominant == v.left.type) // idk
        }

    case Ast_Return_Stmt:
        check_return_stmt(c, ast)

    case Ast_Call_Expr:
        check_call_expr(c, ast)

    case Ast_For_Stmt:
        check_begin_scope(c, v.scope)
        check_value_decl(c, v.init)
        check_expr(c, v.cond)
        check_stmt(c, v.post)
        check_block_stmt(c, v.body)
        check_end_scope(c)

    case Ast_For_Range_Stmt:
        iter := v.ident.variant.(Ast_Ident).token.text

    case Ast_If_Stmt:
        check_if_stmt(c, ast)

    case Ast_Break_Stmt:
        // for s := c.curr_scope; s != nil; s = s.parent {
        // }

    case Ast_Continue_Stmt:

    case Ast_Block_Stmt:
        check_block_stmt(c, ast)
    }
}

check_block_stmt :: proc(c: ^Checker, ast: ^Ast, scope := true) {
    block := ast.variant.(Ast_Block_Stmt)

    if scope {
        check_begin_scope(c, block.scope)
    }

    for stmt, i in block.statements {
        check_stmt(c, stmt)
        #partial switch v in stmt.variant {
        case Ast_Return_Stmt:
            if i != len(block.statements) - 1 {
                assert(false)
                checker_error(c, stmt.token,
                    "Statements after return statement will never get executed",
                )
            }

        case Ast_Block_Stmt,
             Ast_If_Stmt,
             Ast_For_Stmt,
             Ast_Break_Stmt,
             Ast_Continue_Stmt,
             Ast_For_Range_Stmt,
             Ast_Assign_Stmt:

        case Ast_Value_Decl,
             Ast_Proc_Decl:

        case Ast_Call_Expr:

        case:
            checker_error(c, stmt.token, "Unused expression")
        }
    }

    if scope {
        check_end_scope(c)
    }
}

check_if_stmt :: proc(c: ^Checker, ast: ^Ast) {
    stmt := ast.variant.(Ast_If_Stmt)
    check_expr(c, stmt.cond)

    is_vector := false
    #partial switch v in stmt.cond.type.variant {
    case Type_Basic:
        if !type_is_boolean(stmt.cond.type) {
            checker_error(c, stmt.cond.token, "Condition in a scalar if statement must have boolean type")
        }

    case Type_Array:
        if v.kind != .Vector {
            checker_error(c, stmt.cond.token, "If statements don't accept non-vector arrays")
        }

        if !type_is_boolean(v.type) {
            checker_error(c, stmt.cond.token, "Condition in an vector if statement must have boolean element type")
        }

        is_vector = true

    case:
        checker_error(c, stmt.cond.token,
            "Unexpected type in an if statement, expected vector or scalar boolean, got {}",
            type_to_string(stmt.cond.type)
        )
    }

    _check_cond_block(c, stmt.if_body, is_vector)

    if stmt.else_body != nil {
        _check_cond_block(c, stmt.else_body, is_vector)
    }

    return

    _check_cond_block :: proc(c: ^Checker, ast: ^Ast, is_vector: bool) {
        block := ast.variant.(Ast_Block_Stmt)
        check_begin_scope(c, block.scope)

        block.scope.flags += {.Conditional}
        if is_vector {
            block.scope.flags += {.Masked}
            block.scope.vector_width = VECTOR_WIDTH
        }

        check_block_stmt(c, ast, scope = false)

        check_end_scope(c)
    }
}

check_return_stmt :: proc(c: ^Checker, ast: ^Ast) {
    stmt := ast.variant.(Ast_Return_Stmt)
    check_expr(c, stmt.value)
    proc_decl := c.curr_entity.ast.variant.(Ast_Proc_Decl)
    proc_type := proc_decl.type.variant.(Ast_Proc_Type)

    if proc_type.result == nil {
        if stmt.value == nil {
            // Ok
        } else {
            checker_error(c, ast.token,
                "Unexpected value, this procedure doesn't return anything",
            )
        }
    } else {
        if stmt.value == nil {
            checker_error(c, ast.token,
                "Expected a value of type {}",
                type_to_string(proc_type.result.type),
            )
        } else {
            if stmt.value.type != proc_type.result.type {
                checker_error(c, stmt.value.token,
                    "Invalid return type, expected {}, got {}",
                    type_to_string(proc_type.result.type),
                    type_to_string(stmt.value.type),
                )
            }
        }
    }
}

check_begin_scope :: proc(c: ^Checker, scope: ^Scope) {
    assert(scope.parent == c.curr_scope)
    c.curr_scope = scope
}

check_end_scope :: proc(c: ^Checker) {
    c.curr_scope = c.curr_scope.parent
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

create_new_type_from_ast :: proc(c: ^Checker, ast: ^Ast, type_hint: ^Type = nil) -> (result: ^Type) {
    #partial switch v in ast.variant {
    case Ast_Basic_Literal:
        hint_basic := type_elem_basic_type(type_hint)

        if hint_basic != nil {
            #partial switch v.token.kind {
            case .Integer:
                if type_is_numeric(hint_basic) {
                    return type_hint
                } else {
                    checker_error(c, ast.token,
                        "Integer literal cannot be assigned to non-numeric-element type ({} from {})",
                        type_to_string(hint_basic),
                        type_to_string(type_hint),
                    )
                }

            case .Float:
                // Accept ints without a fraction
                if type_is_float(hint_basic) {
                    return type_hint
                } else {
                    checker_error(c, ast.token,
                        "Integer literal cannot be assigned to non-float-element type ({} from {})",
                        type_to_string(hint_basic),
                        type_to_string(type_hint),
                    )
                }

            case .True, .False:
                if type_is_boolean(hint_basic) {
                    return type_hint
                } else {
                    checker_error(c, ast.token,
                        "Boolean literal cannot be assigned to non-boolean-element type",
                    )
                }

            case .Char:
                if type_is_integer(hint_basic) {
                    return type_hint
                } else {
                    checker_error(c, ast.token,
                        "Character literal cannot be assigned to non-integer-element type",
                    )
                }
            }
        }

        #partial switch v.token.kind {
        case .Integer:      return c.basic_types[.I32]
        case .Float:        return c.basic_types[.F32]
        case .True, .False: return c.basic_types[.B8]
        case .Char:         return c.basic_types[.U8]
        case .String:       return c.basic_types[.String]
        }

    case Ast_Pointer_Type:
        result = new(Type)
        result.size = 8
        result.variant = Type_Pointer{
            kind = .Single,
            type = find_or_create_type_ast(c, v.type, type_hint = type_hint),
        }

    case Ast_Multi_Pointer_Type:
        result = new(Type)
        result.size = 8
        result.variant = Type_Pointer{
            kind = .Multi,
            type = find_or_create_type_ast(c, v.type, type_hint = type_hint),
        }

    case Ast_Array_Type:
        result = new(Type)
        length := 0
        #partial switch l in v.len.value {
        case i128:
            length = int_cast(int, l)
        case:
            assert(false)
        }
        assert(length > 0)
        elem := find_or_create_type_ast(c, v.type, type_hint = type_hint)

        result.size = elem.size * length

        result.variant = Type_Array{
            kind = v.kind,
            len = length,
            type = elem,
        }

    case Ast_Struct_Type:
        result = new(Type)
        str: Type_Struct
        str.fields = make([]Type_Struct_Field, len(v.fields))
        for field, i in v.fields {
            f := field.variant.(Ast_Field)
            str.fields[i] = {
                name = f.name.variant.(Ast_Ident).token.text,
                type = find_or_create_type_ast(c, f.type, type_hint = type_hint),
            }
        }
        result.variant = str

    case Ast_Field:
        result = find_or_create_type_ast(c, v.type, type_hint = type_hint)

    case:
        assert(false, "AST is not a valid type")
    }

    return result
}

find_or_create_type_ast :: proc(c: ^Checker, ast: ^Ast, type_hint: ^Type = nil) -> (result: ^Type) {
    #partial switch v in ast.variant {
    case Ast_Ident:
        switch v.token.text {
        case "B8", "Bool"   : result = c.basic_types[.B8 ]
        case "B16"          : result = c.basic_types[.B16]
        case "B32"          : result = c.basic_types[.B32]
        case "B64"          : result = c.basic_types[.B64]
        case "I8"           : result = c.basic_types[.I8 ]
        case "I16"          : result = c.basic_types[.I16]
        case "I32"          : result = c.basic_types[.I32]
        case "I64"          : result = c.basic_types[.I64]
        case "U8"           : result = c.basic_types[.U8 ]
        case "U16"          : result = c.basic_types[.U16]
        case "U32"          : result = c.basic_types[.U32]
        case "U64"          : result = c.basic_types[.U64]
        case "F32", "Float" : result = c.basic_types[.F32]
        case "F64"          : result = c.basic_types[.F64]
        case "String"       : result = c.basic_types[.String]

        case "b8", "bool",
             "b16",
             "b32",
             "b64",
             "i8",
             "i16",
             "i32",
             "i64",
             "u8",
             "u16",
             "u32",
             "u64",
             "f32", "float",
             "f64",
             "string":
            checker_error(c, ast.token,
                "Basic types are uppercase, please use {} instead of {}",
                strings.to_upper(v.token.text),
                v.token.text,
            )

        case:
            if ent, _, ok := find_entity(c.curr_scope, v.token.text); ok {
                if !ent.checked {
                    check_entity(c, ent)
                }

                #partial switch ev in ent.variant {
                // TODO: alias
                case Entity_Struct:
                    result = ev.type

                case:
                    checker_error(c, ast.token, "Entity {} is not a type", v.token.text)
                }

            } else {
                checker_error(c, ast.token, "Unknown type: {}", v.token.text)
            }
        }

    case:
        result = find_or_create_type(c,
            type = create_new_type_from_ast(c, ast, type_hint = type_hint),
        )
    }

    assert(result != nil)

    return result
}

// If an already existing match is found, the match gets returned.
// Otherwise the input type will get registered as a new type and reused later!
find_or_create_type :: proc(c: ^Checker, type: ^Type) -> ^Type {
    switch &v in type.variant {
    case Type_Basic:
        return c.basic_types[v.kind]

    case Type_Array:
        v.type = find_or_create_type(c, v.type)

    case Type_Pointer:
        v.type = find_or_create_type(c, v.type)

    case Type_Struct:
        for &field in v.fields {
            field.type = find_or_create_type(c, field.type)
        }
    }

    // Linear search, bit dumb. Could hash some type info to split into buckets.
    for t in c.types {
        if types_equal(t, type) {
            return t
        }
    }

    append(&c.types, type)

    return type
}

check_type_decl :: proc(c: ^Checker, ast: ^Ast, name: string) {

}

check_entitites_recursive :: proc(c: ^Checker, scope: ^Scope) {
    for name, ent in scope.entities {
        check_entity(c, ent)
    }

    for child in scope.children {
        c.curr_scope = child
        check_entitites_recursive(c, child)
        c.curr_scope = c.curr_scope.parent
    }
}

check_scope_procedure_types_recursive :: proc(c: ^Checker, scope: ^Scope) {
    for name, ent in scope.entities {
        c.curr_entity = ent

        #partial switch v in ent.variant {
        case Entity_Proc:
            decl := ent.ast.variant.(Ast_Proc_Decl)
            check_proc_type(c, decl.type, name)
            ent.ast.type = decl.type.type
        }
    }

    for child in scope.children {
        c.curr_scope = child
        check_scope_procedure_types_recursive(c, child)
        c.curr_scope = c.curr_scope.parent
    }
}

check_entity :: proc(c: ^Checker, ent: ^Entity) {
    assert(ent != nil)
    if ent.checked {
        return
    }

    prev_ent := c.curr_entity
    c.curr_entity = ent
    defer c.curr_entity = prev_ent

    switch &v in ent.variant {
    case Entity_Variable:
        decl := ent.ast.variant.(Ast_Value_Decl)
        if decl.scope != c.curr_file_scope {
            return
        }
        check_value_decl(c, ent.ast)
        ent.ast.type = decl.type.type

    case Entity_Struct:
        if ent.ast == nil do break
        decl := ent.ast.variant.(Ast_Struct_Decl) or_break
        check_type(c, decl.type)
        ent.ast.type = decl.type.type
        v.type = decl.type.type

    case
        Entity_Builtin,
        Entity_Struct_Field,
        Entity_Proc:
        return
    }

    ent.checked = true
}

// Check and codegen C
check_program :: proc(c: ^Checker) {
    c.basic_types = [Type_Basic_Kind]^Type{
        .Invalid = new_clone(Type{size = 0, variant = Type_Basic{kind = .Invalid}}),
        .B8      = new_clone(Type{size = 1, variant = Type_Basic{kind = .B8 }}),
        .B16     = new_clone(Type{size = 2, variant = Type_Basic{kind = .B16}}),
        .B32     = new_clone(Type{size = 4, variant = Type_Basic{kind = .B32}}),
        .B64     = new_clone(Type{size = 8, variant = Type_Basic{kind = .B64}}),
        .I8      = new_clone(Type{size = 1, variant = Type_Basic{kind = .I8 }}),
        .I16     = new_clone(Type{size = 2, variant = Type_Basic{kind = .I16}}),
        .I32     = new_clone(Type{size = 4, variant = Type_Basic{kind = .I32}}),
        .I64     = new_clone(Type{size = 8, variant = Type_Basic{kind = .I64}}),
        .U8      = new_clone(Type{size = 1, variant = Type_Basic{kind = .U8 }}),
        .U16     = new_clone(Type{size = 2, variant = Type_Basic{kind = .U16}}),
        .U32     = new_clone(Type{size = 4, variant = Type_Basic{kind = .U32}}),
        .U64     = new_clone(Type{size = 8, variant = Type_Basic{kind = .U64}}),
        .F32     = new_clone(Type{size = 4, variant = Type_Basic{kind = .F32}}),
        .F64     = new_clone(Type{size = 8, variant = Type_Basic{kind = .F64}}),
        .String  = new_clone(Type{size = 16, variant = Type_Basic{kind = .String}}),
    }

    for t in c.basic_types {
        append(&c.types, t)
    }

    v8i32_type := find_or_create_type(c, new_clone(Type{variant = Type_Array{
        kind = .Vector, len = 8, type = c.basic_types[.I32],
    }}))

    v8b32_type := find_or_create_type(c, new_clone(Type{variant = Type_Array{
        kind = .Vector, len = 8, type = c.basic_types[.B32],
    }}))

    create_entity(c.curr_file_scope, "vector_width", nil, Entity_Builtin{
        kind = .Vector_Width, value = VECTOR_WIDTH, type = c.basic_types[.I32]})
    create_entity(c.curr_file_scope, "vector_index", nil, Entity_Builtin{
        kind = .Vector_Index, value = [8]i32{0, 1, 2, 3, 4, 5, 6, 7}, type = v8i32_type})
    create_entity(c.curr_file_scope, "vector_mask", nil, Entity_Builtin{
        kind = .Vector_Index, type = v8b32_type})

    create_entity(c.curr_file_scope, "print"   , nil, Entity_Builtin{kind = .Print})
    create_entity(c.curr_file_scope, "println" , nil, Entity_Builtin{kind = .Println})
    create_entity(c.curr_file_scope, "min"     , nil, Entity_Builtin{kind = .Min})
    create_entity(c.curr_file_scope, "max"     , nil, Entity_Builtin{kind = .Max})
    create_entity(c.curr_file_scope, "clamp"   , nil, Entity_Builtin{kind = .Clamp})
    create_entity(c.curr_file_scope, "abs"     , nil, Entity_Builtin{kind = .Abs})
    create_entity(c.curr_file_scope, "sign"    , nil, Entity_Builtin{kind = .Sign})
    create_entity(c.curr_file_scope, "blend"   , nil, Entity_Builtin{kind = .Blend})
    create_entity(c.curr_file_scope, "trunc"   , nil, Entity_Builtin{kind = .Trunc})
    create_entity(c.curr_file_scope, "floor"   , nil, Entity_Builtin{kind = .Floor})
    create_entity(c.curr_file_scope, "round"   , nil, Entity_Builtin{kind = .Round})
    create_entity(c.curr_file_scope, "ceil"    , nil, Entity_Builtin{kind = .Ceil})
    create_entity(c.curr_file_scope, "fract"   , nil, Entity_Builtin{kind = .Fract})
    create_entity(c.curr_file_scope, "rcp"     , nil, Entity_Builtin{kind = .Rcp})
    create_entity(c.curr_file_scope, "sqrt"    , nil, Entity_Builtin{kind = .Sqrt})
    create_entity(c.curr_file_scope, "rsqrt"   , nil, Entity_Builtin{kind = .RSqrt})
    create_entity(c.curr_file_scope, "sin"     , nil, Entity_Builtin{kind = .Sin})
    create_entity(c.curr_file_scope, "cos"     , nil, Entity_Builtin{kind = .Cos})
    create_entity(c.curr_file_scope, "tan"     , nil, Entity_Builtin{kind = .Tan})
    create_entity(c.curr_file_scope, "pow"     , nil, Entity_Builtin{kind = .Pow})
    create_entity(c.curr_file_scope, "exp"     , nil, Entity_Builtin{kind = .Exp})
    create_entity(c.curr_file_scope, "exp2"    , nil, Entity_Builtin{kind = .Exp2})
    create_entity(c.curr_file_scope, "log"     , nil, Entity_Builtin{kind = .Log})
    create_entity(c.curr_file_scope, "log2"    , nil, Entity_Builtin{kind = .Log2})
    create_entity(c.curr_file_scope, "reduce_add",nil,Entity_Builtin{kind = .Reduce_Add})
    create_entity(c.curr_file_scope, "reduce_mul",nil,Entity_Builtin{kind = .Reduce_Mul})
    create_entity(c.curr_file_scope, "reduce_and",nil,Entity_Builtin{kind = .Reduce_And})
    create_entity(c.curr_file_scope, "reduce_all",nil,Entity_Builtin{kind = .Reduce_All})
    create_entity(c.curr_file_scope, "reduce_any",nil,Entity_Builtin{kind = .Reduce_Any})
    create_entity(c.curr_file_scope, "to_bitmask", nil, Entity_Builtin{kind = .To_Bitmask})
    create_entity(c.curr_file_scope, "to_vector", nil, Entity_Builtin{kind = .To_Vector})
    create_entity(c.curr_file_scope, "to_scalar", nil, Entity_Builtin{kind = .To_Scalar})
    create_entity(c.curr_file_scope, "to_string", nil, Entity_Builtin{kind = .To_String})
    create_entity(c.curr_file_scope, "count_leading_zeros", nil, Entity_Builtin{kind = .Count_Leading_Zeros})
    create_entity(c.curr_file_scope, "count_trailing_zeros", nil, Entity_Builtin{kind = .Count_Trailing_Zeros})

    // 1. check all types
    // 2. check all entity declarations
    // 3. check procedure bodies - needs proc declarations for checking

    check_entitites_recursive(c, c.curr_file_scope)

    check_scope_procedure_types_recursive(c, c.curr_file_scope)

    for name, ent in c.curr_file_scope.entities {
        c.curr_entity = ent

        #partial switch v in ent.variant {
        case Entity_Proc:
            fmt.println("\nPROC ENTITY", name, ent)
            decl := ent.ast.variant.(Ast_Proc_Decl)
        }
    }

    proc_entities := make([dynamic]^Entity)

    for name, ent in c.curr_file_scope.entities {
        #partial switch v in ent.variant {
        case Entity_Proc:
            fmt.println("\nPROC ENTITY", name, ent)
            append(&proc_entities, ent)
        }
    }

    // for name, ent in c.curr_file_scope.entities {
    for ent in proc_entities {
        c.curr_entity = ent

        #partial switch v in ent.variant {
        case Entity_Proc:
            // fmt.println("\nPROC ENTITY", name, ent)
            decl := ent.ast.variant.(Ast_Proc_Decl)
            check_block_stmt(c, decl.body)
        }
    }
}