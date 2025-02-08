package vecc

import "core:reflect"
import "core:fmt"
import "core:slice"

Type :: struct {
    size:           int,
    variant:        Type_Variant,
    cname:          string,
    cname_lower:    string,
}

Type_Variant :: union {
    Type_Basic,
    Type_Array,
    Type_Pointer,
    Type_Struct,
}

Type_Basic :: struct {
    kind:   Type_Basic_Kind,
}

Type_Basic_Kind :: enum u8 {
    B8,
    B16,
    B32,
    B64,
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    F32,
    F64,
}

Type_Array :: struct {
    kind:   Type_Array_Kind,
    len:    int,
    type:   ^Type,
}

Type_Array_Kind :: enum u8 {
    Fixed_Array = 0,
    Vector,
}

Type_Pointer :: struct {
    kind:   Type_Pointer_Kind,
    type:   ^Type,
}

Type_Pointer_Kind :: enum u8 {
    Single = 0,
    Multi,
}

Type_Struct :: struct {
    fields: []Type_Struct_Field,
}

Type_Struct_Field :: struct {
    name:   string,
    type:   ^Type,
}

type_is_boolean :: proc(t: ^Type) -> bool {
    #partial switch v in t.variant {
    case Type_Basic:
        #partial switch v.kind {
        case .B8, .B16, .B32, .B64:
            return true
        case:
            return false
        }

    case:
        return false
    }
}

type_is_integer :: proc(t: ^Type) -> bool {
    #partial switch v in t.variant {
    case Type_Basic:
        #partial switch v.kind {
        case .I8,
             .I16,
             .I32,
             .I64,
             .U8,
             .U16,
             .U32,
             .U64:
            return true

        case:
            return false
        }

    case:
        return false
    }
}

type_elem_basic_type :: proc(t: ^Type) -> ^Type {
    if t == nil {
        return nil
    }
    switch v in t.variant {
    case Type_Basic:
        return t

    case Type_Array:
        type_elem_basic_type(v.type)

    case Type_Pointer:
        type_elem_basic_type(v.type)

    case Type_Struct:
    }
    return nil
}

type_is_float :: proc(t: ^Type) -> bool {
    #partial switch v in t.variant {
    case Type_Basic:
        #partial switch v.kind {
        case .F32,
             .F64:
            return true

        case:
            return false
        }

    case:
        return false
    }
}

type_is_numeric :: proc(t: ^Type) -> bool {
    #partial switch v in t.variant {
    case Type_Basic:
        #partial switch v.kind {
        case .I8,
             .I16,
             .I32,
             .I64,
             .U8,
             .U16,
             .U32,
             .U64,
             .F32,
             .F64:
            return true

        case:
            return false
        }

    case:
        return false
    }
}

type_to_string :: proc(type: ^Type) -> string {
    switch v in type.variant {
    case Type_Basic:
        return reflect.enum_name_from_value(v.kind) or_break

    case Type_Array:
        return fmt.tprintf("{}[{}]{}", v.kind, v.len, type_to_string(v.type))

    case Type_Pointer:
        switch v.kind {
        case .Single:
            return fmt.tprintf("^{}", type_to_string(v.type))
        case .Multi:
            return fmt.tprintf("[^]{}", type_to_string(v.type))
        }

    case Type_Struct:
        return type.cname
    }

    assert(false)
    return "Invalid"
}

// For cases when types are not yet interned so we can't compare pointers.
types_equal :: proc(a, b: ^Type) -> bool {
    switch va in a.variant {
    case Type_Basic:
        vb := b.variant.(Type_Basic) or_return
        if va.kind != vb.kind do return false

    case Type_Array:
        vb := b.variant.(Type_Array) or_return
        if va.len != vb.len do return false
        if va.kind != vb.kind do return false
        if !types_equal(va.type, vb.type) do return false

    case Type_Pointer:
        vb := b.variant.(Type_Pointer) or_return
        if va.kind != vb.kind do return false
        if !types_equal(va.type, vb.type) do return false

    case Type_Struct:
        vb := b.variant.(Type_Struct) or_return
        if len(va.fields) != len(vb.fields) do return false
        for f in soa_zip(a = va.fields, b = vb.fields) {
            if f.a.name != f.b.name do return false
            if !types_equal(f.a.type, f.b.type) do return false
        }
    }

    return true
}

type_clone :: proc(type: ^Type) -> ^Type {
    result := new_clone(type^)

    switch &v in result.variant {
    case Type_Basic:
    case Type_Array:
        v.type = type_clone(v.type)

    case Type_Pointer:
        v.type = type_clone(v.type)

    case Type_Struct:
        v.fields = slice.clone(v.fields)
        for &field in v.fields {
            field.type = type_clone(field.type)
        }
    }

    return result
}

VECTOR_WIDTH :: 8

type_vectorize :: proc(type: ^Type) -> ^Type {
    result := new_clone(type^)

    switch &v in result.variant {
    case Type_Basic:
        vec := new(Type)
        vec.variant = Type_Array{
            kind = .Vector,
            len  = VECTOR_WIDTH,
            type = result,
        }
        result = vec

    case Type_Array:
        assert(v.kind == .Fixed_Array)
        v.type = type_vectorize(v.type)

    case Type_Pointer:
        v.type = type_vectorize(v.type)

    case Type_Struct:
        v.fields = slice.clone(v.fields)
        for &field in v.fields {
            field.type = type_vectorize(field.type)
        }
    }

    return result
}