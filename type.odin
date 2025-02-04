package vecc

import "core:reflect"
import "core:fmt"

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
    #partial switch v in type.variant {
    case Type_Basic:
        return reflect.enum_name_from_value(v.kind) or_break

    case Type_Array:
        return fmt.tprintf("{}[{}]{}", v.kind, v.len, type_to_string(v.type))

    case:
        assert(false, "Invalid type")
    }

    return "Invalid"
}
