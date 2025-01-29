package vecc

import "core:unicode/utf8"

Pos :: struct {
    offset: int,
    line:   int,
    column: int,
}

Token :: struct {
    using pos:  Pos,
    kind:       Token_Kind,
    text:       string,
}

Token_Kind :: enum u8 {
    Invalid = 0,
    EOF,

    Ident,
    Builtin,

    Integer,
    Float,
    String,
    Char,
    True,
    False,

    Period,
    Comma,
    Semicolon,
    Colon,

    Open_Paren,
    Close_Paren,
    Open_Brace,
    Close_Brace,
    Open_Bracket,
    Close_Bracket,

    If,
    Else,
    For,
    Range,
    Break,
    Continue,
    In,
    Proc,
    Vector,
    Scalar,
    Conv,
    Reinterpret,
    Return,
    Lanes,
    Private,
    Export,

    Equal,
    Less_Than,
    Less_Than_Equal,
    Greater_Than,
    Greater_Than_Equal,
    Not_Equal,

    Add,
    Sub,
    Mul,
    Div,
    Mod,

    Bit_And,
    Bit_Or,
    Bit_Xor,
    Bit_Shift_Left,
    Bit_Shift_Right,

    Bit_Not,
    Not,

    Assign,
    Assign_Add,
    Assign_Sub,
    Assign_Mul,
    Assign_Div,
    Assign_Mod,
    Assign_Bit_And,
    Assign_Bit_Or,
    Assign_Bit_Xor,
    Assign_Bit_Shift_Left,
    Assign_Bit_Shift_Right,

    Range_Excl,
    Range_Incl,
}

Error :: enum u8 {
    None = 0,
    EOF,
    Illegal_Character,
}

Tokenizer :: struct {
    using pos:          Pos,
    data:               string,
    curr_rune:          rune,
    curr_rune_size:     int,
    curr_line_offset:   int,
    insert_semicolon:   bool,
}

tokenizer_make :: proc(data: string) -> (t: Tokenizer) {
    t = {
        pos = {line = 1},
        data = data,
    }
    next_rune(&t)
    if t.curr_rune == utf8.RUNE_BOM {
        next_rune(&t)
    }
    return t
}

next_rune :: proc(t: ^Tokenizer) -> rune {
    if t.offset >= len(t.data) {
        t.curr_rune = utf8.RUNE_EOF
    } else {
        t.offset += t.curr_rune_size
        t.curr_rune, t.curr_rune_size = utf8.decode_rune_in_string(t.data[t.offset:])
        t.pos.column = t.offset - t.curr_line_offset
        if t.offset >= len(t.data) {
            t.curr_rune = utf8.RUNE_EOF
        }
    }
    return t.curr_rune
}

peek_byte :: proc(t: ^Tokenizer) -> byte {
    if t.offset >= len(t.data) {
        return 0
    }
    return t.data[t.offset]
}

get_token :: proc(t: ^Tokenizer) -> (result: Token, err: Error) {
    // skip whitespace
    loop: for t.offset < len(t.data) {
        switch t.curr_rune {
        case ' ', '\t', '\v', '\f', '\r':
            next_rune(t)
        case '\n':
            if t.insert_semicolon {
                break loop
            }
            t.line += 1
            t.curr_line_offset = t.offset
            t.pos.column = 1
            next_rune(t)
        case:
            switch t.curr_rune {
            case 0x2028, 0x2029, 0xfeff:
                next_rune(t)
                continue loop
            }
            break loop
        }
    }

    result.pos = t.pos
    result.kind = .Invalid

    curr_rune := t.curr_rune
    next_rune(t)

    block: switch curr_rune {
    case utf8.RUNE_ERROR:
        err = .Illegal_Character

    case utf8.RUNE_EOF, '\x00':
        result.kind = .EOF
        err = .EOF

    case '\n':
        // If this is reached, treat a newline as if it is a semicolon
        t.insert_semicolon = false
        result.text = "\n"
        result.kind = .Semicolon
        t.line += 1
        t.curr_line_offset = t.offset
        t.pos.column = 1
        return

    case 'A'..='Z', 'a'..='z', '_':
        result.kind = .Ident
        for t.offset < len(t.data) {
            switch t.curr_rune {
            case 'A'..='Z', 'a'..='z', '0'..='9', '_':
                next_rune(t)
                continue
            }
            break
        }
        switch string(t.data[result.offset:t.offset]) {
        case "true": result.kind = .True
        case "false": result.kind = .False
        case "if":  result.kind = .If
        case "else":  result.kind = .Else
        case "for": result.kind = .For
        case "range": result.kind = .Range
        case "break":  result.kind = .Break
        case "continue":  result.kind = .Continue
        case "in":  result.kind = .In
        case "proc": result.kind = .Proc
        case "vector": result.kind = .Vector
        case "scalar": result.kind = .Scalar
        case "conv": result.kind = .Conv
        case "reinterpret": result.kind = .Reinterpret
        case "return": result.kind = .Return
        case "lanes": result.kind = .Lanes
        case "private": result.kind = .Private
        case "export": result.kind = .Export
        }

    case '@':
        result.kind = .Builtin
        // for t.offset < len(t.data) {
        //     switch t.curr_rune {
        //     case 'A'..='Z', 'a'..='z', '0'..='9', '_':
        //         next_rune(t)
        //         continue
        //     }
        //     break
        // }

    case '0'..='9':
        result.kind = .Integer
        if curr_rune == '0' && (t.curr_rune == 'x') {
            next_rune(t)
            for t.offset < len(t.data) {
                switch t.curr_rune {
                case '0'..='9', 'a'..='f', 'A'..='F':
                    next_rune(t)
                    continue
                }
                break
            }
        }
        for t.offset < len(t.data) {
            if t.curr_rune == '.' && t.offset + t.curr_rune_size < len(t.data) {
                next_byte := t.data[t.offset + t.curr_rune_size]
                if next_byte >= '0' && next_byte <= '9' {
                    next_rune(t)
                    result.kind = .Float
                    continue
                } else {
                    break
                }
            }
            if t.curr_rune >= '0' && t.curr_rune <= '9' {
                next_rune(t)
                continue
            }
            break
        }

    case '"':
        result.kind = .String
        for t.offset < len(t.data) {
            next_rune(t)
            if t.curr_rune == '"' {
                next_rune(t)
                break
            }
        }

    case '\'':
        result.kind = .Char
        num := 0
        for t.offset < len(t.data) {
            next_rune(t)
            if t.curr_rune == '\'' {
                next_rune(t)
                break
            }
            num += 1
        }
        if num != 1 {
            err = .Illegal_Character
        }

    case '=':
        result.kind = .Assign
        if t.curr_rune == '=' {
            next_rune(t)
            if t.curr_rune == '\n' || t. curr_rune < 0 {
                err = .Illegal_Character
                break
            }
            result.kind = .Equal
        }

    case '+':
        result.kind = .Add
        if t.curr_rune == '=' {
            next_rune(t)
            result.kind = .Assign_Add
        }

    case '-':
        result.kind = .Sub
        if t.curr_rune == '=' {
            next_rune(t)
            result.kind = .Assign_Sub
        }

    case '*':
        result.kind = .Mul
        if t.curr_rune == '=' {
            next_rune(t)
            result.kind = .Assign_Mul
        }

    case '%':
        result.kind = .Mod
        if t.curr_rune == '=' {
            next_rune(t)
            result.kind = .Assign_Mod
        }

    case '&':
        result.kind = .Bit_And
        if t.curr_rune == '=' {
            next_rune(t)
            result.kind = .Assign_Bit_And
        }

    case '|':
        result.kind = .Bit_Or
        if t.curr_rune == '=' {
            next_rune(t)
            result.kind = .Assign_Bit_Or
        }

    case '^':
        result.kind = .Bit_Xor
        if t.curr_rune == '=' {
            next_rune(t)
            result.kind = .Assign_Bit_Xor
        }

    case '~':
        result.kind = .Bit_Not

    case '.':
        result.kind = .Period
        if t.curr_rune == '.' {
            next_rune(t)
            if t.curr_rune == '<' {
                next_rune(t)
                result.kind = .Range_Excl
            } else if t.curr_rune == '=' {
                next_rune(t)
                result.kind = .Range_Incl
            } else {
                err = .Illegal_Character
            }
        }

    case ',': result.kind = .Comma
    case ';': result.kind = .Semicolon
    case ':': result.kind = .Colon
    case '{': result.kind = .Open_Brace
    case '}': result.kind = .Close_Brace
    case '(': result.kind = .Open_Paren
    case ')': result.kind = .Close_Paren
    case '[': result.kind = .Open_Bracket
    case ']': result.kind = .Close_Bracket

    case '<':
        result.kind = .Less_Than
        switch t.curr_rune {
        case '=':
            next_rune(t)
            result.kind = .Less_Than_Equal
        case '<':
            next_rune(t)
            result.kind = .Bit_Shift_Left
            if t.curr_rune == '=' {
                result.kind = .Assign_Bit_Shift_Left
            }
        }

    case '>':
        result.kind = .Greater_Than
        switch t.curr_rune {
        case '=':
            next_rune(t)
            result.kind = .Greater_Than_Equal
        case '>':
            next_rune(t)
            result.kind = .Bit_Shift_Right
            if t.curr_rune == '=' {
                result.kind = .Assign_Bit_Shift_Right
            }
        }

    case '!':
        result.kind = .Not
        if t.curr_rune == '=' {
            next_rune(t)
            result.kind = .Not_Equal
        }

    case '/':
        result.kind = .Div
        switch t.curr_rune {
        case '/':
            // Single-line comments
            for t.offset < len(t.data) {
                r := next_rune(t)
                if r == '\n' {
                    break
                }
            }
            return get_token(t)
        }

    case:
        err = .Illegal_Character
    }

    #partial switch result.kind {
    case .Invalid:
        // preserve
    case
        .Ident,
        .Integer,
        .String,
        .Float,
        .Break,
        .Continue,
        .Close_Brace,
        .Close_Paren:
        t.insert_semicolon = true
    case:
        t.insert_semicolon = false
    }

    result.text = string(t.data[result.offset:t.offset])
    return result, err
}