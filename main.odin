package vecc

import "base:intrinsics"
import "core:unicode/utf8"
import "core:os"
import "core:fmt"
import "core:strings"
import "core:strconv"
import "core:path/filepath"
import "core:reflect"
import "core:slice"
import "core:mem/virtual"

main :: proc() {
    if len(os.args) != 2 {
        fmt.println("Enter source code file name")
        return
    }
    src_file := os.args[1]
    data, ok := os.read_entire_file_from_filename(src_file)
    if !ok {
        fmt.println("Failed to open file")
        return
    }

    allocator := context.allocator

    arena: virtual.Arena
    if virtual.arena_init_growing(&arena) == nil {
        allocator = virtual.arena_allocator(&arena)
    }

    context.allocator = allocator

    parser: Parser = {
        filename = src_file,
        tokenizer = tokenizer_make(string(data)),
    }

    next(&parser)

    parse_file(&parser)

    checker: Checker = {
        filename = src_file,
        curr_scope = parser.file_scope,
        curr_file_scope = parser.file_scope,
        curr_lanes = 1,
    }

    check_program(&checker)

    gen: Gen = {
        curr_scope = parser.file_scope,
        curr_file_scope = parser.file_scope,
        types = checker.types[:],
        basic_types = checker.basic_types,
    }
    strings.builder_init_len_cap(&gen.source, 0, len(data))

    gen_program(&gen)

    dst_dir, dst_file := filepath.split(src_file)

    dst_source := fmt.tprintf("{}{}.h", dst_dir, filepath.stem(dst_file))
    fmt.println("WRITING", dst_source)

    os.write_entire_file(dst_source, gen.source.buf[:])
    fmt.println("DONE")
}



find_entity :: proc(scope: ^Scope, name: string) -> (^Entity, ^Scope, bool) {
    for s := scope; s != nil; s = s.parent {
        ent := s.entities[name] or_continue
        return ent, s, true
    }
    return nil, nil, false
}

// Ensure the integer can be safely cast into 'Dst'
// type without overflowing
int_cast :: #force_inline proc($Dst: typeid, val: $Src) -> Dst
where intrinsics.type_is_integer(Dst) && intrinsics.type_is_integer(Src) {
    assert(Src(Dst(val)) == val)
    return Dst(val)
}