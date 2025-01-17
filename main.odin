package vecc

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
        curr_scope = parser.curr_scope,
        curr_lanes = 1,
    }
    strings.builder_init_len_cap(&checker.source, 0, len(data))
    
    check_program(&checker)

    dst_source := fmt.tprintf("{}.h", filepath.short_stem(src_file))
    
    os.write_entire_file(dst_source, checker.source.buf[:])
    fmt.println("DONE")
}