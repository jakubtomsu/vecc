#include "test1.h"

void main() {
    printf("Entered main\n");
    vecc_main();
}

// Note: this is down here only to make sure none of the private procs get used
#define VECC_IMPL
#include "test1.h"