#include <HsFFI.h>
#include "MyFib_stub.h"
#include <stdio.h>

int main(int argc, char *argv[])
{
    int i;
    hs_init(&argc, &argv);

    i = fib_hs(42);
    printf("Fibonacci: %d\n", i);

    hs_exit();
    return 0;
}
