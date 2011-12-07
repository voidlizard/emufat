
#include <stdint.h>
#include <stdio.h>

#define DEFSTACK(n, t, l) t n[(l)], *n##top = n
#define RESET(a)   (a##top) = a
#define PTOP(a)    (a##top)
#define TOP(a)     (*(a##top))
#define POP(a)     (*(a##top)--)
#define PUSH(a,v) (*(++(a##top)) = v)

int main() {
    int i = 0;
    DEFSTACK(a, uint32_t, 32);

    TOP(a) = 0;

    PUSH(a, 0xDEADBEEF); 

    printf("%08X\n", TOP(a));

    PUSH(a, 0xCAFEBABE);

    printf("%08X\n", TOP(a));

    POP(a);

    printf("%08X\n", TOP(a));

    PUSH(a, 1);
    PUSH(a, 2);
    PUSH(a, 3);
    PUSH(a, 4);
    PUSH(a, 5);
    PUSH(a, 6);

    while(PTOP(a) != a) {
        printf("A: %08X\n", POP(a));
    }

    return 0;
}

