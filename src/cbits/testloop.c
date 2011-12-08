
#include <stdint.h>
#include <stdio.h>


int main() {

    uint8_t opcodes[] = { 0x01, 0x02, 0x03, 0x04, 0x05, 0x00 };
    uint8_t *op = opcodes;

    for(; ; ++op) {
        printf("%02X\n", *op);
        if( *op == 0x00 ) break;
    }

    return 0;
}

