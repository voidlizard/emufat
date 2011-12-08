
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

void _emufat_decode_debug(uint32_t *a, uint32_t *atop, uint32_t *r, uint32_t *rtop, char *outp, char *out) {
    fprintf(stderr, "\n*** DEBUG CALLBACK\n\n");
}

extern  int runDecode(uint8_t * code, const int bsize, char * out);

int main() {
    const int blen = 512;
    char buf[512] = { 0 };
    uint8_t opcodes[] = { 49, 50 };

    runDecode(opcodes, blen, buf);

    return 0;
}


