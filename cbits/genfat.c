
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <time.h>

#include "opcodes.h"

void _emufat_decode_debug(uint32_t *a, uint32_t *atop, uint32_t *r, uint32_t *rtop, char *outp, char *out) {
    fprintf(stderr, "\n*** DEBUG CALLBACK\n\n");
    fprintf(stderr, "A: %08X\n", *atop);
}

int runDecode(uint32_t n, uint8_t * code, const int bsize, char * out);

void dump(const int len, char *buf) {
    int i = 0, j = 0;
    for(i = 0; i < len; i++) {
        if( !(i%16) ) printf("\n");
        printf("%02X ", (unsigned char)buf[i]);
    }
    printf("\n");
    printf("\n");
}

#define MAXOPCODES 16384
#define SECTOR 512

static uint8_t opcodes[MAXOPCODES] = { EXIT };

int main(int argc, char **argv) {
    const int blen = SECTOR;
    unsigned char buf[SECTOR] = { 0 };
    size_t read = 0;
    uint32_t n  = 0;
    uint32_t i = 0;
    time_t t0 = 0;

    if( argc > 1 ) n = atoi(argv[1]);

    read = fread(opcodes, 1, MAXOPCODES, stdin);

    t0 = time(0);

    for(i = 0; i < n; i++ ) {
        time_t dt = 0;
        uint32_t kbs = 0;
       
        runDecode(i, opcodes, blen, buf);
        fwrite(buf, 1, blen, stdout);
        
        dt = time(0) - t0;
        kbs = dt ? (i*512/1024/dt) : 0;

        fprintf(stderr, "%10d / %-10d   (%6d kb/s)            \r", i, n, kbs);
    }
    fprintf(stderr, "                                                  \r");

    return 0;
}

