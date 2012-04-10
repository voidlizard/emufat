
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "opcodes.h"
#include "emufatstubs.h"

void _emufat_decode_debug(uint32_t *a, uint32_t *atop, uint32_t *r, uint32_t *rtop, char *outp, char *out) {
    fprintf(stderr, "\n*** DEBUG CALLBACK\n\n");
    fprintf(stderr, "A: %08X\n", *atop);
}

int runDecode(uint32_t n, uint8_t * code, const int bsize, char * out, emufat_cb cb[256]);

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

static emufat_cb cb[256] = { { .fn = 0, .clos = 0} };

void cb1(void *clos, char *buf) {
    const char hello[] = "HELLO WORLD";
    memcpy(buf, hello, sizeof(hello));
}

int main(int argc, char **argv) {
    const int blen = SECTOR;
    unsigned char buf[SECTOR] = { 0 };
    size_t read = 0;
    int n  = 0;

    // 0 for streams, 1 for normal files, 2 testing hello world
    cb[2].fn = cb1;

    if( argc > 1 ) n = atoi(argv[1]);

    read = fread(opcodes, 1, MAXOPCODES, stdin);
    runDecode(n, opcodes, blen, buf, &cb[0]);
    fwrite(buf, 1, blen, stdout);

    return 0;
}

