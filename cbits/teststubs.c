/******************************************************************************
 Copyright (c) 2014, Dmitry Zuikov
 All rights reserved.

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * Neither the name of emufat nor the names of its
   contributors may be used to endorse or promote products derived from
   this software without specific prior written permission.

 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
 FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

******************************************************************************/

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

