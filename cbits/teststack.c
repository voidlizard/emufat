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

