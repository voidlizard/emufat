.DEFAULT: all

all: stubs opcodes test

test: stubs.c teststubs.c opcodes.h
	gcc -g ./stubs.c ./teststubs.c -o teststubs

genfat: genfat.c stubs.c opcodes.h
	gcc -O2 ./stubs.c ./genfat.c -o genfat

opcodes:
	echo "#ifndef __opcodes_h" > opcodes.h
	echo "#define __opcodes_h" >> opcodes.h
	echo "#endif" >> opcodes.h
	../dist/build/FatGen/FatGen opcodes >> opcodes.h
	echo "" >> opcodes.h

stubs:
	../dist/build/FatGen/FatGen stubs > stubs.c

