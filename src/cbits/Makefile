.DEFAULT: all

all: stubs opcodes test

test:
	gcc -O2 ./stubs.c ./teststubs.c -o teststubs

genfat: genfat.c
	gcc -O2 ./stubs.c ./genfat.c -o genfat

opcodes: 
	echo "#ifndef __opcodes_h" > opcodes.h
	echo "#define __opcodes_h" >> opcodes.h
	echo "#endif" >> opcodes.h
	../FatGen opcodes >> opcodes.h
	echo "" >> opcodes.h

stubs:
	../FatGen stubs > stubs.c
