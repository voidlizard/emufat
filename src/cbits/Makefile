.DEFAULT: all

all: test

test: opcodes
	../FatGen stubs > stubs.c
	gcc -g ./stubs.c ./teststubs.c -o teststubs

opcodes:
	echo "#ifndef __opcodes_h" > opcodes.h
	echo "#define __opcodes_h" >> opcodes.h
	echo "#endif" >> opcodes.h
	../FatGen opcodes >> opcodes.h
	echo "" >> opcodes.h
