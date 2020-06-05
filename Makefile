hello: hello.o
	ld -m elf_x86_64 -o hello hello.o

hello.o: hello.asm
	nasm -felf64 -g -o hello.o hello.asm

hello.asm: forthf
	./forthf > hello.asm

forthf: main.ml
	ocamlopt -o forthf main.ml
