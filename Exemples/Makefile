%.o: %.asm
	nasm -f elf64 -g -o "$@" "$<"

%.out: %.o
	gcc -no-pie -ggdb -o "$@" "$<" -lc -lm

