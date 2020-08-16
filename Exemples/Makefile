%.o: %.asm
	nasm -f elf64 -g -o "$@" "$<"

%.out: %.o
	gcc -no-pie -ggdb -o "$@" "$<" -lc -lm

debug_gdb: debug.out
# Disabling pagination is useful to avoid a strange pagination when running run at startup
	gdb --eval-command "set pagination off" --eval-command "set disassembly-flavor intel" --eval-command "layout asm" --eval-command "b main" --eval-command "run" debug.out

.PHONY: debug_gdb
