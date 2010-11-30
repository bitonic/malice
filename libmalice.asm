; libmalice

; Code for Linux on IA-32:

section .text ; start of code
global _start ; export the main function

_start:
call _main
mov ebx, eax
mov eax, 1
int 0x80
