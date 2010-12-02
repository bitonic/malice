; libmalice

; Code for Linux on IA-32:

%macro printstring 2
	mov eax, 0x4	; write()
	mov ebx, 1		; to stdout
	mov ecx, %1		; [string]
	mov edx, %2		; string len
	int 0x80
%endmacro


extern _main

section .text ; start of code
global _start ; export the main function



_start:
call _main
mov ebx, eax
mov eax, 1
int 0x80


global _checkarr
_checkarr:
mov eax, 1
ret

global _readint
_readint:
push _readintimpl
call _print_string
mov eax, 0
ret
_readintimpl: db "Implement _readint.",0x0a, 0

_print_string:		; void printString(char *string)
push eax	; will be: syscall number
push ebx ; will be: stdout fd
push ecx ; will be: character start address
push edx	; will be: character counter

mov  eax, 0			; prepare for holding a char
mov  ecx, [esp+20]	; string start address
mov  edx, -1		; init char counter to 0

_print_string_loop:
	inc  edx			; char_counter++
	mov  al, [ecx+edx]	; check next char
cmp al, 0				; if != '\0' continue
jne _print_string_loop

mov  ebx, 1	; stdout fd
mov  eax, 4	; write()
int 0x80

pop edx
pop ecx
pop ebx
pop eax
ret


section .data

_linect: dd 0
