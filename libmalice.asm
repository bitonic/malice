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



global _print_string
_print_string:	; void printString(char *string)
push eax	; will be: syscall number
push ebx 	; will be: stdout fd
push ecx 	; will be: character start address
push edx	; will be: character counter

mov  eax, 0		; prepare for holding a char
mov  ecx, [esp+20]	; string start address
mov  edx, -1		; init char counter to 0

_print_string_loop:
	inc  edx		; char_counter++
	mov  al, [ecx+edx]	; check next char
cmp al, 0			; if != '\0' continue
jne _print_string_loop

mov  ebx, 1	; stdout fd
mov  eax, 4	; write()
int 0x80

pop edx
pop ecx
pop ebx
pop eax
ret


global _print_int
_print_int:	; void printInt(int num)
push eax
push ecx
push edi
mov eax, [esp+16]	; num
sub esp, 12		; make space for converted integer
lea edi, [esp+11]	; string offset counter

mov [esp+11], byte 0	; make sure string is terminated
mov ecx, 10		; always divide by 10


cmp [esp+28], dword 0
jb _print_int_loop
neg eax

_print_int_loop:
	mov edx, 0xFFFFFFFF
	idiv ecx
	add edx, 0x30	; generate ASCII digit
	dec edi		; save next char
	mov [edi], dl
test eax, eax
jne _print_int_loop

cmp [esp+28], dword 0	; check for negative number
jge _print_int_positive
dec edi
mov [edi], byte '-'

_print_int_positive:
push edi
call _print_string
add esp, 4

add esp, 12
pop edi
pop ecx
pop eax
ret



section .data

_linect: dd 0
