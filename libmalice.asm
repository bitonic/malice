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





; PRINTING function for STRINGS (8-bit)

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





; PRINTING function for INTEGERS (signed, 32-bit)

global _print_int
_print_int:	; void printInt(int num)
push eax	; will be: dividend
push ebx	; will be: divisor
push ecx 	; will be: character start address
push edx	; will be: character counter
mov eax, [esp+20]	; load num
sub esp, 12		; make space for converted integer
lea ecx, [esp+11]	; string offset counter, start at lastchar+1
			; so writing ends at 10 and char 11 is reserved

mov ebx, 10		; always divide by 10

cmp eax, dword 0	; if the number is negative, negate
jge _print_int_loop
neg eax			; great fun at -2147483648. Overflow ftw!

_print_int_loop:
	mov edx, 0
	idiv ebx
	add edx, 0x30
	dec ecx		; write next char
	mov [ecx], dl
test eax, eax
jne _print_int_loop

cmp [esp+32], dword 0	; check for negative number
jge _print_int_end	; skip for positive
dec ecx
mov [ecx], byte '-'	; add - sign

_print_int_end:
lea edx, [esp+11]
sub edx, ecx	; number of chars
mov  ebx, 1	; stdout fd
mov  eax, 4	; write()
int 0x80	; let the number speak

add esp, 12
pop edx
pop ecx
pop ebx
pop eax
ret





; READING function for INTEGERS (signed, 32-bit)

global _read_int
_read_int:	; int readInt(void)
push eax
push ebx
push ecx
push edx
push esi	; negative number info
push edi	; actual number

sub esp, 4	; make room for character to be read

mov esi, 0	; 0 = positive
mov edi, 0	; start with 0


_read_int_next:
mov edx, 1	; number of chars
mov ecx, esp	; character buffer
mov ebx, 0	; stdin fd
mov eax, 3	; read()
int 0x80

cmp eax, 0
je _read_int_end	; End of input

mov eax, 0
mov al, [esp]

cmp al, '-'
jne _read_int_process_num
mov esi, 1
jmp _read_int_next

_read_int_process_num:
cmp al, 0x30
jb _read_int_end	; char < '0'
cmp al, 0x39
ja _read_int_end	; char > '9'

sub eax, 0x30
imul edi, 10	; shift old digits
add edi, eax	; add new digit

jmp _read_int_next


_read_int_end:
test esi, esi
jz _read_int_end2
neg edi

_read_int_end2:
mov eax, edi	; Return value: The number read

add esp, 4
pop edi
pop esi
pop edx
pop ecx
pop ebx
add esp, 4	; pop eax
ret





section .data

_linect: dd 0
