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
extern malloc
extern free

section .text ; start of code
global _start ; export the main function



_start:
mov [__start_esp], esp
call _main
_start_end:
mov esp, [__start_esp]
call _garbage_free_all
mov ebx, eax
mov eax, 1
int 0x80





; ALLOCATION function for new garbage

global _malice_alloc
_malice_alloc:		; void* maliceAlloc(int size, int sourceline)
push dword [esp+4]
call malloc
add esp, 4
test eax, eax
jne _malice_alloc_ok	; if memory allocated -> OK, return it

push _str_paragraph_2
push dword [esp+8+4]
push _str_paragraph_1
push _str_alloc_2
push dword [esp+4+16]
push _str_alloc_1

call _print_string
add esp, 4
call _print_int
add esp, 4
call _print_string
add esp, 4
call _print_string
add esp, 4
call _print_int
add esp, 4
call _print_string
add esp, 4

mov eax, 1
jmp _start_end	; poor man's exception handling

_malice_alloc_ok:
push eax
call _garbage_add	; keep track of allocated memory
add esp, 4
ret





; CHECKING function for array bounds

global _check_arr
_check_arr:		; int checkArr(int sourceline, int *array, int item)
mov eax, [esp+8]	; get array address
mov eax, [eax]		; get array size (hidden element 0)
cmp [esp+12], eax
jg _check_arr_notok	; if item > size -> Not OK
cmp [esp+12], dword 0
jle _check_arr_notok	; if item <= 0 -> Not OK
jmp _check_arr_ok

_check_arr_notok:
push _str_paragraph_2
push dword [esp+4+4]
push _str_paragraph_1
push eax
push _str_abc_2
push dword [esp+12+20]
push _str_abc_1

call _print_string
add esp, 4
call _print_int
add esp, 4
call _print_string
add esp, 4
call _print_int
add esp, 4
call _print_string
add esp, 4
call _print_int
add esp, 4
call _print_string
add esp, 4

mov eax, 1
jmp _start_end	; poor man's exception handling

_check_arr_ok:
mov eax, [esp+12]
_check_arr_end:
ret





; PRINTING procedure for CHARS (8-bit in 32-bit, LSB in Intel byte order)

global _print_char
_print_char:	; void printChar(int char)
push eax	; will be: syscall number
push ebx 	; will be: stdout fd
push ecx 	; will be: character start address
push edx	; will be: character counter

mov  edx, 1	; print one char
lea  ecx, [esp+20]	; address of the char
mov  ebx, 1	; stdout fd
mov  eax, 4	; write()
int 0x80

pop edx
pop ecx
pop ebx
pop eax
ret





; PRINTING procedure for STRINGS (8-bit)

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





; PRINTING procedure for INTEGERS (signed, 32-bit)

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





; READING procedure for INTEGERS (signed, 32-bit)

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





; GARBAGE COLLECTION: Adding entry to head of single linked list

global _garbage_add
_garbage_add:	; void garbageAdd(void *addr)
push eax
push ebx

push dword 8
call malloc
add esp, 4

test eax, eax		; malloc() == 0?
jne _garbage_add_listok


push _str_gc_alloc_fail
call _print_string
add esp, 4

push dword [esp+12]	; free the address we were supposed to keep
call free		; track of so we can bail out correctly
add esp, 4

mov eax, 1
jmp _start_end	; poor man's exception handling


_garbage_add_listok:
mov ebx, [esp+12]
mov [eax], ebx		; store the new entry
mov ebx, [__alloc_list]
mov [eax+4], ebx	; store the tail of the list
mov [__alloc_list], eax	; update head address

pop ebx
pop eax
ret





; GARBAGE COLLECTION: Freeing the entire linked list of garbage

global _garbage_free_all
_garbage_free_all:	; void garbageFreeAll()
push eax	; eax is destroyed by free()'s inexistent return value
push ebx

mov ebx, [__alloc_list]	; get the party started

_garbage_free_all_next:
test ebx, ebx		; next entry == 0?
je _garbage_free_all_done


push dword [ebx]	; free the user mem
call free
add esp, 4

push ebx		; free the list entry mem
mov ebx, [ebx+4]	; get address of next element
call free
add esp, 4

jmp _garbage_free_all_next


_garbage_free_all_done:
mov [__alloc_list], dword 0	; update head address

pop ebx
pop eax
ret





section .data

_str_abc_1: db "Oh no! You wanted their ",0
; requested item number
_str_abc_2: db " piece, but they only had ",0
; array size
_str_paragraph_1: db ". Check paragraph ",0
; source code line
_str_paragraph_2: db " of the story again!",10,0

_str_alloc_1: db "Oh no! You wanted ",0
_str_alloc_2: db " many physical pieces, but they didn't have this many",0

_str_gc_alloc_fail: db "Error allocating memory for garbage tracker.",10,0

__start_esp: dd 0
__alloc_list: dd 0
