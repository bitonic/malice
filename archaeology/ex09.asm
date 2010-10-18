section .data    ;declare section
NULL   equ   0
line_number:  db  0
section .text
global main
main:
    sub esp, 4
    ;assingment begins!
    mov eax, ebp
    sub eax, 4
    mov ebx, dword 0
    mov [eax], ebx
    ;assingment deads!
    mov [line_number], dword 2
    mov ecx, [ebp -4]
    not dword ecx
    mov ebx, ecx
    jmp exit_program
    mov ebx, 0 ; return code, succesful

exit_program:
    mov eax, 1  ;Linux system call to return
    int 0x80
