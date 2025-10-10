bits 64
default rel
section .text
    extern  ExitProcess, GetStdHandle, WriteFile
    global  main

main:
    push rbp
    mov  rbp, rsp
    sub rsp, 32
;  bool b1 = false;
    mov rax, 0
    mov [rbp-8], rax
;  bool b2 = true;
    mov rax, 1
    mov [rbp-16], rax
;  bool b3 = false;
    mov rax, 0
    mov [rbp-24], rax
;  int i1 = 144;
    mov rax, 144
    mov [rbp-32], rax
    mov rax, [rbp-32]
    mov rsp, rbp
    pop rbp
    ret