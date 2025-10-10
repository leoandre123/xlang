; xlang_runtime.asm  (Windows x64, NASM)
bits 64
default rel

section .data
__xlang_written: dq 0           ; scratch for WriteFile

section .text
extern  GetStdHandle, WriteFile
global  __xlang_print
global  __xlang_println
global  __xlang_print_i64
global  __xlang_print_hex_u64

; ------------------------------------------------------------
; void __xlang_print(const void* ptr, uint64 len)
; RCX=ptr, RDX=len
__xlang_print:
    push rbp
    mov  rbp, rsp
    sub  rsp, 32                ; locals: [rbp-8]=ptr, [rbp-16]=len
    mov  [rbp-8], rcx
    mov  [rbp-16], rdx

    ; HANDLE h = GetStdHandle(STD_OUTPUT_HANDLE)
    sub  rsp, 32
    mov  ecx, -11               ; STD_OUTPUT_HANDLE
    call GetStdHandle           ; RAX=h
    add  rsp, 32

    ; WriteFile(h, ptr, (DWORD)len, &written, NULL)
    sub  rsp, 32
    mov  rcx, rax               ; hFile
    mov  rdx, [rbp-8]           ; lpBuffer
    mov  r8,  [rbp-16]          ; nNumberOfBytesToWrite
    lea  r9,  [rel __xlang_written]
    mov  qword [rsp+32], 0      ; LPOVERLAPPED = NULL
    call WriteFile
    add  rsp, 32

    mov  rsp, rbp
    pop  rbp
    ret

; ------------------------------------------------------------
; void __xlang_println(const void* ptr, uint64 len)
; prints bytes then CRLF
__xlang_println:
    push rbp
    mov  rbp, rsp
    ; first print(ptr,len)
    sub  rsp, 32
    mov  [rbp-8], rcx
    mov  [rbp-16], rdx
    ; call __xlang_print(ptr,len)
    sub  rsp, 32
    mov  rcx, [rbp-8]
    mov  rdx, [rbp-16]
    call __xlang_print
    add  rsp, 32

    ; now write "\r\n"
    section .rdata
__xlang_crlf: db 13,10
    section .text
    sub  rsp, 32
    lea  rcx, [rel __xlang_crlf]
    mov  rdx, 2
    call __xlang_print
    add  rsp, 32

    mov  rsp, rbp
    pop  rbp
    ret

; ------------------------------------------------------------
; void __xlang_print_hex_u64(uint64 x)
; RCX=x  -> prints "0x" + 16 hex nibbles
__xlang_print_hex_u64:
    push rbp
    mov  rbp, rsp
    sub  rsp, 48                 ; 32 for locals + 16 for small buf
    lea  rdi, [rbp-32]           ; buffer at [rbp-32 .. -17]
    mov  byte [rdi+0], '0'
    mov  byte [rdi+1], 'x'
    mov  rax, rcx                ; value
    mov  r10, 16                 ; count
.hex_loop:
    mov  r11, rax
    and  r11, 0xF
    cmp  r11, 10
    sbb  r9,  r9                 ; r9 = CF? -1 : 0
    lea  r11, [r11 + '0']
    ;lea  r11, [r11 + r9*39]      ; add 39 if >=10 → 'A'..'F'
    mov  byte [rdi+2 + r10 - 1], r11b
    shr  rax, 4
    dec  r10
    jnz  .hex_loop

    ; print 18 bytes
    sub  rsp, 32
    lea  rcx, [rbp-32]
    mov  rdx, 18
    call __xlang_print
    add  rsp, 32

    mov  rsp, rbp
    pop  rbp
    ret

; ------------------------------------------------------------
; void __xlang_print_i64(int64 x)
; RCX=x -> prints decimal (no sign for now? -> includes sign)
__xlang_print_i64:
    push rbp
    mov  rbp, rsp
    sub  rsp, 64                 ; room for temp buffer & locals
    lea  rsi, [rbp-1]            ; write from end downward
    mov  rax, rcx                ; value
    mov  r8b, 0                  ; negative flag = 0

    ; handle sign
    test rax, rax
    jns  .abs_done
    neg  rax
    mov  r8b, 1
.abs_done:

    ; special case 0
    cmp  rax, 0
    jne  .conv
    mov  byte [rsi], '0'
    dec  rsi
    jmp  .conv_done

.conv:
    ; convert digits in reverse
    ; (use 128-bit idiv path: for 64-bit idiv, RDX:RAX / 10)
    ; but 64-bit fits in RAX, zero RDX each time
.conv_loop:
    xor  edx, edx
    mov  r9, 10
    div  r9                     ; RAX=quotient, RDX=remainder
    add  dl, '0'
    mov  byte [rsi], dl
    dec  rsi
    test rax, rax
    jnz  .conv_loop

.conv_done:
    cmp  r8b, 0
    je   .no_sign
    mov  byte [rsi], '-'
    dec  rsi
.no_sign:
    ; now [rsi+1 .. rbp-1] holds the string
    lea  rcx, [rsi+1]
    mov  rdx, rbp
    sub  rdx, rcx               ; len = &end - &start

    sub  rsp, 32
    call __xlang_println
    add  rsp, 32

    mov  rsp, rbp
    pop  rbp
    ret
