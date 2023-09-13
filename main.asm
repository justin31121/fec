    global _main
    extern ExitProcess
    extern GetStdHandle
    extern WriteFile

    section .text
_main:
    sub rsp, 8+8

    ;; message := ...
    mov rdi, message_len
    push rdi 
    mov rdi, message
    push rdi 

    ;; exitCode := ...
    push 69

    ;; print
    mov rcx, -11
    call GetStdHandle
    mov rdi, qword [rsp + 16]
    push rdi
    mov rdi, qword [rsp + 16]
    push rdi
    pop rdx
    pop r8
    sub rsp, 40
    mov rcx, rax
    lea r9, [rsp]
    mov qword [rsp + 32], 0
    call WriteFile
    add rsp, 40

    ;; exit
    mov rdi, qword [rsp + 0]
    push rdi
    pop rcx
    call ExitProcess

    section .data
message:         db 'Hello, World!'
message_len:     equ $-message
