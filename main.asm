        global main
        extern GetStdHandle
        extern GetLastError
        extern ExitProcess
        extern WriteFile
        section .text
main:
        sub rsp, 16
        lea rax, [rsp + 8]
        mov [rsp + 0], rax
        sub rsp, 8
        mov rax, 0
        mov [rsp + 0], rax
        sub rsp, 8
        mov rax, 4
        mov [rsp + 0], rax
        sub rsp, 8
        sub rsp, 8
        mov rax, [rsp + 32]
        mov [rsp + 0], rax
        mov rax, [rsp + 16]
        mov rcx, 1
        sub rax, rcx
        mov rcx, rax
        mov rax, [rsp + 0]
        add rsp, 8
        add rax, rcx
        mov [rsp + 0], rax
        sub rsp, 8
        mov al, 65
        mov [rsp + 0], al
.label0:
        mov rax, [rsp + 16]
        mov rcx, 0
        cmp rax, rcx
        jle .label1
        mov rax, [rsp + 8]
        mov cl, [rsp + 0]
        mov [rax + 0], cl
        mov rax, [rsp + 8]
        mov rcx, 1
        sub rax, rcx
        mov [rsp + 8], rax
        mov rax, [rsp + 0]
        mov rcx, 1
        add rax, rcx
        mov [rsp + 0], rax
        mov rax, [rsp + 24]
        mov rcx, 1
        add rax, rcx
        mov [rsp + 24], rax
        mov rax, [rsp + 16]
        mov rcx, 1
        sub rax, rcx
        mov [rsp + 16], rax
        jmp .label0
.label1:
        mov rax, [rsp + 24]
        mov rcx, 0
        cmp rax, rcx
        jle .label2
        sub rsp, 8
        sub rsp, 40
        mov rax, -11
        mov [rsp + 0], rax
        mov rcx, [rsp + 0]
        call GetStdHandle
        add rsp, 40
        mov [rsp + 0], rax
        mov rax, [rsp + 0]
        mov rcx, -1
        cmp rax, rcx
        jne .label3
        sub rsp, 40
        sub rsp, 32
        call GetLastError
        add rsp, 32
        mov [rsp + 0], rax
        mov rcx, [rsp + 0]
        call ExitProcess
        add rsp, 40
.label3:
        sub rsp, 8
        sub rsp, 48
        lea rax, [rsp + 48]
        mov [rsp + 24], rax
        mov rax, 0
        mov [rsp + 32], rax
        mov rcx, [rsp + 56]
        mov rdx, [rsp + 96]
        mov r8, [rsp + 88]
        mov r9, [rsp + 24]
        call WriteFile
        add rsp, 48
        mov rax, rax
        mov rcx, 0
        cmp rax, rcx
        jne .label4
        sub rsp, 32
        sub rsp, 32
        call GetLastError
        add rsp, 32
        mov [rsp + 0], rax
        mov rcx, [rsp + 0]
        call ExitProcess
        add rsp, 32
.label4:
        add rsp, 16
.label2:
        add rsp, 48
        mov rcx, 0
        sub rsp, 40
        call ExitProcess
