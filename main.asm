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
        mov qword [rsp + 0], 0
        sub rsp, 8
        mov rax, [rsp + 16]
        mov [rsp + 0], rax
        mov rax, [rsp + 0]
        mov qword [rax + 0], 65
        mov rax, [rsp + 0]
        mov qword rcx, 1
        add rax, rcx
        mov [rsp + 0], rax
        mov rax, [rsp + 0]
        mov qword [rax + 0], 66
        mov rax, [rsp + 0]
        mov qword rcx, 1
        add rax, rcx
        mov [rsp + 0], rax
        mov rax, [rsp + 0]
        mov qword [rax + 0], 67
        mov rax, [rsp + 0]
        mov qword rcx, 1
        add rax, rcx
        mov [rsp + 0], rax
        mov rax, [rsp + 0]
        mov qword [rax + 0], 10
        mov rax, [rsp + 8]
        mov qword rcx, 4
        add rax, rcx
        mov [rsp + 8], rax
        mov rax, [rsp + 8]
        mov qword rcx, 0
        cmp rax, rcx
        jle .label0
        sub rsp, 8
        sub rsp, 40
        mov qword [rsp + 0], -11
        mov rcx, [rsp + 0]
        call GetStdHandle
        add rsp, 40
        mov [rsp + 0], rax
        mov rax, [rsp + 0]
        mov qword rcx, -1
        cmp rax, rcx
        jne .label1
        sub rsp, 40
        sub rsp, 32
        call GetLastError
        add rsp, 32
        mov [rsp + 0], rax
        mov rcx, [rsp + 0]
        call ExitProcess
        add rsp, 40
.label1:
        sub rsp, 8
        sub rsp, 48
        lea rax, [rsp + 48]
        mov [rsp + 24], rax
        mov qword [rsp + 32], 0
        mov rcx, [rsp + 56]
        mov rdx, [rsp + 80]
        mov r8, [rsp + 72]
        mov r9, [rsp + 24]
        call WriteFile
        add rsp, 48
        mov rax, rax
        mov qword rcx, 0
        cmp rax, rcx
        jne .label2
        sub rsp, 32
        sub rsp, 32
        call GetLastError
        add rsp, 32
        mov [rsp + 0], rax
        mov rcx, [rsp + 0]
        call ExitProcess
        add rsp, 32
.label2:
        add rsp, 16
.label0:
        add rsp, 32
        mov rcx, 0
        sub rsp, 40
        call ExitProcess
