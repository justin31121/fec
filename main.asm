        global main
        extern CreateFileA
        extern ExitProcess
        extern GetLastError
        extern GetFileSize
        extern GetStdHandle
        extern ReadFile
        extern WriteFile
        extern CloseHandle

        section .data
        ;; CONSTANT_STRING
        ;;     value="main.fe"
constant0: db 109,97,105,110,46,102,101,0

        section .text
main:
        sub rsp, 8
        sub rsp, 56
        mov rax, constant0
        mov [rsp + 0], rax
        mov rax, 2147483648
        mov [rsp + 8], rax
        mov rax, 1
        mov [rsp + 16], rax
        mov rax, 0
        mov [rsp + 24], rax
        mov rax, 3
        mov [rsp + 32], rax
        mov rax, 128
        mov [rsp + 40], rax
        mov rax, 0
        mov [rsp + 48], rax
        mov rcx, [rsp + 0]
        mov rdx, [rsp + 8]
        mov r8, [rsp + 16]
        mov r9, [rsp + 24]
        call CreateFileA
        add rsp, 56
        mov [rsp + 0], rax
        mov rax, [rsp + 0]
        mov rcx, -1
        cmp rax, rcx
        jne .label0
        sub rsp, 40
        sub rsp, 32
        call GetLastError
        add rsp, 32
        mov [rsp + 0], eax
        mov rcx, [rsp + 0]
        call ExitProcess
        add rsp, 40
.label0:
        sub rsp, 8
        sub rsp, 32
        mov rax, 0
        mov [rsp + 8], rax
        mov rcx, [rsp + 40]
        mov rdx, [rsp + 8]
        call GetFileSize
        add rsp, 32
        mov [rsp + 0], eax
        sub rsp, 4
        sub rsp, 16
        lea rax, [rsp + 8]
        mov [rsp + 0], rax
        sub rsp, 8
        sub rsp, 8
        sub rsp, 44
        mov rax, -11
        mov [rsp + 0], rax
        mov rcx, [rsp + 0]
        call GetStdHandle
        add rsp, 44
        mov [rsp + 0], rax
.label1:
        mov rax, [rsp + 36]
        mov rcx, 0
        cmp rax, rcx
        je .label2
        mov rax, 8
        mov [rsp + 8], rax
        mov rax, [rsp + 36]
        mov rcx, [rsp + 8]
        cmp rax, rcx
        jge .label3
        mov rax, [rsp + 36]
        mov [rsp + 8], rax
.label3:
        sub rsp, 44
        mov rax, [rsp + 52]
        mov [rsp + 16], rax
        lea rax, [rsp + 76]
        mov [rsp + 24], rax
        mov rax, 0
        mov [rsp + 32], rax
        mov rcx, [rsp + 88]
        mov rdx, [rsp + 60]
        mov r8, [rsp + 16]
        mov r9, [rsp + 24]
        call ReadFile
        add rsp, 44
        mov rax, [rsp + 36]
        mov ecx, [rsp + 32]
        sub rax, rcx
        mov [rsp + 36], rax
        sub rsp, 44
        mov eax, [rsp + 76]
        mov [rsp + 16], eax
        lea rax, [rsp + 76]
        mov [rsp + 24], rax
        mov rax, 0
        mov [rsp + 32], rax
        mov rcx, [rsp + 44]
        mov rdx, [rsp + 60]
        mov r8, [rsp + 16]
        mov r9, [rsp + 24]
        call WriteFile
        add rsp, 44
        jmp .label1
.label2:
        sub rsp, 44
        mov rcx, [rsp + 88]
        call CloseHandle
        add rsp, 44
        sub rsp, 44
        mov rax, [rsp + 80]
        mov [rsp + 0], rax
        mov rcx, [rsp + 0]
        call ExitProcess
        add rsp, 44
        add rsp, 52
        mov rcx, 0
        sub rsp, 40
        call ExitProcess
