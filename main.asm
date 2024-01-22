        section .data
        ;; CONSTANT_STRING
        ;;     value="main.fe"
constant0: db 109,97,105,110,46,102,101,0
        extern ExitProcess
        extern CreateFileA
        extern GetLastError
        extern GetProcessHeap
        extern GetFileSize
        extern HeapAlloc
        extern ReadFile
        extern WriteFile
        extern GetStdHandle
        extern CloseHandle
        extern HeapFree
        global dump
        global foo
        global add
        global main
        global sub
        section .text
dump:
        sub rsp, 8
        sub rsp, 56
        mov eax, 2147483648
        mov [rsp + 8], eax
        mov eax, 1
        mov [rsp + 16], eax
        mov rax, 0
        mov [rsp + 24], rax
        mov eax, 3
        mov [rsp + 32], eax
        mov eax, 128
        mov [rsp + 40], eax
        mov rax, 0
        mov [rsp + 48], rax
        mov rcx, constant0
        mov edx, [rsp + 8]
        mov r8d, [rsp + 16]
        mov r9, [rsp + 24]
        call CreateFileA
        add rsp, 56
        mov [rsp + 0], rax
        sub rsp, 8
        mov rax, [rsp + 8]
        mov [rsp + 0], rax
        mov rbx, -1
        mov rax, [rsp + 0]
        add rsp, 8
        cmp rax, rbx
        jne .label0
        mov al, 0
        add rsp, 8
        ret
.label0:
        sub rsp, 8
        sub rsp, 32
        call GetProcessHeap
        add rsp, 32
        mov [rsp + 0], rax
        sub rsp, 8
        mov rax, [rsp + 8]
        mov [rsp + 0], rax
        mov rbx, 0
        mov rax, [rsp + 0]
        add rsp, 8
        cmp rax, rbx
        jne .label1
        mov al, 0
        add rsp, 16
        ret
.label1:
        sub rsp, 4
        sub rsp, 44
        mov rax, 0
        mov [rsp + 8], rax
        mov rcx, [rsp + 56]
        mov rdx, [rsp + 8]
        call GetFileSize
        add rsp, 44
        mov [rsp + 0], eax
        sub rsp, 4
        mov eax, [rsp + 4]
        mov [rsp + 0], eax
        mov ebx, 4294967295
        mov eax, [rsp + 0]
        add rsp, 4
        cmp eax, ebx
        jne .label2
        mov al, 0
        add rsp, 20
        ret
.label2:
        sub rsp, 8
        sub rsp, 36
        mov eax, 0
        mov [rsp + 8], eax
        mov rax, 0
        mov eax, [rsp + 44]
        mov [rsp + 16], rax
        mov rcx, [rsp + 48]
        mov edx, [rsp + 8]
        mov r8, [rsp + 16]
        call HeapAlloc
        add rsp, 36
        mov [rsp + 0], rax
        sub rsp, 8
        mov rax, [rsp + 8]
        mov [rsp + 0], rax
        mov rbx, 0
        mov rax, [rsp + 0]
        add rsp, 8
        cmp rax, rbx
        jne .label3
        mov al, 0
        add rsp, 28
        ret
.label3:
        sub rsp, 4
        sub rsp, 48
        mov rax, 0
        mov [rsp + 32], rax
        mov rcx, [rsp + 72]
        mov rdx, [rsp + 52]
        mov r8d, [rsp + 60]
        lea r9, [rsp + 48]
        call ReadFile
        add rsp, 48
        sub rsp, 48
        sub rsp, 32
        mov eax, -11
        mov [rsp + 0], eax
        mov ecx, [rsp + 0]
        call GetStdHandle
        add rsp, 32
        mov [rsp + 0], rax
        mov rax, 0
        mov [rsp + 32], rax
        mov rcx, [rsp + 0]
        mov rdx, [rsp + 52]
        mov r8d, [rsp + 60]
        lea r9, [rsp + 48]
        call WriteFile
        add rsp, 48
        sub rsp, 32
        mov eax, 0
        mov [rsp + 8], eax
        mov rcx, [rsp + 48]
        mov edx, [rsp + 8]
        mov r8, [rsp + 36]
        call HeapFree
        add rsp, 32
        sub rsp, 32
        mov rcx, [rsp + 56]
        call CloseHandle
        add rsp, 32
        mov al, 1
        add rsp, 32
        ret
foo:
        mov al, 35
        ret
add:
        mov [rsp + 8], rcx
        mov [rsp + 16], rdx
        mov al, [rsp + 8]
        mov bl, [rsp + 16]
        add al, bl
        ret
main:
        sub rsp, 1
        sub rsp, 47
        call dump
        add rsp, 47
        mov [rsp + 0], al
        mov bl, 0
        mov al, [rsp + 0]
        add rsp, 1
        cmp al, bl
        jne .label0
        sub rsp, 32
        sub rsp, 32
        call GetLastError
        add rsp, 32
        mov [rsp + 0], al
        mov cl, [rsp + 0]
        call ExitProcess
        add rsp, 32
.label0:
        sub rsp, 32
        sub rsp, 32
        sub rsp, 32
        call foo
        add rsp, 32
        mov [rsp + 0], al
        mov al, 34
        mov [rsp + 8], al
        mov cl, [rsp + 0]
        mov dl, [rsp + 8]
        call sub
        add rsp, 32
        mov [rsp + 0], al
        mov cl, [rsp + 0]
        call ExitProcess
        add rsp, 32
        ret
sub:
        mov [rsp + 8], rcx
        mov [rsp + 16], rdx
        mov al, [rsp + 8]
        mov bl, [rsp + 16]
        sub al, bl
        ret
