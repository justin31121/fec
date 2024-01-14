        global main
        extern CreateFileA
        extern ExitProcess
        extern GetLastError
        extern GetFileSize
        extern GetStdHandle
        extern ReadFile
        extern WriteFile
        extern CloseHandle
        extern GetProcessHeap
        extern HeapAlloc
        section .text
main:
        sub rsp, 16
        lea rax, [rsp + 8]
        mov [rsp + 0], rax
        sub rsp, 4
        mov eax, 0
        mov [rsp + 0], eax
        sub rsp, 4
        mov eax, 4
        mov [rsp + 0], eax
        sub rsp, 8
        sub rsp, 8
        mov rax, [rsp + 24]
        mov [rsp + 0], rax
        mov rax, 0
        sub rsp, 4
        mov eax, [rsp + 20]
        mov [rsp + 0], eax
        mov ebx, 1
        mov eax, [rsp + 0]
        add rsp, 4
        sub eax, ebx
        mov rbx, rax
        mov rax, [rsp + 0]
        add rsp, 8
        add rax, rbx
        mov [rsp + 0], rax
        sub rsp, 1
        mov al, 65
        mov [rsp + 0], al
.label0:
        sub rsp, 4
        mov eax, [rsp + 13]
        mov [rsp + 0], eax
        mov ebx, 0
        mov eax, [rsp + 0]
        add rsp, 4
        cmp eax, ebx
        jle .label1
        mov rax, [rsp + 1]
        mov bl, [rsp + 0]
        mov [rax + 0], bl
        sub rsp, 8
        mov rax, [rsp + 9]
        mov [rsp + 0], rax
        mov rbx, 1
        mov rax, [rsp + 0]
        add rsp, 8
        sub rax, rbx
        mov [rsp + 1], rax
        sub rsp, 1
        mov al, [rsp + 1]
        mov [rsp + 0], al
        mov bl, 1
        mov al, [rsp + 0]
        add rsp, 1
        add al, bl
        mov [rsp + 0], al
        sub rsp, 4
        mov eax, [rsp + 17]
        mov [rsp + 0], eax
        mov ebx, 1
        mov eax, [rsp + 0]
        add rsp, 4
        add eax, ebx
        mov [rsp + 13], eax
        sub rsp, 4
        mov eax, [rsp + 13]
        mov [rsp + 0], eax
        mov ebx, 1
        mov eax, [rsp + 0]
        add rsp, 4
        sub eax, ebx
        mov [rsp + 9], eax
        jmp .label0
.label1:
        sub rsp, 4
        mov eax, [rsp + 17]
        mov [rsp + 0], eax
        mov ebx, 0
        mov eax, [rsp + 0]
        add rsp, 4
        cmp eax, ebx
        jle .label2
        sub rsp, 8
        sub rsp, 39
        mov eax, -11
        mov [rsp + 0], eax
        mov ecx, [rsp + 0]
        call GetStdHandle
        add rsp, 39
        mov [rsp + 0], rax
        sub rsp, 8
        mov rax, [rsp + 8]
        mov [rsp + 0], rax
        mov rbx, -1
        mov rax, [rsp + 0]
        add rsp, 8
        cmp rax, rbx
        jne .label3
        sub rsp, 39
        sub rsp, 32
        call GetLastError
        add rsp, 32
        mov [rsp + 0], al
        mov cl, [rsp + 0]
        call ExitProcess
        add rsp, 39
.label3:
        sub rsp, 4
        sub rsp, 1
        sub rsp, 50
        mov rax, 0
        mov [rsp + 32], rax
        mov rcx, [rsp + 55]
        mov rdx, [rsp + 80]
        mov r8d, [rsp + 76]
        lea r9, [rsp + 51]
        call WriteFile
        add rsp, 50
        mov [rsp + 0], al
        mov bl, 0
        mov al, [rsp + 0]
        add rsp, 1
        cmp al, bl
        jne .label4
        sub rsp, 35
        sub rsp, 32
        call GetLastError
        add rsp, 32
        mov [rsp + 0], al
        mov cl, [rsp + 0]
        call ExitProcess
        add rsp, 35
.label4:
        add rsp, 12
.label2:
        add rsp, 33
        mov rcx, 0
        sub rsp, 40
        call ExitProcess
