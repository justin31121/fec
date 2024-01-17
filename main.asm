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

        section .data
        ;; CONSTANT_STRING
        ;;     value="Hello, World!"
constant0: db 72,101,108,108,111,44,32,87,111,114,108,100,33,10,0
        ;; CONSTANT_S64
        ;;     value=14
%define constant1 14

        section .text
main:
        sub rsp, 16
        mov rax, constant0
        mov [rsp + 0], rax
        mov rax, constant1
        mov [rsp + 8], rax
        sub rsp, 4
        sub rsp, 44
        sub rsp, 32
        mov eax, -11
        mov [rsp + 0], eax
        mov ecx, [rsp + 0]
        call GetStdHandle
        add rsp, 32
        mov [rsp + 0], rax
        mov eax, [rsp + 56]
        mov [rsp + 16], eax
        mov rax, 0
        mov [rsp + 32], rax
        mov rcx, [rsp + 0]
        mov rdx, [rsp + 48]
        mov r8d, [rsp + 16]
        lea r9, [rsp + 44]
        call WriteFile
        add rsp, 44
        add rsp, 20
        mov rcx, 0
        sub rsp, 40
        call ExitProcess
