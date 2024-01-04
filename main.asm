        global main
        extern GetStdHandle
        extern ExitProcess
        section .text
main:
        sub rsp, 8
        sub rsp, 40
        mov qword [rsp + 0], -11
        mov rcx, [rsp + 0]
        call GetStdHandle
        add rsp, 40
        mov [rsp + 0], rax
        mov rcx, 0
        sub rsp, 40
        call ExitProcess
