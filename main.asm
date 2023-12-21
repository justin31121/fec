        global main
        extern GetStdHandle
        extern ExitProcess
        section .text
main:
        sub rsp, 16

        mov rcx, -11
        sub rsp, 8
        call GetStdHandle
        add rsp, 8

        add rsp, 16

        mov rcx, 0
        sub rsp, 8
        call ExitProcess
