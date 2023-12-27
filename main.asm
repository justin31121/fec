        global main
        extern ExitProcess
        extern GetStdHandle

        section .text:
main:
        ;; STMT_DECLARATION
        ;;     name="foo"
        ;;     type=u64
        sub rsp, 16

        ;; STMT_ASSIGN
        ;; EXPR_CONSTANT
        ;;         value=69
        mov qword [rsp + 16], 69

        ;; EXPR_CONSTANT
        ;;         value=69
        push qword 69
        ;; EXPR_CONSTANT
        ;;         value=2
        push qword 2

        ;; EXPR_VARIABLE
        ;;         name="foo"
        mov rcx, [rsp + 32]

        add rsp, 32

        mov rcx, 0
        sub rsp, 8
        call ExitProcess
