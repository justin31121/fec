        global main
        extern ExitProcess
        extern GetStdHandle
        extern WriteFile

        section .data
        ;; CONSTANT_STRING
        ;;     value="Hello World!"
constant_000001E870872810: db 72,101,108,108,111,32,87,111,114,108,100,33,10
        ;; CONSTANT_U64
        ;;     value=13
%define constant_000001E870872820 13

        section .text:
main:
        ;; STMT_DECLARATION
        ;;     name="handle"
        ;;     type=void*
        sub rsp, 8
        ;; STMT_ASSIGN
        ;;     EXPR_FUNCCALL
        ;;     EXPR_VALUE
        ;;         value=-11
        mov qword rcx, -11
        sub rsp, 40
        call GetStdHandle
        add rsp, 40
        mov [rsp + 0], rax
        ;; STMT_IF
        ;;     EXPR_VARIABLE
        ;;         name="handle"
        mov rax, [rsp + 0]
        ;;     EXPR_VALUE
        ;;         value=-1
        mov qword rcx, -1
        cmp rax, rcx
        jne .label_000001E870872D00
        ;; STMT_FUNCCALL
        ;;     EXPR_VALUE
        ;;         value=1
        mov qword rcx, 1
        sub rsp, 40
        call ExitProcess
        add rsp, 40
.label_000001E870872D00:
        ;; STMT_DECLARATION
        ;;     name="written"
        ;;     type=u64
        sub rsp, 8
        ;; STMT_IF
        ;;     EXPR_FUNCCALL
        ;;     EXPR_VARIABLE
        ;;         name="handle"
        mov rcx, [rsp + 8]
        ;;     EXPR_CONSTANT
        mov rdx, constant_000001E870872810
        ;;     EXPR_CONSTANT
        mov r8, constant_000001E870872820
        ;;     EXPR_VARIABLE_PTR
        lea r9, [rsp + 0]
        ;;     EXPR_VALUE
        ;;         value=0
        push qword 0
        sub rsp, 40
        call WriteFile
        add rsp, 48
        ;;     EXPR_VALUE
        ;;         value=0
        mov qword rcx, 0
        cmp rax, rcx
        jne .label_000001E870872DC0
        ;; STMT_FUNCCALL
        ;;     EXPR_VALUE
        ;;         value=1
        mov qword rcx, 1
        sub rsp, 32
        call ExitProcess
        add rsp, 32
.label_000001E870872DC0:

        add rsp, 16
        mov rcx, 0
        sub rsp, 40
        call ExitProcess
