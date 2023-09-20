        global entry ; (define (f) 1) (apply f (cons 1 '()))
        default rel
        section .text
        extern peek_byte
        extern read_byte
        extern write_byte
        extern raise_error
        global entry
entry:
        mov rbx, rdi                           ; recv heap pointer
        lea rax, [rel label_f_5e96933745]      ; compile define values
        mov [rbx + 0], rax                     ; |
        mov rax, rbx                           ; |
        add rax, 0                             ; |
        or rax, 5                              ; |
        push rax                               ; |
        add rbx, 8                            ;<-|

                                                ; compile apply  (apply f es e)
        lea rax, [rel ret6412]                  ; (Lea rax r)
        push rax
        mov rax, 16                             ; compile es (cons 1 '()) compile the list arg
        mov r11, 1
        cmp r11, 1
        jne raise_error_align
        push rax
        mov rax, 152
        mov r11, 1
        cmp r11, 1
        jne raise_error_align
        mov [rbx + 0], rax
        pop rax
        mov [rbx + 8], rax
        mov rax, rbx
        or rax, 2
        add rbx, 16
        mov r10, 0                             ; Mov r10 (length es) 
loop6413:                                      ; push list
        cmp rax, 152
        je end6414
        mov r9, rax
        and r9, 7
        cmp r9, 2
        jne raise_error_align
        xor rax, 2
        mov r8, [rax + 8]
        push r8
        add r10, 1
        mov rax, [rax + 0]
        jmp loop6413
end6414:
        jmp label_f_5e96933745                 ; Jmp to fun
ret6412:
        add rsp, 8
        ret
label_f_5e96933745:
        cmp r10, 0
        jne raise_error_align
        mov rax, [rsp + 0]
        xor rax, 5
        mov rax, 16
        mov r11, 1
        add rsp, 8
        ret
raise_error_align:
        mov r15, rsp
        and r15, 8
        sub rsp, r15
        call raise_error
