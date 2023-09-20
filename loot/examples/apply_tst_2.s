        global entry  ; (define (f) 1) (apply f (cons 1 '()))
        default rel
        section .text
        extern peek_byte
        extern read_byte
        extern write_byte
        extern raise_error
        global entry
entry:
        mov rbx, rdi
        lea rax, [rel label_f_5e96933745]
        mov [rbx + 0], rax
        mov rax, rbx
        add rax, 0
        or rax, 5
        push rax
        add rbx, 8
        mov rax, [rsp + 0]
        mov r11, 1
        mov rax, 16
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
        mov r10, 0
loop6413:
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
        mov rax, [rsp + 8]
        mov r9, rax
        and r9, 7
        cmp r9, 5
        jne raise_error_align
        xor rax, 5
        mov rax, [rax + 0]
        jmp rax
ret6412:
        add rsp, 8
        ret
label_f_5e96933745:
        cmp r10, 1
        jne raise_error_align
        mov rax, [rsp + 8]
        xor rax, 5
        mov rax, 16
        mov r11, 1
        add rsp, 16
        ret
raise_error_align:
        mov r15, rsp
        and r15, 8
        sub rsp, r15
        call raise_error
