        global entry                        ; (define (f) 1)      (f)  
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

        mov rax, [rsp + 0]                     ;; compile variable
        mov r11, 1                             ;<-|
        cmp r11, 1                             ;  compile app tail -> compile es res arity check
        jne raise_error_align                  ;  |
        push rax                               ;<-|
        mov r10, 0
        mov r8, [rsp + 0]
        mov [rsp + 8], r8                      ; move args
        add rsp, 8                             ; 
        mov rax, [rsp + 0]
        mov r9, rax                            ; assert proc rax
        and r9, 7
        cmp r9, 5
        jne raise_error_align               ;<-|
        xor rax, 5                          ; xor rax type proc
        mov rax, [rax + 0]                  ; Mov rax (Offset rax 0)
        jmp rax
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
