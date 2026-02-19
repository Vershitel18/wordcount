SYS_EXIT:               equ                     60
SYS_WRITE:              equ                     1
STDOUT:                 equ                     1
STDERR:                 equ                     2
EXIT_FAILURE:           equ                     1
section                 .text
                        global                  _start
_start:
                        mov                     r8, 1 ; last character is whitespace
                        xor                     r9, r9 ; word count
                        xor                     edi, edi ; STDIN
                        mov                     rsi, buffer
                        mov                     rdx, BUFFER_LEN
.read_loop:
                        xor                     eax, eax ; SYS_READ
                        syscall
                        test                    rax, rax
                        jz                      .eof
                        js                      print_error
                        xor                     ecx, ecx ; index in buffer
.next_byte:
                        cmp     rcx, rax
                        jz     .read_loop
                        mov     r10b, [buffer + rcx]
                        sub     r10b, 9 ; check if byte is a whitespace character [9, 13] -> [0, 4]
                        cmp     r10b, 4
                        jbe     .whitespace
                        cmp     r10b, 32 - 9 ; check if byte is space character
                        je      .whitespace
.not_whitespace:
                        add     r9, r8
                        xor     r8, r8; reset last character flag
                        inc     rcx
                        jmp     .next_byte
.whitespace:
                        mov     r8, 1 ; last character is whitespace
                        inc     rcx
                        jmp     .next_byte
.eof:
                        call    print
                        mov     eax, SYS_EXIT
                        xor     edi, edi
                        syscall
print:
                        mov                     r10d, 10
                        mov                     rax, r9
                        lea                     rsi, [rsp - 1]
                        mov                     BYTE [rsi], 10; null terminator for string
.next_digit:
                        dec                     rsi
                        xor                     edx, edx
                        div                     r10
                        add                     dl, '0'
                        mov                     [rsi], dl
                        test                    rax, rax
                        jnz                     .next_digit
                        mov                     rdx, rsp
                        sub                     rdx, rsi
                        mov                     eax, SYS_WRITE
                        mov                     edi, STDOUT
                        syscall
                        ret
print_error:
                        mov                     eax, SYS_WRITE
                        mov                     edi, STDERR
                        mov                     rsi, err_message
                        mov                     rdx, ERR_MESSAGE_LEN
                        syscall
                        mov                     eax, SYS_EXIT
                        xor                     edi, EXIT_FAILURE
                        syscall
section                 .rodata
err_message:            db                      `Read error!\n`
ERR_MESSAGE_LEN:        equ                     $ - err_message
section                 .bss
BUFFER_LEN:             equ                     4096
buffer:                 resb                    BUFFER_LEN
