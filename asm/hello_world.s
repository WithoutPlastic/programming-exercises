#-------------------------------------------------------------------------------
# hello_world.s, hello world program in assemble language!
#-------------------------------------------------------------------------------

.global _start

.text
    _start:
    # write(1, message, 13)
    mov     $1, %rax            # system call ID. 1 for write
    mov     $1, %rdi            # file handle 1 refer to stdout
    mov     $message, %rsi      # address of string to output
    mov     $13, %rdx           # string length
    syscall                     # system call invocation

    # exit(0)
    mov     $60, %rax           # system call ID. 60 is exit
    xor     %rdi, %rdi          # return code 0
    syscall                     # system call invocation

message:
    .ascii  "Hello, world\n"
