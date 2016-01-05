	.text
  .globl codeswitch_glue_callNativeFunction
  .type codeswitch_glue_callNativeFunction, @function
# %rdi VM*: the virtual machine to pass to the function as the first argument
# %rsi/32(%rsp) NativeFunction: pointer to the function to call
# %rdx/r10 int: number of integer arguments (at most 5)
# %rcx/r11 uint64_t*: array of integer arguments
# %r8/r10/24(%rsp) int: number of float arguments (at most 8)
# %r9/r11/16(%rsp) uint64_t*: array of float arguments
# 16(%rsp)/r10/8(%rsp) int: number of stack arguments
# 8(%rsp)/r11/(%rsp) uint64_t*: array of stack arguments
# (%rsp)/40(%rsp): return address
codeswitch_glue_callNativeFunction:
  # Reserve stack space for arguments and callee save regs.
  popq %rax  # return address
  popq %r10  # stack arg count
  popq %r11  # stack arg ptr
  shlq $3, %r10
  subq %r10, %rsp
  shrq $3, %r10
  pushq %rax  # return address
  pushq %rsi  # native function
  pushq %r8  # float arg count
  pushq %r9  # float arg ptr
  pushq %r10  # stack arg count
  pushq %r11  # stack arg ptr

  # Move integer arguments into scratch registers, since we'll be replacing them.
  movq %rdx, %r10  # int arg count
  movq %rcx, %r11  # int arg ptr

  # Load integer arguments
  cmpq $0, %r10
  je .LloadFloatArgs
  movq (%r11), %rsi
  cmpq $1, %r10
  je .LloadFloatArgs
  movq 8(%r11), %rdx
  cmpq $2, %r10
  je .LloadFloatArgs
  movq 16(%r11), %rcx
  cmpq $3, %r10
  je .LloadFloatArgs
  movq 24(%r11), %r8
  cmpq $4, %r10
  je .LloadFloatArgs
  movq 24(%r11), %r9

  # Load float arguments
.LloadFloatArgs:
  movq 24(%rsp), %r10  # float arg count
  movq 16(%rsp), %r11  # float arg ptr
  cmpq $0, %r10
  je .LloadStackArgs
  movsd (%r11), %xmm0
  cmpq $1, %r10
  je .LloadStackArgs
  movsd 8(%r11), %xmm0
  cmpq $2, %r10
  je .LloadStackArgs
  movsd 16(%r11), %xmm0
  cmpq $3, %r10
  je .LloadStackArgs
  movsd 24(%r11), %xmm0
  cmpq $4, %r10
  je .LloadStackArgs
  movsd 32(%r11), %xmm0
  cmpq $5, %r10
  je .LloadStackArgs
  movsd 40(%r11), %xmm0
  cmpq $6, %r10
  je .LloadStackArgs
  movsd 48(%r11), %xmm0
  cmpq $7, %r10
  je .LloadStackArgs
  movsd 56(%r11), %xmm0

  # Load the remaining arguments onto the stack
.LloadStackArgs:
  movq 8(%rsp), %r10  # stack arg count
  movq (%rsp), %r11  # stack arg ptr
  cmpq $0, %r10
  je .Lcall
.LloadStackArgLoop:
  decq %r10
  movq (%r11, %r10, 8), %rax
  movq %rax, 48(%rsp, %r10, 8)
  cmpq $0, %r10
  jg .LloadStackArgLoop

  # Restore the stack and make the call
.Lcall:
  addq $32, %rsp
  popq %rax  # native function
  jmp *%rax
