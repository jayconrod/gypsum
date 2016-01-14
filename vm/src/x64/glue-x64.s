	.text

# The two functions below (actually one function with two labels) are responsible for actually
# calling native functions. They load arguments passed in through arrays of different types
# into register appropriate for the calling convention and onto the native stack. Once the
# arguments are prepared, the native function is tail-called, which means that when the native
# function returns, it returns directly to the caller
# (codeswitch::internal::callNativeFunctionRaw) instead of this function. This function does
# not construct its own stack frame and does not leave a return address. This lets us support
# returns through %rax (for integers and pointers) and %xmm0 (for floating point) using the
# same code. The caller will look for the return value in these two places, depending on
# which symbol it uses to call.
  .globl codeswitch_glue_callNativeFunctionRawForInt
  .type codeswitch_glue_callNativeFunctionRawForInt, @function
  .globl codeswitch_glue_callNativeFunctionRawForFloat
  .type codeswitch_glue_callNativeFunctionRawForFloat, @function
# %rdi VM*: the virtual machine to pass to the function as the first argument
# %rsi/32(%rsp) NativeFunction: pointer to the function to call
# %rdx/r10 int: number of integer arguments (at most 5)
# %rcx/r11 uint64_t*: array of integer arguments
# %r8/r10/24(%rsp) int: number of float arguments (at most 8)
# %r9/r11/16(%rsp) uint64_t*: array of float arguments
# 16(%rsp)/r10/8(%rsp) int: number of stack arguments
# 8(%rsp)/r11/(%rsp) uint64_t*: array of stack arguments
# (%rsp)/40(%rsp): return address
codeswitch_glue_callNativeFunctionRawForInt:
codeswitch_glue_callNativeFunctionRawForFloat:
  # Reserve stack space for stack arguments and push registers we won't need immediately.
  # We don't use any callee save registers.
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

  # Move integer array pointer and size into scratch registers, since we'll be replacing them.
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

  # Load float arguments.
.LloadFloatArgs:
  movq 24(%rsp), %r10  # float arg count
  movq 16(%rsp), %r11  # float arg ptr
  cmpq $0, %r10
  je .LloadStackArgs
  movsd (%r11), %xmm0
  cmpq $1, %r10
  je .LloadStackArgs
  movsd 8(%r11), %xmm1
  cmpq $2, %r10
  je .LloadStackArgs
  movsd 16(%r11), %xmm2
  cmpq $3, %r10
  je .LloadStackArgs
  movsd 24(%r11), %xmm3
  cmpq $4, %r10
  je .LloadStackArgs
  movsd 32(%r11), %xmm4
  cmpq $5, %r10
  je .LloadStackArgs
  movsd 40(%r11), %xmm5
  cmpq $6, %r10
  je .LloadStackArgs
  movsd 48(%r11), %xmm6
  cmpq $7, %r10
  je .LloadStackArgs
  movsd 56(%r11), %xmm7

  # Load the remaining arguments onto the stack.
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

  # Restore the stack and make the call.
.Lcall:
  addq $32, %rsp
  popq %rax  # native function
  jmp *%rax
