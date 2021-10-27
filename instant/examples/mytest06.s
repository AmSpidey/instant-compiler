	.text
	.file	"mytest06.ll"
	.globl	printInt                # -- Begin function printInt
	.p2align	4, 0x90
	.type	printInt,@function
printInt:                               # @printInt
	.cfi_startproc
# %bb.0:
	pushq	%rax
	.cfi_def_cfa_offset 16
	movl	%edi, %esi
	leaq	dnl(%rip), %rdi
	xorl	%eax, %eax
	callq	printf@PLT
	popq	%rax
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end0:
	.size	printInt, .Lfunc_end0-printInt
	.cfi_endproc
                                        # -- End function
	.globl	main                    # -- Begin function main
	.p2align	4, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# %bb.0:
	subq	$56, %rsp
	.cfi_def_cfa_offset 64
	movl	$0, 24(%rsp)
	movl	$1, 12(%rsp)
	movl	$0, 8(%rsp)
	movl	$1, 20(%rsp)
	movl	$0, 16(%rsp)
	movl	$1, (%rsp)
	movl	$0, 28(%rsp)
	movl	$1, 4(%rsp)
	movl	$2, %edi
	callq	printInt@PLT
	movl	$1, 24(%rsp)
	movl	$2, 12(%rsp)
	movl	$1, 8(%rsp)
	movl	$2, 20(%rsp)
	movl	$1, 16(%rsp)
	movl	$2, (%rsp)
	movl	$1, 28(%rsp)
	movl	$2, 4(%rsp)
	movl	$1, 32(%rsp)
	movl	$2, 44(%rsp)
	movl	$1, 40(%rsp)
	movl	$2, 52(%rsp)
	movl	$1, 48(%rsp)
	movl	$2, 36(%rsp)
	movl	$2, %edi
	callq	printInt@PLT
	movl	$3, %eax
	addq	$56, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end1:
	.size	main, .Lfunc_end1-main
	.cfi_endproc
                                        # -- End function
	.type	dnl,@object             # @dnl
	.section	.rodata,"a",@progbits
dnl:
	.asciz	"%d\n"
	.size	dnl, 4

	.section	".note.GNU-stack","",@progbits
