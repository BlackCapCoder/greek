	.file	"main.c"
	.text
	.p2align 4
	.globl	run
	.type	run, @function
run:
.LFB16:
	.cfi_startproc
	subq	$65560, %rsp
	.cfi_def_cfa_offset 65568
	xorl	%esi, %esi
	movl	$65544, %edx
	movq	%fs:40, %rax
	movq	%rax, 65544(%rsp)
	xorl	%eax, %eax
	movq	%rsp, %rdi
	call	memset@PLT
	movl	$800000000, %ecx
	xorl	%eax, %eax
	xorl	%r8d, %r8d
	movl	$32768, %esi
	movl	$32768, %edi
	leaq	system0(%rip), %r9
	jmp	.L2
	.p2align 4,,10
	.p2align 3
.L9:
	subl	$1, %esi
	subl	$1, %ecx
	je	.L1
.L4:
	movslq	%esi, %rdi
	movzbl	4(%rsp,%rdi), %eax
.L2:
	movzbl	%r8b, %r8d
	addl	%eax, %eax
	orl	%r8d, %eax
	cltq
	movzbl	(%r9,%rax), %eax
	movl	%eax, %edx
	movl	%eax, %r8d
	shrb	%dl
	andl	$1, %r8d
	andl	$3, %edx
	movb	%dl, 4(%rsp,%rdi)
	testb	$8, %al
	jne	.L9
	addl	$1, %esi
	subl	$1, %ecx
	jne	.L4
.L1:
	movq	65544(%rsp), %rax
	subq	%fs:40, %rax
	jne	.L10
	movl	%r8d, %eax
	addq	$65560, %rsp
	.cfi_remember_state
	.cfi_def_cfa_offset 8
	ret
.L10:
	.cfi_restore_state
	call	__stack_chk_fail@PLT
	.cfi_endproc
.LFE16:
	.size	run, .-run
	.section	.text.startup,"ax",@progbits
	.p2align 4
	.globl	main
	.type	main, @function
main:
.LFB17:
	.cfi_startproc
	subq	$8, %rsp
	.cfi_def_cfa_offset 16
	call	run
	addq	$8, %rsp
	.cfi_def_cfa_offset 8
	movzbl	%al, %eax
	ret
	.cfi_endproc
.LFE17:
	.size	main, .-main
	.globl	system0
	.section	.rodata
	.type	system0, @object
	.size	system0, 6
system0:
	.byte	11
	.byte	4
	.byte	4
	.byte	13
	.byte	2
	.byte	8
	.ident	"GCC: (GNU) 10.2.0"
	.section	.note.GNU-stack,"",@progbits
