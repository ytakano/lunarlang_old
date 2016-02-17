	.section	__TEXT,__text,regular,pure_instructions
	.macosx_version_min 10, 11
                                        ## Start of file scope inline assembly
	.globl	___INVOKE
___INVOKE:
	popq	%rax
	popq	%rdi
	pushq	%rdi
	callq	*%rax
	popq	%rdi
	popq	%rax
	movl	$3, (%rax)
	jmp	___YIELD


                                        ## End of file scope inline assembly
	.globl	___YIELD
	.align	4, 0x90
___YIELD:                               ## @__YIELD
	.cfi_startproc
## BB#0:
	pushq	%rbp
Ltmp0:
	.cfi_def_cfa_offset 16
Ltmp1:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
Ltmp2:
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rdi
	movq	%rdi, -16(%rbp)
	movq	-16(%rbp), %rdi
	callq	__ZN5lunar12green_thread5yieldEv
	addq	$16, %rsp
	popq	%rbp
	retq
	.cfi_endproc

	.globl	__ZN5lunar12green_thread5yieldEv
	.align	4, 0x90
__ZN5lunar12green_thread5yieldEv:       ## @_ZN5lunar12green_thread5yieldEv
	.cfi_startproc
## BB#0:
	pushq	%rbp
Ltmp3:
	.cfi_def_cfa_offset 16
Ltmp4:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
Ltmp5:
	.cfi_def_cfa_register %rbp
	subq	$1712, %rsp             ## imm = 0x6B0
	leaq	-1512(%rbp), %rax
	movq	%rdi, -1544(%rbp)
	movq	-1544(%rbp), %rdi
	movq	%rdi, %rcx
	addq	$160, %rcx
	movq	%rcx, -1536(%rbp)
	movq	-1536(%rbp), %rcx
	movq	%rcx, -1520(%rbp)
	movq	-1520(%rbp), %rcx
	movq	8(%rcx), %rcx
	movq	%rax, -1496(%rbp)
	movq	%rcx, -1504(%rbp)
	movq	-1496(%rbp), %rax
	movq	-1504(%rbp), %rcx
	movq	%rax, -1480(%rbp)
	movq	%rcx, -1488(%rbp)
	movq	-1480(%rbp), %rax
	movq	-1488(%rbp), %rcx
	movq	%rcx, (%rax)
	movq	-1512(%rbp), %rax
	movq	%rax, -1528(%rbp)
	movq	-1528(%rbp), %rax
	movq	%rax, -1552(%rbp)
	movq	%rdi, -1696(%rbp)       ## 8-byte Spill
LBB1_1:                                 ## =>This Inner Loop Header: Depth=1
	leaq	-1560(%rbp), %rax
	leaq	-1552(%rbp), %rcx
	leaq	-1448(%rbp), %rdx
	movq	-1696(%rbp), %rsi       ## 8-byte Reload
	addq	$160, %rsi
	movq	%rsi, -1472(%rbp)
	movq	-1472(%rbp), %rsi
	movq	%rsi, -1456(%rbp)
	movq	-1456(%rbp), %rsi
	movq	%rsi, -1440(%rbp)
	movq	-1440(%rbp), %rsi
	movq	%rsi, -1432(%rbp)
	movq	-1432(%rbp), %rsi
	movq	%rdx, -1416(%rbp)
	movq	%rsi, -1424(%rbp)
	movq	-1416(%rbp), %rdx
	movq	-1424(%rbp), %rsi
	movq	%rdx, -1400(%rbp)
	movq	%rsi, -1408(%rbp)
	movq	-1400(%rbp), %rdx
	movq	-1408(%rbp), %rsi
	movq	%rsi, (%rdx)
	movq	-1448(%rbp), %rdx
	movq	%rdx, -1464(%rbp)
	movq	-1464(%rbp), %rdx
	movq	%rdx, -1560(%rbp)
	movq	%rcx, -1288(%rbp)
	movq	%rax, -1296(%rbp)
	movq	-1288(%rbp), %rax
	movq	-1296(%rbp), %rcx
	movq	%rax, -1272(%rbp)
	movq	%rcx, -1280(%rbp)
	movq	-1272(%rbp), %rax
	movq	(%rax), %rax
	movq	-1280(%rbp), %rcx
	cmpq	(%rcx), %rax
	sete	%dil
	xorb	$1, %dil
	testb	$1, %dil
	jne	LBB1_2
	jmp	LBB1_26
LBB1_2:                                 ##   in Loop: Header=BB1_1 Depth=1
	leaq	-1552(%rbp), %rax
	movq	%rax, -1200(%rbp)
	movq	-1200(%rbp), %rax
	movq	(%rax), %rax
	addq	$16, %rax
	movq	%rax, -1104(%rbp)
	movq	-1104(%rbp), %rax
	movq	%rax, -1096(%rbp)
	movq	-1096(%rbp), %rax
	movq	%rax, -1088(%rbp)
	movq	-1088(%rbp), %rax
	movq	(%rax), %rax
	cmpl	$2, (%rax)
	jne	LBB1_9
## BB#3:
	movq	-1696(%rbp), %rax       ## 8-byte Reload
	cmpq	$0, 152(%rax)
	je	LBB1_8
## BB#4:
	movq	-1696(%rbp), %rax       ## 8-byte Reload
	movq	152(%rax), %rcx
	cmpl	$1, (%rcx)
	jne	LBB1_8
## BB#5:
	movq	-1696(%rbp), %rax       ## 8-byte Reload
	movq	152(%rax), %rcx
	addq	$4, %rcx
	movq	%rcx, %rdi
	callq	_setjmp
	cmpl	$0, %eax
	jne	LBB1_7
## BB#6:
	leaq	-1552(%rbp), %rax
	leaq	-1584(%rbp), %rcx
	leaq	-1576(%rbp), %rdx
	leaq	-1568(%rbp), %rsi
	leaq	-56(%rbp), %rdi
	movq	%rax, -856(%rbp)
	movq	-856(%rbp), %r8
	movq	(%r8), %r8
	addq	$16, %r8
	movq	%r8, -760(%rbp)
	movq	-760(%rbp), %r8
	movq	%r8, -752(%rbp)
	movq	-752(%rbp), %r8
	movq	%r8, -744(%rbp)
	movq	-744(%rbp), %r8
	movq	(%r8), %r8
	movl	$1, (%r8)
	movq	%rax, -448(%rbp)
	movq	-448(%rbp), %r8
	movq	(%r8), %r8
	addq	$16, %r8
	movq	%r8, -440(%rbp)
	movq	-440(%rbp), %r8
	movq	%r8, -432(%rbp)
	movq	-432(%rbp), %r8
	movq	%r8, -168(%rbp)
	movq	-168(%rbp), %r8
	movq	%r8, -160(%rbp)
	movq	-160(%rbp), %r8
	movq	%r8, -152(%rbp)
	movq	-152(%rbp), %r8
	movq	(%r8), %r8
	movq	-1696(%rbp), %r9        ## 8-byte Reload
	movq	%r8, 152(%r9)
	addq	$160, %r9
	movq	-1696(%rbp), %r8        ## 8-byte Reload
	addq	$160, %r8
	movq	%r8, -80(%rbp)
	movq	-80(%rbp), %r8
	movq	%r8, -64(%rbp)
	movq	-64(%rbp), %r8
	movq	%r8, -48(%rbp)
	movq	-48(%rbp), %r8
	movq	%r8, -40(%rbp)
	movq	-40(%rbp), %r8
	movq	%rdi, -24(%rbp)
	movq	%r8, -32(%rbp)
	movq	-24(%rbp), %rdi
	movq	-32(%rbp), %r8
	movq	%rdi, -8(%rbp)
	movq	%r8, -16(%rbp)
	movq	-8(%rbp), %rdi
	movq	-16(%rbp), %r8
	movq	%r8, (%rdi)
	movq	-56(%rbp), %rdi
	movq	%rdi, -72(%rbp)
	movq	-72(%rbp), %rdi
	movq	%rdi, -1576(%rbp)
	movq	%rsi, -104(%rbp)
	movq	%rdx, -112(%rbp)
	movq	-104(%rbp), %rdx
	movq	-112(%rbp), %rsi
	movq	%rdx, -88(%rbp)
	movq	%rsi, -96(%rbp)
	movq	-88(%rbp), %rdx
	movq	-96(%rbp), %rsi
	movq	(%rsi), %rsi
	movq	%rsi, (%rdx)
	movq	-1696(%rbp), %rdx       ## 8-byte Reload
	addq	$160, %rdx
	movq	%rcx, -136(%rbp)
	movq	%rax, -144(%rbp)
	movq	-136(%rbp), %rax
	movq	-144(%rbp), %rcx
	movq	%rax, -120(%rbp)
	movq	%rcx, -128(%rbp)
	movq	-120(%rbp), %rax
	movq	-128(%rbp), %rcx
	movq	(%rcx), %rcx
	movq	%rcx, (%rax)
	movq	-1568(%rbp), %rsi
	movq	-1584(%rbp), %rcx
	movq	%r9, %rdi
	callq	__ZNSt3__14listINS_10unique_ptrIN5lunar12green_thread7contextENS_14default_deleteIS4_EEEENS_9allocatorIS7_EEE6spliceENS_21__list_const_iteratorIS7_PvEERSA_SD_
	movl	$1, %esi
	leaq	-1552(%rbp), %rax
	movq	%rax, -176(%rbp)
	movq	-176(%rbp), %rax
	movq	(%rax), %rax
	addq	$16, %rax
	movq	%rax, -200(%rbp)
	movq	-200(%rbp), %rax
	movq	%rax, -192(%rbp)
	movq	-192(%rbp), %rax
	movq	%rax, -184(%rbp)
	movq	-184(%rbp), %rax
	movq	(%rax), %rax
	addq	$4, %rax
	movq	%rax, %rdi
	callq	_longjmp
LBB1_7:
	jmp	LBB1_27
LBB1_8:
	leaq	-1552(%rbp), %rax
	leaq	-1608(%rbp), %rcx
	leaq	-1600(%rbp), %rdx
	leaq	-1592(%rbp), %rsi
	leaq	-336(%rbp), %rdi
	movq	%rax, -208(%rbp)
	movq	-208(%rbp), %r8
	movq	(%r8), %r8
	addq	$16, %r8
	movq	%r8, -232(%rbp)
	movq	-232(%rbp), %r8
	movq	%r8, -224(%rbp)
	movq	-224(%rbp), %r8
	movq	%r8, -216(%rbp)
	movq	-216(%rbp), %r8
	movq	(%r8), %r8
	movl	$1, (%r8)
	movq	%rax, -256(%rbp)
	movq	-256(%rbp), %r8
	movq	(%r8), %r8
	addq	$16, %r8
	movq	%r8, -248(%rbp)
	movq	-248(%rbp), %r8
	movq	%r8, -240(%rbp)
	movq	-240(%rbp), %r8
	movq	%r8, -280(%rbp)
	movq	-280(%rbp), %r8
	movq	%r8, -272(%rbp)
	movq	-272(%rbp), %r8
	movq	%r8, -264(%rbp)
	movq	-264(%rbp), %r8
	movq	(%r8), %r8
	movq	-1696(%rbp), %r9        ## 8-byte Reload
	movq	%r8, 152(%r9)
	addq	$160, %r9
	movq	-1696(%rbp), %r8        ## 8-byte Reload
	addq	$160, %r8
	movq	%r8, -360(%rbp)
	movq	-360(%rbp), %r8
	movq	%r8, -344(%rbp)
	movq	-344(%rbp), %r8
	movq	%r8, -328(%rbp)
	movq	-328(%rbp), %r8
	movq	%r8, -320(%rbp)
	movq	-320(%rbp), %r8
	movq	%rdi, -304(%rbp)
	movq	%r8, -312(%rbp)
	movq	-304(%rbp), %rdi
	movq	-312(%rbp), %r8
	movq	%rdi, -288(%rbp)
	movq	%r8, -296(%rbp)
	movq	-288(%rbp), %rdi
	movq	-296(%rbp), %r8
	movq	%r8, (%rdi)
	movq	-336(%rbp), %rdi
	movq	%rdi, -352(%rbp)
	movq	-352(%rbp), %rdi
	movq	%rdi, -1600(%rbp)
	movq	%rsi, -384(%rbp)
	movq	%rdx, -392(%rbp)
	movq	-384(%rbp), %rdx
	movq	-392(%rbp), %rsi
	movq	%rdx, -368(%rbp)
	movq	%rsi, -376(%rbp)
	movq	-368(%rbp), %rdx
	movq	-376(%rbp), %rsi
	movq	(%rsi), %rsi
	movq	%rsi, (%rdx)
	movq	-1696(%rbp), %rdx       ## 8-byte Reload
	addq	$160, %rdx
	movq	%rcx, -416(%rbp)
	movq	%rax, -424(%rbp)
	movq	-416(%rbp), %rax
	movq	-424(%rbp), %rcx
	movq	%rax, -400(%rbp)
	movq	%rcx, -408(%rbp)
	movq	-400(%rbp), %rax
	movq	-408(%rbp), %rcx
	movq	(%rcx), %rcx
	movq	%rcx, (%rax)
	movq	-1592(%rbp), %rsi
	movq	-1608(%rbp), %rcx
	movq	%r9, %rdi
	callq	__ZNSt3__14listINS_10unique_ptrIN5lunar12green_thread7contextENS_14default_deleteIS4_EEEENS_9allocatorIS7_EEE6spliceENS_21__list_const_iteratorIS7_PvEERSA_SD_
	movl	$1, %esi
	leaq	-1552(%rbp), %rax
	movq	%rax, -456(%rbp)
	movq	-456(%rbp), %rax
	movq	(%rax), %rax
	addq	$16, %rax
	movq	%rax, -480(%rbp)
	movq	-480(%rbp), %rax
	movq	%rax, -472(%rbp)
	movq	-472(%rbp), %rax
	movq	%rax, -464(%rbp)
	movq	-464(%rbp), %rax
	movq	(%rax), %rax
	addq	$4, %rax
	movq	%rax, %rdi
	callq	_longjmp
LBB1_9:                                 ##   in Loop: Header=BB1_1 Depth=1
	leaq	-1552(%rbp), %rax
	movq	%rax, -488(%rbp)
	movq	-488(%rbp), %rax
	movq	(%rax), %rax
	addq	$16, %rax
	movq	%rax, -512(%rbp)
	movq	-512(%rbp), %rax
	movq	%rax, -504(%rbp)
	movq	-504(%rbp), %rax
	movq	%rax, -496(%rbp)
	movq	-496(%rbp), %rax
	movq	(%rax), %rax
	cmpl	$0, (%rax)
	jne	LBB1_18
## BB#10:                               ##   in Loop: Header=BB1_1 Depth=1
	movq	-1696(%rbp), %rax       ## 8-byte Reload
	cmpq	$0, 152(%rax)
	je	LBB1_16
## BB#11:                               ##   in Loop: Header=BB1_1 Depth=1
	movq	-1696(%rbp), %rax       ## 8-byte Reload
	movq	152(%rax), %rcx
	cmpl	$1, (%rcx)
	jne	LBB1_16
## BB#12:                               ##   in Loop: Header=BB1_1 Depth=1
	movq	-1696(%rbp), %rax       ## 8-byte Reload
	movq	152(%rax), %rcx
	addq	$4, %rcx
	movq	%rcx, %rdi
	callq	_setjmp
	cmpl	$0, %eax
	jne	LBB1_14
## BB#13:                               ##   in Loop: Header=BB1_1 Depth=1
	leaq	-1552(%rbp), %rax
	leaq	-1632(%rbp), %rcx
	leaq	-1624(%rbp), %rdx
	leaq	-1616(%rbp), %rsi
	leaq	-648(%rbp), %rdi
	movq	%rax, -520(%rbp)
	movq	-520(%rbp), %r8
	movq	(%r8), %r8
	addq	$16, %r8
	movq	%r8, -544(%rbp)
	movq	-544(%rbp), %r8
	movq	%r8, -536(%rbp)
	movq	-536(%rbp), %r8
	movq	%r8, -528(%rbp)
	movq	-528(%rbp), %r8
	movq	(%r8), %r8
	movl	$1, (%r8)
	movq	%rax, -568(%rbp)
	movq	-568(%rbp), %r8
	movq	(%r8), %r8
	addq	$16, %r8
	movq	%r8, -560(%rbp)
	movq	-560(%rbp), %r8
	movq	%r8, -552(%rbp)
	movq	-552(%rbp), %r8
	movq	%r8, -592(%rbp)
	movq	-592(%rbp), %r8
	movq	%r8, -584(%rbp)
	movq	-584(%rbp), %r8
	movq	%r8, -576(%rbp)
	movq	-576(%rbp), %r8
	movq	(%r8), %r8
	movq	-1696(%rbp), %r9        ## 8-byte Reload
	movq	%r8, 152(%r9)
	addq	$160, %r9
	movq	-1696(%rbp), %r8        ## 8-byte Reload
	addq	$160, %r8
	movq	%r8, -672(%rbp)
	movq	-672(%rbp), %r8
	movq	%r8, -656(%rbp)
	movq	-656(%rbp), %r8
	movq	%r8, -640(%rbp)
	movq	-640(%rbp), %r8
	movq	%r8, -632(%rbp)
	movq	-632(%rbp), %r8
	movq	%rdi, -616(%rbp)
	movq	%r8, -624(%rbp)
	movq	-616(%rbp), %rdi
	movq	-624(%rbp), %r8
	movq	%rdi, -600(%rbp)
	movq	%r8, -608(%rbp)
	movq	-600(%rbp), %rdi
	movq	-608(%rbp), %r8
	movq	%r8, (%rdi)
	movq	-648(%rbp), %rdi
	movq	%rdi, -664(%rbp)
	movq	-664(%rbp), %rdi
	movq	%rdi, -1624(%rbp)
	movq	%rsi, -696(%rbp)
	movq	%rdx, -704(%rbp)
	movq	-696(%rbp), %rdx
	movq	-704(%rbp), %rsi
	movq	%rdx, -680(%rbp)
	movq	%rsi, -688(%rbp)
	movq	-680(%rbp), %rdx
	movq	-688(%rbp), %rsi
	movq	(%rsi), %rsi
	movq	%rsi, (%rdx)
	movq	-1696(%rbp), %rdx       ## 8-byte Reload
	addq	$160, %rdx
	movq	%rcx, -728(%rbp)
	movq	%rax, -736(%rbp)
	movq	-728(%rbp), %rax
	movq	-736(%rbp), %rcx
	movq	%rax, -712(%rbp)
	movq	%rcx, -720(%rbp)
	movq	-712(%rbp), %rax
	movq	-720(%rbp), %rcx
	movq	(%rcx), %rcx
	movq	%rcx, (%rax)
	movq	-1616(%rbp), %rsi
	movq	-1632(%rbp), %rcx
	movq	%r9, %rdi
	callq	__ZNSt3__14listINS_10unique_ptrIN5lunar12green_thread7contextENS_14default_deleteIS4_EEEENS_9allocatorIS7_EEE6spliceENS_21__list_const_iteratorIS7_PvEERSA_SD_
	leaq	-1552(%rbp), %rax
	movq	%rax, -768(%rbp)
	movq	-768(%rbp), %rcx
	movq	(%rcx), %rcx
	addq	$16, %rcx
	movq	%rcx, -792(%rbp)
	movq	-792(%rbp), %rcx
	movq	%rcx, -784(%rbp)
	movq	-784(%rbp), %rcx
	movq	%rcx, -776(%rbp)
	movq	-776(%rbp), %rcx
	movq	(%rcx), %rcx
	addq	$160, %rcx
	movq	%rax, -800(%rbp)
	movq	-800(%rbp), %rax
	movq	(%rax), %rax
	addq	$16, %rax
	movq	%rax, -824(%rbp)
	movq	-824(%rbp), %rax
	movq	%rax, -816(%rbp)
	movq	-816(%rbp), %rax
	movq	%rax, -808(%rbp)
	movq	-808(%rbp), %rax
	movq	(%rax), %rax
	addq	$160, %rax
	movq	%rax, -832(%rbp)
	movq	-832(%rbp), %rax
	movq	8(%rax), %rdx
	movq	(%rax), %rax
	subq	%rax, %rdx
	sarq	$3, %rdx
	subq	$2, %rdx
	movq	%rcx, -840(%rbp)
	movq	%rdx, -848(%rbp)
	movq	-840(%rbp), %rax
	movq	-848(%rbp), %rcx
	shlq	$3, %rcx
	addq	(%rax), %rcx
	movq	%rcx, -1640(%rbp)
	movq	-1640(%rbp), %rax
	## InlineAsm Start
	movq	%rax, %rsp
	movq	%rax, %rbp
	jmp	___INVOKE

	## InlineAsm End
	jmp	LBB1_15
LBB1_14:
	jmp	LBB1_27
LBB1_15:                                ##   in Loop: Header=BB1_1 Depth=1
	jmp	LBB1_17
LBB1_16:                                ##   in Loop: Header=BB1_1 Depth=1
	leaq	-1552(%rbp), %rax
	leaq	-1664(%rbp), %rcx
	leaq	-1656(%rbp), %rdx
	leaq	-1648(%rbp), %rsi
	leaq	-992(%rbp), %rdi
	movq	%rax, -864(%rbp)
	movq	-864(%rbp), %r8
	movq	(%r8), %r8
	addq	$16, %r8
	movq	%r8, -888(%rbp)
	movq	-888(%rbp), %r8
	movq	%r8, -880(%rbp)
	movq	-880(%rbp), %r8
	movq	%r8, -872(%rbp)
	movq	-872(%rbp), %r8
	movq	(%r8), %r8
	movl	$1, (%r8)
	movq	%rax, -912(%rbp)
	movq	-912(%rbp), %r8
	movq	(%r8), %r8
	addq	$16, %r8
	movq	%r8, -904(%rbp)
	movq	-904(%rbp), %r8
	movq	%r8, -896(%rbp)
	movq	-896(%rbp), %r8
	movq	%r8, -936(%rbp)
	movq	-936(%rbp), %r8
	movq	%r8, -928(%rbp)
	movq	-928(%rbp), %r8
	movq	%r8, -920(%rbp)
	movq	-920(%rbp), %r8
	movq	(%r8), %r8
	movq	-1696(%rbp), %r9        ## 8-byte Reload
	movq	%r8, 152(%r9)
	addq	$160, %r9
	movq	-1696(%rbp), %r8        ## 8-byte Reload
	addq	$160, %r8
	movq	%r8, -1016(%rbp)
	movq	-1016(%rbp), %r8
	movq	%r8, -1000(%rbp)
	movq	-1000(%rbp), %r8
	movq	%r8, -984(%rbp)
	movq	-984(%rbp), %r8
	movq	%r8, -976(%rbp)
	movq	-976(%rbp), %r8
	movq	%rdi, -960(%rbp)
	movq	%r8, -968(%rbp)
	movq	-960(%rbp), %rdi
	movq	-968(%rbp), %r8
	movq	%rdi, -944(%rbp)
	movq	%r8, -952(%rbp)
	movq	-944(%rbp), %rdi
	movq	-952(%rbp), %r8
	movq	%r8, (%rdi)
	movq	-992(%rbp), %rdi
	movq	%rdi, -1008(%rbp)
	movq	-1008(%rbp), %rdi
	movq	%rdi, -1656(%rbp)
	movq	%rsi, -1040(%rbp)
	movq	%rdx, -1048(%rbp)
	movq	-1040(%rbp), %rdx
	movq	-1048(%rbp), %rsi
	movq	%rdx, -1024(%rbp)
	movq	%rsi, -1032(%rbp)
	movq	-1024(%rbp), %rdx
	movq	-1032(%rbp), %rsi
	movq	(%rsi), %rsi
	movq	%rsi, (%rdx)
	movq	-1696(%rbp), %rdx       ## 8-byte Reload
	addq	$160, %rdx
	movq	%rcx, -1072(%rbp)
	movq	%rax, -1080(%rbp)
	movq	-1072(%rbp), %rax
	movq	-1080(%rbp), %rcx
	movq	%rax, -1056(%rbp)
	movq	%rcx, -1064(%rbp)
	movq	-1056(%rbp), %rax
	movq	-1064(%rbp), %rcx
	movq	(%rcx), %rcx
	movq	%rcx, (%rax)
	movq	-1648(%rbp), %rsi
	movq	-1664(%rbp), %rcx
	movq	%r9, %rdi
	callq	__ZNSt3__14listINS_10unique_ptrIN5lunar12green_thread7contextENS_14default_deleteIS4_EEEENS_9allocatorIS7_EEE6spliceENS_21__list_const_iteratorIS7_PvEERSA_SD_
	leaq	-1552(%rbp), %rax
	movq	%rax, -1112(%rbp)
	movq	-1112(%rbp), %rcx
	movq	(%rcx), %rcx
	addq	$16, %rcx
	movq	%rcx, -1136(%rbp)
	movq	-1136(%rbp), %rcx
	movq	%rcx, -1128(%rbp)
	movq	-1128(%rbp), %rcx
	movq	%rcx, -1120(%rbp)
	movq	-1120(%rbp), %rcx
	movq	(%rcx), %rcx
	addq	$160, %rcx
	movq	%rax, -1144(%rbp)
	movq	-1144(%rbp), %rax
	movq	(%rax), %rax
	addq	$16, %rax
	movq	%rax, -1168(%rbp)
	movq	-1168(%rbp), %rax
	movq	%rax, -1160(%rbp)
	movq	-1160(%rbp), %rax
	movq	%rax, -1152(%rbp)
	movq	-1152(%rbp), %rax
	movq	(%rax), %rax
	addq	$160, %rax
	movq	%rax, -1176(%rbp)
	movq	-1176(%rbp), %rax
	movq	8(%rax), %rdx
	movq	(%rax), %rax
	subq	%rax, %rdx
	sarq	$3, %rdx
	subq	$2, %rdx
	movq	%rcx, -1184(%rbp)
	movq	%rdx, -1192(%rbp)
	movq	-1184(%rbp), %rax
	movq	-1192(%rbp), %rcx
	shlq	$3, %rcx
	addq	(%rax), %rcx
	movq	%rcx, -1672(%rbp)
	movq	-1672(%rbp), %rax
	## InlineAsm Start
	movq	%rax, %rsp
	movq	%rax, %rbp
	jmp	___INVOKE

	## InlineAsm End
LBB1_17:                                ##   in Loop: Header=BB1_1 Depth=1
	jmp	LBB1_24
LBB1_18:                                ##   in Loop: Header=BB1_1 Depth=1
	leaq	-1552(%rbp), %rax
	movq	%rax, -1208(%rbp)
	movq	-1208(%rbp), %rax
	movq	(%rax), %rax
	addq	$16, %rax
	movq	%rax, -1232(%rbp)
	movq	-1232(%rbp), %rax
	movq	%rax, -1224(%rbp)
	movq	-1224(%rbp), %rax
	movq	%rax, -1216(%rbp)
	movq	-1216(%rbp), %rax
	movq	(%rax), %rax
	cmpl	$3, (%rax)
	jne	LBB1_22
## BB#19:                               ##   in Loop: Header=BB1_1 Depth=1
	leaq	-1552(%rbp), %rax
	movq	-1696(%rbp), %rcx       ## 8-byte Reload
	addq	$184, %rcx
	movq	%rax, -1240(%rbp)
	movq	-1240(%rbp), %rax
	movq	(%rax), %rax
	addq	$16, %rax
	movq	%rax, -1264(%rbp)
	movq	-1264(%rbp), %rax
	movq	%rax, -1256(%rbp)
	movq	-1256(%rbp), %rax
	movq	%rax, -1248(%rbp)
	movq	-1248(%rbp), %rax
	movq	(%rax), %rax
	addq	$152, %rax
	movq	%rcx, -1304(%rbp)
	movq	%rax, -1312(%rbp)
	movq	-1304(%rbp), %rdi
	movq	-1312(%rbp), %rsi
	callq	__ZNSt3__112__hash_tableINS_17__hash_value_typeIiPN5lunar12green_thread7contextEEENS_22__unordered_map_hasherIiS6_NS_4hashIiEELb1EEENS_21__unordered_map_equalIiS6_NS_8equal_toIiEELb1EEENS_9allocatorIS6_EEE14__erase_uniqueIiEEmRKT_
	leaq	-1552(%rbp), %rcx
	movq	%rcx, -1336(%rbp)
	movq	-1336(%rbp), %rcx
	movq	(%rcx), %rcx
	addq	$16, %rcx
	movq	%rcx, -1328(%rbp)
	movq	-1328(%rbp), %rcx
	movq	%rcx, -1320(%rbp)
	movq	-1320(%rbp), %rcx
	movq	%rcx, -1360(%rbp)
	movq	-1360(%rbp), %rcx
	movq	%rcx, -1352(%rbp)
	movq	-1352(%rbp), %rcx
	movq	%rcx, -1344(%rbp)
	movq	-1344(%rbp), %rcx
	movq	(%rcx), %rcx
	movq	-1696(%rbp), %rsi       ## 8-byte Reload
	cmpq	152(%rsi), %rcx
	movq	%rax, -1704(%rbp)       ## 8-byte Spill
	jne	LBB1_21
## BB#20:                               ##   in Loop: Header=BB1_1 Depth=1
	movq	-1696(%rbp), %rax       ## 8-byte Reload
	movq	$0, 152(%rax)
LBB1_21:                                ##   in Loop: Header=BB1_1 Depth=1
	leaq	-1552(%rbp), %rax
	leaq	-1688(%rbp), %rcx
	movq	-1696(%rbp), %rdx       ## 8-byte Reload
	addq	$160, %rdx
	movq	%rcx, -1384(%rbp)
	movq	%rax, -1392(%rbp)
	movq	-1384(%rbp), %rax
	movq	-1392(%rbp), %rcx
	movq	%rax, -1368(%rbp)
	movq	%rcx, -1376(%rbp)
	movq	-1368(%rbp), %rax
	movq	-1376(%rbp), %rcx
	movq	(%rcx), %rcx
	movq	%rcx, (%rax)
	movq	-1688(%rbp), %rsi
	movq	%rdx, %rdi
	callq	__ZNSt3__14listINS_10unique_ptrIN5lunar12green_thread7contextENS_14default_deleteIS4_EEEENS_9allocatorIS7_EEE5eraseENS_21__list_const_iteratorIS7_PvEE
	movq	%rax, -1680(%rbp)
	movq	-1680(%rbp), %rax
	movq	%rax, -1552(%rbp)
	jmp	LBB1_23
LBB1_22:
	jmp	LBB1_26
LBB1_23:                                ##   in Loop: Header=BB1_1 Depth=1
	jmp	LBB1_24
LBB1_24:                                ##   in Loop: Header=BB1_1 Depth=1
	jmp	LBB1_25
LBB1_25:                                ##   in Loop: Header=BB1_1 Depth=1
	jmp	LBB1_1
LBB1_26:
	movl	$1, %esi
	movq	-1696(%rbp), %rdi       ## 8-byte Reload
	callq	_longjmp
LBB1_27:
	addq	$1712, %rsp             ## imm = 0x6B0
	popq	%rbp
	retq
	.cfi_endproc

	.globl	__ZN5lunar12green_thread5spawnEPFvPvEi
	.align	4, 0x90
__ZN5lunar12green_thread5spawnEPFvPvEi: ## @_ZN5lunar12green_thread5spawnEPFvPvEi
Lfunc_begin2:
	.cfi_startproc
	.cfi_personality 155, ___gxx_personality_v0
	.cfi_lsda 16, Lexception2
## BB#0:
	pushq	%rbp
Ltmp13:
	.cfi_def_cfa_offset 16
Ltmp14:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
Ltmp15:
	.cfi_def_cfa_register %rbp
	subq	$656, %rsp              ## imm = 0x290
	leaq	-520(%rbp), %rax
	movq	%rdi, -496(%rbp)
	movq	%rsi, -504(%rbp)
	movl	%edx, -508(%rbp)
	movq	-496(%rbp), %rsi
	movq	%rax, %rdi
	movq	%rsi, -568(%rbp)        ## 8-byte Spill
	callq	__ZN4llvm11make_uniqueIN5lunar12green_thread7contextEJEEENSt3__19enable_ifIXntsr3std8is_arrayIT_EE5valueENS4_10unique_ptrIS6_NS4_14default_deleteIS6_EEEEE4typeEDpOT0_
LBB2_1:                                 ## =>This Inner Loop Header: Depth=1
	movq	-568(%rbp), %rax        ## 8-byte Reload
	movl	148(%rax), %ecx
	addl	$1, %ecx
	movl	%ecx, 148(%rax)
	cmpl	$0, %ecx
	je	LBB2_3
## BB#2:                                ##   in Loop: Header=BB2_1 Depth=1
	jmp	LBB2_1
LBB2_3:
	leaq	-520(%rbp), %rax
	movq	%rax, -488(%rbp)
	movq	%rax, -480(%rbp)
	movq	%rax, -472(%rbp)
	movq	-520(%rbp), %rax
	addq	$160, %rax
	movslq	-508(%rbp), %rsi
Ltmp6:
	movq	%rax, %rdi
	callq	__ZNSt3__16vectorIyNS_9allocatorIyEEE6resizeEm
Ltmp7:
	jmp	LBB2_4
LBB2_4:
	movq	-568(%rbp), %rax        ## 8-byte Reload
	addq	$148, %rax
	movq	-568(%rbp), %rcx        ## 8-byte Reload
	movl	148(%rcx), %edx
	leaq	-520(%rbp), %rsi
	movq	%rsi, -360(%rbp)
	movq	%rsi, -352(%rbp)
	movq	%rsi, -344(%rbp)
	movq	-520(%rbp), %rdi
	movl	%edx, 152(%rdi)
	movq	%rsi, -336(%rbp)
	movq	%rsi, -328(%rbp)
	movq	%rsi, -320(%rbp)
	movq	-520(%rbp), %rdi
	movl	$0, (%rdi)
	movq	%rsi, -208(%rbp)
	movq	%rsi, -200(%rbp)
	movq	%rsi, -192(%rbp)
	movq	-520(%rbp), %rsi
	movq	%rsi, -544(%rbp)
	addq	$184, %rcx
Ltmp8:
	movq	%rcx, %rdi
	movq	%rsi, -576(%rbp)        ## 8-byte Spill
	movq	%rax, %rsi
	callq	__ZNSt3__113unordered_mapIiPN5lunar12green_thread7contextENS_4hashIiEENS_8equal_toIiEENS_9allocatorINS_4pairIKiS4_EEEEEixERSB_
Ltmp9:
	movq	%rax, -584(%rbp)        ## 8-byte Spill
	jmp	LBB2_5
LBB2_5:
	movq	-584(%rbp), %rax        ## 8-byte Reload
	movq	-576(%rbp), %rcx        ## 8-byte Reload
	movq	%rcx, (%rax)
	movq	-568(%rbp), %rdx        ## 8-byte Reload
	addq	$160, %rdx
	leaq	-520(%rbp), %rsi
	movq	%rsi, -184(%rbp)
Ltmp10:
	movq	%rdx, %rdi
	callq	__ZNSt3__14listINS_10unique_ptrIN5lunar12green_thread7contextENS_14default_deleteIS4_EEEENS_9allocatorIS7_EEE9push_backEOS7_
Ltmp11:
	jmp	LBB2_6
LBB2_6:
	leaq	-520(%rbp), %rax
	movq	%rax, -24(%rbp)
	movq	-24(%rbp), %rcx
	movq	%rcx, -16(%rbp)
	movq	-16(%rbp), %rcx
	movq	%rcx, -8(%rbp)
	movq	-8(%rbp), %rcx
	movq	(%rcx), %rcx
	addq	$160, %rcx
	movq	%rcx, -32(%rbp)
	movq	-32(%rbp), %rcx
	movq	8(%rcx), %rdx
	movq	(%rcx), %rcx
	subq	%rcx, %rdx
	sarq	$3, %rdx
	movq	%rdx, -552(%rbp)
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rcx
	movq	%rcx, -48(%rbp)
	movq	-48(%rbp), %rcx
	movq	%rcx, -40(%rbp)
	movq	-40(%rbp), %rcx
	movq	(%rcx), %rcx
	movq	%rax, -80(%rbp)
	movq	-80(%rbp), %rax
	movq	%rax, -72(%rbp)
	movq	-72(%rbp), %rax
	movq	%rax, -64(%rbp)
	movq	-64(%rbp), %rax
	movq	(%rax), %rax
	addq	$160, %rax
	movq	-552(%rbp), %rdx
	subq	$0, %rdx
	movq	%rax, -88(%rbp)
	movq	%rdx, -96(%rbp)
	movq	-88(%rbp), %rax
	movq	-96(%rbp), %rdx
	shlq	$3, %rdx
	addq	(%rax), %rdx
	movq	%rcx, -592(%rbp)        ## 8-byte Spill
	movq	%rdx, -600(%rbp)        ## 8-byte Spill
## BB#7:
	leaq	-520(%rbp), %rax
	movq	-600(%rbp), %rcx        ## 8-byte Reload
	movq	-592(%rbp), %rdx        ## 8-byte Reload
	movq	%rdx, (%rcx)
	movq	%rax, -120(%rbp)
	movq	-120(%rbp), %rax
	movq	%rax, -112(%rbp)
	movq	-112(%rbp), %rax
	movq	%rax, -104(%rbp)
	movq	-104(%rbp), %rax
	movq	(%rax), %rax
	addq	$160, %rax
	movq	-552(%rbp), %rsi
	subq	$1, %rsi
	movq	%rax, -128(%rbp)
	movq	%rsi, -136(%rbp)
	movq	-128(%rbp), %rax
	movq	-136(%rbp), %rsi
	shlq	$3, %rsi
	addq	(%rax), %rsi
	movq	%rsi, -608(%rbp)        ## 8-byte Spill
## BB#8:
	leaq	-520(%rbp), %rax
	movq	-608(%rbp), %rcx        ## 8-byte Reload
	movq	-568(%rbp), %rdx        ## 8-byte Reload
	movq	%rdx, (%rcx)
	movq	-504(%rbp), %rsi
	movq	%rax, -160(%rbp)
	movq	-160(%rbp), %rax
	movq	%rax, -152(%rbp)
	movq	-152(%rbp), %rax
	movq	%rax, -144(%rbp)
	movq	-144(%rbp), %rax
	movq	(%rax), %rax
	addq	$160, %rax
	movq	-552(%rbp), %rdi
	subq	$2, %rdi
	movq	%rax, -168(%rbp)
	movq	%rdi, -176(%rbp)
	movq	-168(%rbp), %rax
	movq	-176(%rbp), %rdi
	shlq	$3, %rdi
	addq	(%rax), %rdi
	movq	%rsi, -616(%rbp)        ## 8-byte Spill
	movq	%rdi, -624(%rbp)        ## 8-byte Spill
## BB#9:
	leaq	-520(%rbp), %rax
	movq	-624(%rbp), %rcx        ## 8-byte Reload
	movq	-616(%rbp), %rdx        ## 8-byte Reload
	movq	%rdx, (%rcx)
	movl	$1, -556(%rbp)
	movq	%rax, -312(%rbp)
	movq	-312(%rbp), %rax
	movq	%rax, -304(%rbp)
	movq	-304(%rbp), %rax
	movq	%rax, -280(%rbp)
	movq	$0, -288(%rbp)
	movq	-280(%rbp), %rax
	movq	%rax, -272(%rbp)
	movq	-272(%rbp), %rsi
	movq	%rsi, -264(%rbp)
	movq	-264(%rbp), %rsi
	movq	(%rsi), %rsi
	movq	%rsi, -296(%rbp)
	movq	-288(%rbp), %rsi
	movq	%rax, -240(%rbp)
	movq	-240(%rbp), %rdi
	movq	%rdi, -232(%rbp)
	movq	-232(%rbp), %rdi
	movq	%rsi, (%rdi)
	cmpq	$0, -296(%rbp)
	movq	%rax, -632(%rbp)        ## 8-byte Spill
	je	LBB2_13
## BB#10:
	movq	-632(%rbp), %rax        ## 8-byte Reload
	movq	%rax, -224(%rbp)
	movq	-224(%rbp), %rcx
	movq	%rcx, -216(%rbp)
	movq	-216(%rbp), %rcx
	movq	-296(%rbp), %rdx
	movq	%rcx, -248(%rbp)
	movq	%rdx, -256(%rbp)
	movq	-256(%rbp), %rcx
	cmpq	$0, %rcx
	movq	%rcx, -640(%rbp)        ## 8-byte Spill
	je	LBB2_12
## BB#11:
	movq	-640(%rbp), %rdi        ## 8-byte Reload
	callq	__ZN5lunar12green_thread7contextD1Ev
	movq	-640(%rbp), %rdi        ## 8-byte Reload
	callq	__ZdlPv
LBB2_12:                                ## %_ZNKSt3__114default_deleteIN5lunar12green_thread7contextEEclEPS3_.exit.i.i.i2
	jmp	LBB2_13
LBB2_13:                                ## %_ZNSt3__110unique_ptrIN5lunar12green_thread7contextENS_14default_deleteIS3_EEED1Ev.exit3
	xorl	%eax, %eax
	addq	$656, %rsp              ## imm = 0x290
	popq	%rbp
	retq
LBB2_14:
Ltmp12:
	leaq	-520(%rbp), %rcx
	movl	%edx, %esi
	movq	%rax, -528(%rbp)
	movl	%esi, -532(%rbp)
	movq	%rcx, -464(%rbp)
	movq	-464(%rbp), %rax
	movq	%rax, -456(%rbp)
	movq	-456(%rbp), %rax
	movq	%rax, -432(%rbp)
	movq	$0, -440(%rbp)
	movq	-432(%rbp), %rax
	movq	%rax, -424(%rbp)
	movq	-424(%rbp), %rcx
	movq	%rcx, -416(%rbp)
	movq	-416(%rbp), %rcx
	movq	(%rcx), %rcx
	movq	%rcx, -448(%rbp)
	movq	-440(%rbp), %rcx
	movq	%rax, -392(%rbp)
	movq	-392(%rbp), %rdx
	movq	%rdx, -384(%rbp)
	movq	-384(%rbp), %rdx
	movq	%rcx, (%rdx)
	cmpq	$0, -448(%rbp)
	movq	%rax, -648(%rbp)        ## 8-byte Spill
	je	LBB2_18
## BB#15:
	movq	-648(%rbp), %rax        ## 8-byte Reload
	movq	%rax, -376(%rbp)
	movq	-376(%rbp), %rcx
	movq	%rcx, -368(%rbp)
	movq	-368(%rbp), %rcx
	movq	-448(%rbp), %rdx
	movq	%rcx, -400(%rbp)
	movq	%rdx, -408(%rbp)
	movq	-408(%rbp), %rcx
	cmpq	$0, %rcx
	movq	%rcx, -656(%rbp)        ## 8-byte Spill
	je	LBB2_17
## BB#16:
	movq	-656(%rbp), %rdi        ## 8-byte Reload
	callq	__ZN5lunar12green_thread7contextD1Ev
	movq	-656(%rbp), %rdi        ## 8-byte Reload
	callq	__ZdlPv
LBB2_17:                                ## %_ZNKSt3__114default_deleteIN5lunar12green_thread7contextEEclEPS3_.exit.i.i.i
	jmp	LBB2_18
LBB2_18:                                ## %_ZNSt3__110unique_ptrIN5lunar12green_thread7contextENS_14default_deleteIS3_EEED1Ev.exit
	jmp	LBB2_19
LBB2_19:
	movq	-528(%rbp), %rdi
	callq	__Unwind_Resume
Lfunc_end2:
	.cfi_endproc
	.section	__TEXT,__gcc_except_tab
	.align	2
GCC_except_table2:
Lexception2:
	.byte	255                     ## @LPStart Encoding = omit
	.byte	155                     ## @TType Encoding = indirect pcrel sdata4
	.byte	41                      ## @TType base offset
	.byte	3                       ## Call site Encoding = udata4
	.byte	39                      ## Call site table length
Lset0 = Lfunc_begin2-Lfunc_begin2       ## >> Call Site 1 <<
	.long	Lset0
Lset1 = Ltmp6-Lfunc_begin2              ##   Call between Lfunc_begin2 and Ltmp6
	.long	Lset1
	.long	0                       ##     has no landing pad
	.byte	0                       ##   On action: cleanup
Lset2 = Ltmp6-Lfunc_begin2              ## >> Call Site 2 <<
	.long	Lset2
Lset3 = Ltmp11-Ltmp6                    ##   Call between Ltmp6 and Ltmp11
	.long	Lset3
Lset4 = Ltmp12-Lfunc_begin2             ##     jumps to Ltmp12
	.long	Lset4
	.byte	0                       ##   On action: cleanup
Lset5 = Ltmp11-Lfunc_begin2             ## >> Call Site 3 <<
	.long	Lset5
Lset6 = Lfunc_end2-Ltmp11               ##   Call between Ltmp11 and Lfunc_end2
	.long	Lset6
	.long	0                       ##     has no landing pad
	.byte	0                       ##   On action: cleanup
	.align	2

	.section	__TEXT,__textcoal_nt,coalesced,pure_instructions
	.globl	__ZN4llvm11make_uniqueIN5lunar12green_thread7contextEJEEENSt3__19enable_ifIXntsr3std8is_arrayIT_EE5valueENS4_10unique_ptrIS6_NS4_14default_deleteIS6_EEEEE4typeEDpOT0_
	.weak_def_can_be_hidden	__ZN4llvm11make_uniqueIN5lunar12green_thread7contextEJEEENSt3__19enable_ifIXntsr3std8is_arrayIT_EE5valueENS4_10unique_ptrIS6_NS4_14default_deleteIS6_EEEEE4typeEDpOT0_
	.align	4, 0x90
__ZN4llvm11make_uniqueIN5lunar12green_thread7contextEJEEENSt3__19enable_ifIXntsr3std8is_arrayIT_EE5valueENS4_10unique_ptrIS6_NS4_14default_deleteIS6_EEEEE4typeEDpOT0_: ## @_ZN4llvm11make_uniqueIN5lunar12green_thread7contextEJEEENSt3__19enable_ifIXntsr3std8is_arrayIT_EE5valueENS4_10unique_ptrIS6_NS4_14default_deleteIS6_EEEEE4typeEDpOT0_
	.cfi_startproc
## BB#0:
	pushq	%rbp
Ltmp16:
	.cfi_def_cfa_offset 16
Ltmp17:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
Ltmp18:
	.cfi_def_cfa_register %rbp
	subq	$128, %rsp
	movq	%rdi, %rax
	movl	$184, %ecx
	movl	%ecx, %edx
	movq	%rdi, -112(%rbp)        ## 8-byte Spill
	movq	%rdx, %rdi
	movq	%rax, -120(%rbp)        ## 8-byte Spill
	callq	__Znwm
	xorl	%esi, %esi
	movl	$184, %ecx
	movl	%ecx, %edx
	movq	%rax, %rdi
	movq	%rax, -128(%rbp)        ## 8-byte Spill
	callq	_memset
	movq	-128(%rbp), %rdi        ## 8-byte Reload
	callq	__ZN5lunar12green_thread7contextC1Ev
	leaq	-24(%rbp), %rax
	leaq	-48(%rbp), %rdx
	leaq	-88(%rbp), %rdi
	movq	-112(%rbp), %r8         ## 8-byte Reload
	movq	%r8, -96(%rbp)
	movq	-128(%rbp), %r9         ## 8-byte Reload
	movq	%r9, -104(%rbp)
	movq	-96(%rbp), %r10
	movq	-104(%rbp), %r11
	movq	%r10, -80(%rbp)
	movq	%r11, -88(%rbp)
	movq	-80(%rbp), %r10
	movq	%rdi, -72(%rbp)
	movq	-72(%rbp), %rdi
	movq	(%rdi), %rdi
	movq	%r10, -56(%rbp)
	movq	%rdi, -64(%rbp)
	movq	-56(%rbp), %rdi
	movq	-64(%rbp), %r10
	movq	%rdi, -40(%rbp)
	movq	%r10, -48(%rbp)
	movq	-40(%rbp), %rdi
	movq	%rdx, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	(%rdx), %rdx
	movq	%rdi, -16(%rbp)
	movq	%rdx, -24(%rbp)
	movq	-16(%rbp), %rdx
	movq	%rax, -8(%rbp)
	movq	-8(%rbp), %rax
	movq	(%rax), %rax
	movq	%rax, (%rdx)
	movq	-120(%rbp), %rax        ## 8-byte Reload
	addq	$128, %rsp
	popq	%rbp
	retq
	.cfi_endproc

	.globl	__ZNSt3__16vectorIyNS_9allocatorIyEEE6resizeEm
	.weak_def_can_be_hidden	__ZNSt3__16vectorIyNS_9allocatorIyEEE6resizeEm
	.align	4, 0x90
__ZNSt3__16vectorIyNS_9allocatorIyEEE6resizeEm: ## @_ZNSt3__16vectorIyNS_9allocatorIyEEE6resizeEm
Lfunc_begin4:
	.cfi_startproc
	.cfi_personality 155, ___gxx_personality_v0
	.cfi_lsda 16, Lexception4
## BB#0:
	pushq	%rbp
Ltmp22:
	.cfi_def_cfa_offset 16
Ltmp23:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
Ltmp24:
	.cfi_def_cfa_register %rbp
	subq	$224, %rsp
	movq	%rdi, -168(%rbp)
	movq	%rsi, -176(%rbp)
	movq	-168(%rbp), %rsi
	movq	%rsi, -160(%rbp)
	movq	-160(%rbp), %rdi
	movq	8(%rdi), %rax
	movq	(%rdi), %rdi
	subq	%rdi, %rax
	sarq	$3, %rax
	movq	%rax, -184(%rbp)
	movq	-184(%rbp), %rax
	cmpq	-176(%rbp), %rax
	movq	%rsi, -192(%rbp)        ## 8-byte Spill
	jae	LBB4_2
## BB#1:
	movq	-176(%rbp), %rax
	subq	-184(%rbp), %rax
	movq	-192(%rbp), %rdi        ## 8-byte Reload
	movq	%rax, %rsi
	callq	__ZNSt3__16vectorIyNS_9allocatorIyEEE8__appendEm
	jmp	LBB4_10
LBB4_2:
	movq	-184(%rbp), %rax
	cmpq	-176(%rbp), %rax
	jbe	LBB4_9
## BB#3:
	movq	-192(%rbp), %rax        ## 8-byte Reload
	movq	(%rax), %rcx
	movq	-176(%rbp), %rdx
	shlq	$3, %rdx
	addq	%rdx, %rcx
	movq	%rax, -136(%rbp)
	movq	%rcx, -144(%rbp)
	movq	-136(%rbp), %rcx
	movq	%rcx, -128(%rbp)
	movq	-128(%rbp), %rdx
	movq	8(%rdx), %rsi
	movq	(%rdx), %rdx
	subq	%rdx, %rsi
	sarq	$3, %rsi
	movq	%rsi, -152(%rbp)
	movq	%rcx, %rdx
	movq	-144(%rbp), %rsi
	movq	%rdx, -112(%rbp)
	movq	%rsi, -120(%rbp)
	movq	-112(%rbp), %rdx
	movq	%rcx, -200(%rbp)        ## 8-byte Spill
	movq	%rdx, -208(%rbp)        ## 8-byte Spill
LBB4_4:                                 ## =>This Inner Loop Header: Depth=1
	movq	-120(%rbp), %rax
	movq	-208(%rbp), %rcx        ## 8-byte Reload
	cmpq	8(%rcx), %rax
	je	LBB4_6
## BB#5:                                ##   in Loop: Header=BB4_4 Depth=1
	movq	-208(%rbp), %rax        ## 8-byte Reload
	movq	%rax, -104(%rbp)
	movq	-104(%rbp), %rcx
	addq	$16, %rcx
	movq	%rcx, -96(%rbp)
	movq	-96(%rbp), %rcx
	movq	%rcx, -88(%rbp)
	movq	-88(%rbp), %rcx
	movq	8(%rax), %rdx
	addq	$-8, %rdx
	movq	%rdx, 8(%rax)
	movq	%rdx, -80(%rbp)
	movq	-80(%rbp), %rdx
	movq	%rcx, -48(%rbp)
	movq	%rdx, -56(%rbp)
	movq	-48(%rbp), %rcx
	movq	-56(%rbp), %rdx
	movq	%rcx, -32(%rbp)
	movq	%rdx, -40(%rbp)
	movq	-32(%rbp), %rcx
	movq	-40(%rbp), %rdx
	movq	%rcx, -8(%rbp)
	movq	%rdx, -16(%rbp)
	jmp	LBB4_4
LBB4_6:                                 ## %_ZNSt3__113__vector_baseIyNS_9allocatorIyEEE17__destruct_at_endEPy.exit.i
	movq	-152(%rbp), %rsi
Ltmp19:
	movq	-200(%rbp), %rdi        ## 8-byte Reload
	callq	__ZNKSt3__16vectorIyNS_9allocatorIyEEE17__annotate_shrinkEm
Ltmp20:
	jmp	LBB4_8
LBB4_7:
Ltmp21:
	movl	%edx, %ecx
	movq	%rax, %rdi
	movl	%ecx, -212(%rbp)        ## 4-byte Spill
	callq	___clang_call_terminate
LBB4_8:                                 ## %_ZNSt3__16vectorIyNS_9allocatorIyEEE17__destruct_at_endEPy.exit
	jmp	LBB4_9
LBB4_9:
	jmp	LBB4_10
LBB4_10:
	addq	$224, %rsp
	popq	%rbp
	retq
Lfunc_end4:
	.cfi_endproc
	.section	__TEXT,__gcc_except_tab
	.align	2
GCC_except_table4:
Lexception4:
	.byte	255                     ## @LPStart Encoding = omit
	.byte	155                     ## @TType Encoding = indirect pcrel sdata4
	.asciz	"\242\200\200"          ## @TType base offset
	.byte	3                       ## Call site Encoding = udata4
	.byte	26                      ## Call site table length
Lset7 = Lfunc_begin4-Lfunc_begin4       ## >> Call Site 1 <<
	.long	Lset7
Lset8 = Ltmp19-Lfunc_begin4             ##   Call between Lfunc_begin4 and Ltmp19
	.long	Lset8
	.long	0                       ##     has no landing pad
	.byte	0                       ##   On action: cleanup
Lset9 = Ltmp19-Lfunc_begin4             ## >> Call Site 2 <<
	.long	Lset9
Lset10 = Ltmp20-Ltmp19                  ##   Call between Ltmp19 and Ltmp20
	.long	Lset10
Lset11 = Ltmp21-Lfunc_begin4            ##     jumps to Ltmp21
	.long	Lset11
	.byte	1                       ##   On action: 1
	.byte	1                       ## >> Action Record 1 <<
                                        ##   Catch TypeInfo 1
	.byte	0                       ##   No further actions
                                        ## >> Catch TypeInfos <<
	.long	0                       ## TypeInfo 1
	.align	2

	.section	__TEXT,__textcoal_nt,coalesced,pure_instructions
	.globl	__ZNSt3__113unordered_mapIiPN5lunar12green_thread7contextENS_4hashIiEENS_8equal_toIiEENS_9allocatorINS_4pairIKiS4_EEEEEixERSB_
	.weak_def_can_be_hidden	__ZNSt3__113unordered_mapIiPN5lunar12green_thread7contextENS_4hashIiEENS_8equal_toIiEENS_9allocatorINS_4pairIKiS4_EEEEEixERSB_
	.align	4, 0x90
__ZNSt3__113unordered_mapIiPN5lunar12green_thread7contextENS_4hashIiEENS_8equal_toIiEENS_9allocatorINS_4pairIKiS4_EEEEEixERSB_: ## @_ZNSt3__113unordered_mapIiPN5lunar12green_thread7contextENS_4hashIiEENS_8equal_toIiEENS_9allocatorINS_4pairIKiS4_EEEEEixERSB_
Lfunc_begin5:
	.cfi_startproc
	.cfi_personality 155, ___gxx_personality_v0
	.cfi_lsda 16, Lexception5
## BB#0:
	pushq	%rbp
Ltmp28:
	.cfi_def_cfa_offset 16
Ltmp29:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
Ltmp30:
	.cfi_def_cfa_register %rbp
	subq	$1216, %rsp             ## imm = 0x4C0
	movq	%rdi, -1048(%rbp)
	movq	%rsi, -1056(%rbp)
	movq	-1048(%rbp), %rsi
	movq	-1056(%rbp), %rdi
	movq	%rsi, -1016(%rbp)
	movq	%rdi, -1024(%rbp)
	movq	-1016(%rbp), %rdi
	movq	-1024(%rbp), %rax
	movq	%rsi, -1152(%rbp)       ## 8-byte Spill
	movq	%rax, %rsi
	callq	__ZNSt3__112__hash_tableINS_17__hash_value_typeIiPN5lunar12green_thread7contextEEENS_22__unordered_map_hasherIiS6_NS_4hashIiEELb1EEENS_21__unordered_map_equalIiS6_NS_8equal_toIiEELb1EEENS_9allocatorIS6_EEE4findIiEENS_15__hash_iteratorIPNS_11__hash_nodeIS6_PvEEEERKT_
	leaq	-1072(%rbp), %rsi
	leaq	-1064(%rbp), %rdi
	leaq	-952(%rbp), %rcx
	leaq	-936(%rbp), %rdx
	leaq	-1008(%rbp), %r8
	movq	%rax, -1032(%rbp)
	movq	-1032(%rbp), %rax
	movq	%rax, -992(%rbp)
	movq	%r8, -1000(%rbp)
	movq	-1000(%rbp), %rax
	movq	-992(%rbp), %r8
	movq	%r8, -976(%rbp)
	movq	%rax, -984(%rbp)
	movq	-984(%rbp), %rax
	movq	-976(%rbp), %r8
	movq	%r8, (%rax)
	movq	-1008(%rbp), %rax
	movq	%rax, -1064(%rbp)
	movq	-1152(%rbp), %rax       ## 8-byte Reload
	movq	%rax, -960(%rbp)
	movq	-960(%rbp), %r8
	movq	%r8, -944(%rbp)
	movq	%rdx, -920(%rbp)
	movq	$0, -928(%rbp)
	movq	-920(%rbp), %rdx
	movq	-928(%rbp), %r8
	movq	%rdx, -904(%rbp)
	movq	%r8, -912(%rbp)
	movq	-904(%rbp), %rdx
	movq	-912(%rbp), %r8
	movq	%r8, (%rdx)
	movq	-936(%rbp), %rdx
	movq	%rdx, -968(%rbp)
	movq	-968(%rbp), %rdx
	movq	%rdx, -888(%rbp)
	movq	%rcx, -896(%rbp)
	movq	-896(%rbp), %rcx
	movq	-888(%rbp), %rdx
	movq	%rdx, -872(%rbp)
	movq	%rcx, -880(%rbp)
	movq	-880(%rbp), %rcx
	movq	-872(%rbp), %rdx
	movq	%rdx, (%rcx)
	movq	-952(%rbp), %rcx
	movq	%rcx, -1072(%rbp)
	movq	%rdi, -584(%rbp)
	movq	%rsi, -592(%rbp)
	movq	-584(%rbp), %rcx
	movq	-592(%rbp), %rdx
	movq	%rcx, -568(%rbp)
	movq	%rdx, -576(%rbp)
	movq	-568(%rbp), %rcx
	movq	-576(%rbp), %rdx
	movq	%rcx, -552(%rbp)
	movq	%rdx, -560(%rbp)
	movq	-552(%rbp), %rcx
	movq	(%rcx), %rcx
	movq	-560(%rbp), %rdx
	cmpq	(%rdx), %rcx
	sete	%r9b
	xorb	$1, %r9b
	testb	$1, %r9b
	jne	LBB5_1
	jmp	LBB5_2
LBB5_1:
	leaq	-1064(%rbp), %rax
	movq	%rax, -272(%rbp)
	movq	-272(%rbp), %rax
	movq	%rax, -264(%rbp)
	movq	-264(%rbp), %rax
	movq	(%rax), %rax
	addq	$16, %rax
	movq	%rax, -256(%rbp)
	movq	-256(%rbp), %rax
	movq	%rax, -248(%rbp)
	movq	-248(%rbp), %rax
	movq	%rax, -240(%rbp)
	movq	-240(%rbp), %rax
	movq	%rax, -232(%rbp)
	movq	-232(%rbp), %rax
	addq	$8, %rax
	movq	%rax, -1040(%rbp)
	jmp	LBB5_23
LBB5_2:
	movq	-1056(%rbp), %rdx
	leaq	-1096(%rbp), %rax
	movq	%rax, %rdi
	movq	-1152(%rbp), %rsi       ## 8-byte Reload
	movq	%rax, -1160(%rbp)       ## 8-byte Spill
	callq	__ZNSt3__113unordered_mapIiPN5lunar12green_thread7contextENS_4hashIiEENS_8equal_toIiEENS_9allocatorINS_4pairIKiS4_EEEEE25__construct_node_with_keyERSB_
	movq	-1160(%rbp), %rax       ## 8-byte Reload
	movq	%rax, -24(%rbp)
	movq	%rax, -16(%rbp)
	movq	%rax, -8(%rbp)
	movq	-1096(%rbp), %rsi
Ltmp25:
	movq	-1152(%rbp), %rdi       ## 8-byte Reload
	callq	__ZNSt3__112__hash_tableINS_17__hash_value_typeIiPN5lunar12green_thread7contextEEENS_22__unordered_map_hasherIiS6_NS_4hashIiEELb1EEENS_21__unordered_map_equalIiS6_NS_8equal_toIiEELb1EEENS_9allocatorIS6_EEE20__node_insert_uniqueEPNS_11__hash_nodeIS6_PvEE
Ltmp26:
	movb	%dl, -1161(%rbp)        ## 1-byte Spill
	movq	%rax, -1176(%rbp)       ## 8-byte Spill
	jmp	LBB5_3
LBB5_3:
	leaq	-1128(%rbp), %rax
	leaq	-1112(%rbp), %rcx
	movq	-1176(%rbp), %rdx       ## 8-byte Reload
	movq	%rdx, -1128(%rbp)
	movb	-1161(%rbp), %sil       ## 1-byte Reload
	movb	%sil, -1120(%rbp)
	movq	%rcx, -112(%rbp)
	movq	%rax, -120(%rbp)
	movq	$0, -128(%rbp)
	movq	-112(%rbp), %rax
	movq	-128(%rbp), %rcx
	movq	-120(%rbp), %rdx
	movq	%rax, -80(%rbp)
	movq	%rdx, -88(%rbp)
	movq	%rcx, -96(%rbp)
	movq	-80(%rbp), %rax
	movq	-88(%rbp), %rcx
	movq	%rcx, -72(%rbp)
	movq	-72(%rbp), %rcx
	movq	(%rcx), %rcx
	movq	%rcx, -104(%rbp)
	movq	-104(%rbp), %rcx
	movq	%rcx, -48(%rbp)
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rcx
	movq	-48(%rbp), %rdx
	movq	%rdx, -32(%rbp)
	movq	%rcx, -40(%rbp)
	movq	-40(%rbp), %rcx
	movq	-32(%rbp), %rdx
	movq	%rdx, (%rcx)
	movq	-88(%rbp), %rcx
	addq	$8, %rcx
	movq	%rcx, -64(%rbp)
	movq	-64(%rbp), %rcx
	movb	(%rcx), %sil
	andb	$1, %sil
	movb	%sil, 8(%rax)
## BB#4:
	leaq	-1112(%rbp), %rax
	leaq	-1096(%rbp), %rcx
	movq	%rcx, -168(%rbp)
	movq	-168(%rbp), %rcx
	movq	%rcx, -160(%rbp)
	movq	-160(%rbp), %rdx
	movq	%rdx, -152(%rbp)
	movq	-152(%rbp), %rdx
	movq	(%rdx), %rdx
	movq	%rdx, -176(%rbp)
	movq	%rcx, -144(%rbp)
	movq	-144(%rbp), %rcx
	movq	%rcx, -136(%rbp)
	movq	-136(%rbp), %rcx
	movq	$0, (%rcx)
	movq	%rax, -224(%rbp)
	movq	-224(%rbp), %rax
	movq	%rax, -216(%rbp)
	movq	-216(%rbp), %rax
	movq	(%rax), %rax
	addq	$16, %rax
	movq	%rax, -208(%rbp)
	movq	-208(%rbp), %rax
	movq	%rax, -200(%rbp)
	movq	-200(%rbp), %rax
	movq	%rax, -192(%rbp)
	movq	-192(%rbp), %rax
	movq	%rax, -184(%rbp)
	movq	-184(%rbp), %rax
	movq	%rax, -1184(%rbp)       ## 8-byte Spill
## BB#5:
	leaq	-1096(%rbp), %rax
	movq	-1184(%rbp), %rcx       ## 8-byte Reload
	addq	$8, %rcx
	movq	%rcx, -1040(%rbp)
	movl	$1, -1144(%rbp)
	movq	%rax, -544(%rbp)
	movq	-544(%rbp), %rax
	movq	%rax, -536(%rbp)
	movq	-536(%rbp), %rax
	movq	%rax, -512(%rbp)
	movq	$0, -520(%rbp)
	movq	-512(%rbp), %rax
	movq	%rax, -504(%rbp)
	movq	-504(%rbp), %rcx
	movq	%rcx, -496(%rbp)
	movq	-496(%rbp), %rcx
	movq	(%rcx), %rcx
	movq	%rcx, -528(%rbp)
	movq	-520(%rbp), %rcx
	movq	%rax, -304(%rbp)
	movq	-304(%rbp), %rdx
	movq	%rdx, -296(%rbp)
	movq	-296(%rbp), %rdx
	movq	%rcx, (%rdx)
	cmpq	$0, -528(%rbp)
	movq	%rax, -1192(%rbp)       ## 8-byte Spill
	je	LBB5_13
## BB#6:
	movq	-1192(%rbp), %rax       ## 8-byte Reload
	movq	%rax, -288(%rbp)
	movq	-288(%rbp), %rcx
	movq	%rcx, -280(%rbp)
	movq	-280(%rbp), %rcx
	addq	$8, %rcx
	movq	-528(%rbp), %rdx
	movq	%rcx, -480(%rbp)
	movq	%rdx, -488(%rbp)
	movq	-480(%rbp), %rcx
	testb	$1, 9(%rcx)
	movq	%rcx, -1200(%rbp)       ## 8-byte Spill
	je	LBB5_8
## BB#7:
	movq	-1200(%rbp), %rax       ## 8-byte Reload
	movq	(%rax), %rcx
	movq	-488(%rbp), %rdx
	addq	$16, %rdx
	addq	$8, %rdx
	movq	%rdx, -472(%rbp)
	movq	-472(%rbp), %rdx
	movq	%rcx, -440(%rbp)
	movq	%rdx, -448(%rbp)
	movq	-440(%rbp), %rcx
	movq	-448(%rbp), %rdx
	movq	%rcx, -424(%rbp)
	movq	%rdx, -432(%rbp)
LBB5_8:
	movq	-1200(%rbp), %rax       ## 8-byte Reload
	testb	$1, 8(%rax)
	je	LBB5_10
## BB#9:
	movq	-1200(%rbp), %rax       ## 8-byte Reload
	movq	(%rax), %rcx
	movq	-488(%rbp), %rdx
	addq	$16, %rdx
	movq	%rdx, -360(%rbp)
	movq	-360(%rbp), %rdx
	movq	%rcx, -328(%rbp)
	movq	%rdx, -336(%rbp)
	movq	-328(%rbp), %rcx
	movq	-336(%rbp), %rdx
	movq	%rcx, -312(%rbp)
	movq	%rdx, -320(%rbp)
LBB5_10:
	cmpq	$0, -488(%rbp)
	je	LBB5_12
## BB#11:
	movq	-1200(%rbp), %rax       ## 8-byte Reload
	movq	(%rax), %rcx
	movq	-488(%rbp), %rdx
	movq	%rcx, -400(%rbp)
	movq	%rdx, -408(%rbp)
	movq	$1, -416(%rbp)
	movq	-400(%rbp), %rcx
	movq	-408(%rbp), %rdx
	movq	-416(%rbp), %rsi
	movq	%rcx, -376(%rbp)
	movq	%rdx, -384(%rbp)
	movq	%rsi, -392(%rbp)
	movq	-384(%rbp), %rcx
	movq	%rcx, -368(%rbp)
	movq	-368(%rbp), %rdi
	callq	__ZdlPv
LBB5_12:                                ## %_ZNSt3__126__hash_map_node_destructorINS_9allocatorINS_11__hash_nodeINS_17__hash_value_typeIiPN5lunar12green_thread7contextEEEPvEEEEEclEPSA_.exit.i.i.i4
	jmp	LBB5_13
LBB5_13:                                ## %_ZNSt3__110unique_ptrINS_11__hash_nodeINS_17__hash_value_typeIiPN5lunar12green_thread7contextEEEPvEENS_26__hash_map_node_destructorINS_9allocatorIS9_EEEEED1Ev.exit5
	jmp	LBB5_23
LBB5_14:
Ltmp27:
	leaq	-1096(%rbp), %rcx
	movl	%edx, %esi
	movq	%rax, -1136(%rbp)
	movl	%esi, -1140(%rbp)
	movq	%rcx, -864(%rbp)
	movq	-864(%rbp), %rax
	movq	%rax, -856(%rbp)
	movq	-856(%rbp), %rax
	movq	%rax, -832(%rbp)
	movq	$0, -840(%rbp)
	movq	-832(%rbp), %rax
	movq	%rax, -824(%rbp)
	movq	-824(%rbp), %rcx
	movq	%rcx, -816(%rbp)
	movq	-816(%rbp), %rcx
	movq	(%rcx), %rcx
	movq	%rcx, -848(%rbp)
	movq	-840(%rbp), %rcx
	movq	%rax, -624(%rbp)
	movq	-624(%rbp), %rdx
	movq	%rdx, -616(%rbp)
	movq	-616(%rbp), %rdx
	movq	%rcx, (%rdx)
	cmpq	$0, -848(%rbp)
	movq	%rax, -1208(%rbp)       ## 8-byte Spill
	je	LBB5_22
## BB#15:
	movq	-1208(%rbp), %rax       ## 8-byte Reload
	movq	%rax, -608(%rbp)
	movq	-608(%rbp), %rcx
	movq	%rcx, -600(%rbp)
	movq	-600(%rbp), %rcx
	addq	$8, %rcx
	movq	-848(%rbp), %rdx
	movq	%rcx, -800(%rbp)
	movq	%rdx, -808(%rbp)
	movq	-800(%rbp), %rcx
	testb	$1, 9(%rcx)
	movq	%rcx, -1216(%rbp)       ## 8-byte Spill
	je	LBB5_17
## BB#16:
	movq	-1216(%rbp), %rax       ## 8-byte Reload
	movq	(%rax), %rcx
	movq	-808(%rbp), %rdx
	addq	$16, %rdx
	addq	$8, %rdx
	movq	%rdx, -792(%rbp)
	movq	-792(%rbp), %rdx
	movq	%rcx, -760(%rbp)
	movq	%rdx, -768(%rbp)
	movq	-760(%rbp), %rcx
	movq	-768(%rbp), %rdx
	movq	%rcx, -744(%rbp)
	movq	%rdx, -752(%rbp)
LBB5_17:
	movq	-1216(%rbp), %rax       ## 8-byte Reload
	testb	$1, 8(%rax)
	je	LBB5_19
## BB#18:
	movq	-1216(%rbp), %rax       ## 8-byte Reload
	movq	(%rax), %rcx
	movq	-808(%rbp), %rdx
	addq	$16, %rdx
	movq	%rdx, -680(%rbp)
	movq	-680(%rbp), %rdx
	movq	%rcx, -648(%rbp)
	movq	%rdx, -656(%rbp)
	movq	-648(%rbp), %rcx
	movq	-656(%rbp), %rdx
	movq	%rcx, -632(%rbp)
	movq	%rdx, -640(%rbp)
LBB5_19:
	cmpq	$0, -808(%rbp)
	je	LBB5_21
## BB#20:
	movq	-1216(%rbp), %rax       ## 8-byte Reload
	movq	(%rax), %rcx
	movq	-808(%rbp), %rdx
	movq	%rcx, -720(%rbp)
	movq	%rdx, -728(%rbp)
	movq	$1, -736(%rbp)
	movq	-720(%rbp), %rcx
	movq	-728(%rbp), %rdx
	movq	-736(%rbp), %rsi
	movq	%rcx, -696(%rbp)
	movq	%rdx, -704(%rbp)
	movq	%rsi, -712(%rbp)
	movq	-704(%rbp), %rcx
	movq	%rcx, -688(%rbp)
	movq	-688(%rbp), %rdi
	callq	__ZdlPv
LBB5_21:                                ## %_ZNSt3__126__hash_map_node_destructorINS_9allocatorINS_11__hash_nodeINS_17__hash_value_typeIiPN5lunar12green_thread7contextEEEPvEEEEEclEPSA_.exit.i.i.i
	jmp	LBB5_22
LBB5_22:                                ## %_ZNSt3__110unique_ptrINS_11__hash_nodeINS_17__hash_value_typeIiPN5lunar12green_thread7contextEEEPvEENS_26__hash_map_node_destructorINS_9allocatorIS9_EEEEED1Ev.exit
	jmp	LBB5_24
LBB5_23:
	movq	-1040(%rbp), %rax
	addq	$1216, %rsp             ## imm = 0x4C0
	popq	%rbp
	retq
LBB5_24:
	movq	-1136(%rbp), %rdi
	callq	__Unwind_Resume
Lfunc_end5:
	.cfi_endproc
	.section	__TEXT,__gcc_except_tab
	.align	2
GCC_except_table5:
Lexception5:
	.byte	255                     ## @LPStart Encoding = omit
	.byte	155                     ## @TType Encoding = indirect pcrel sdata4
	.byte	41                      ## @TType base offset
	.byte	3                       ## Call site Encoding = udata4
	.byte	39                      ## Call site table length
Lset12 = Lfunc_begin5-Lfunc_begin5      ## >> Call Site 1 <<
	.long	Lset12
Lset13 = Ltmp25-Lfunc_begin5            ##   Call between Lfunc_begin5 and Ltmp25
	.long	Lset13
	.long	0                       ##     has no landing pad
	.byte	0                       ##   On action: cleanup
Lset14 = Ltmp25-Lfunc_begin5            ## >> Call Site 2 <<
	.long	Lset14
Lset15 = Ltmp26-Ltmp25                  ##   Call between Ltmp25 and Ltmp26
	.long	Lset15
Lset16 = Ltmp27-Lfunc_begin5            ##     jumps to Ltmp27
	.long	Lset16
	.byte	0                       ##   On action: cleanup
Lset17 = Ltmp26-Lfunc_begin5            ## >> Call Site 3 <<
	.long	Lset17
Lset18 = Lfunc_end5-Ltmp26              ##   Call between Ltmp26 and Lfunc_end5
	.long	Lset18
	.long	0                       ##     has no landing pad
	.byte	0                       ##   On action: cleanup
	.align	2

	.section	__TEXT,__textcoal_nt,coalesced,pure_instructions
	.globl	__ZNSt3__14listINS_10unique_ptrIN5lunar12green_thread7contextENS_14default_deleteIS4_EEEENS_9allocatorIS7_EEE9push_backEOS7_
	.weak_def_can_be_hidden	__ZNSt3__14listINS_10unique_ptrIN5lunar12green_thread7contextENS_14default_deleteIS4_EEEENS_9allocatorIS7_EEE9push_backEOS7_
	.align	4, 0x90
__ZNSt3__14listINS_10unique_ptrIN5lunar12green_thread7contextENS_14default_deleteIS4_EEEENS_9allocatorIS7_EEE9push_backEOS7_: ## @_ZNSt3__14listINS_10unique_ptrIN5lunar12green_thread7contextENS_14default_deleteIS4_EEEENS_9allocatorIS7_EEE9push_backEOS7_
	.cfi_startproc
## BB#0:
	pushq	%rbp
Ltmp31:
	.cfi_def_cfa_offset 16
Ltmp32:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
Ltmp33:
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	subq	$1320, %rsp             ## imm = 0x528
Ltmp34:
	.cfi_offset %rbx, -56
Ltmp35:
	.cfi_offset %r12, -48
Ltmp36:
	.cfi_offset %r13, -40
Ltmp37:
	.cfi_offset %r14, -32
Ltmp38:
	.cfi_offset %r15, -24
	movq	%rdi, -1264(%rbp)
	movq	%rsi, -1272(%rbp)
	movq	-1264(%rbp), %rsi
	movq	%rsi, %rdi
	movq	%rdi, -1256(%rbp)
	movq	-1256(%rbp), %rdi
	addq	$16, %rdi
	movq	%rdi, -1248(%rbp)
	movq	-1248(%rbp), %rdi
	movq	%rdi, -1240(%rbp)
	movq	-1240(%rbp), %rdi
	movq	%rdi, -1280(%rbp)
	movq	-1280(%rbp), %rdi
	movq	%rdi, -1064(%rbp)
	movq	$1, -1072(%rbp)
	movq	-1064(%rbp), %rdi
	movq	-1072(%rbp), %rax
	movq	%rdi, -1040(%rbp)
	movq	%rax, -1048(%rbp)
	movq	$0, -1056(%rbp)
	imulq	$24, -1048(%rbp), %rax
	movq	%rax, -1032(%rbp)
	movq	-1032(%rbp), %rdi
	movq	%rsi, -1344(%rbp)       ## 8-byte Spill
	callq	__Znwm
	leaq	-136(%rbp), %rsi
	leaq	-120(%rbp), %rdi
	leaq	-152(%rbp), %rcx
	leaq	-168(%rbp), %rdx
	leaq	-1304(%rbp), %r8
	leaq	-640(%rbp), %r9
	leaq	-656(%rbp), %r10
	leaq	-680(%rbp), %r11
	leaq	-696(%rbp), %rbx
	leaq	-1320(%rbp), %r14
	movq	-1280(%rbp), %r15
	movq	%r14, -1008(%rbp)
	movq	%r15, -1016(%rbp)
	movq	$1, -1024(%rbp)
	movq	-1008(%rbp), %r15
	movq	-1024(%rbp), %r12
	movq	-1016(%rbp), %r13
	movq	%r15, -984(%rbp)
	movq	%r13, -992(%rbp)
	movq	%r12, -1000(%rbp)
	movq	-984(%rbp), %r15
	movq	-992(%rbp), %r12
	movq	%r12, (%r15)
	movq	-1000(%rbp), %r12
	movq	%r12, 8(%r15)
	movq	%r8, -800(%rbp)
	movq	%rax, -808(%rbp)
	movq	%r14, -816(%rbp)
	movq	-800(%rbp), %rax
	movq	-808(%rbp), %r14
	movq	-816(%rbp), %r15
	movq	%rax, -760(%rbp)
	movq	%r14, -768(%rbp)
	movq	%r15, -776(%rbp)
	movq	-760(%rbp), %rax
	movq	-768(%rbp), %r14
	movq	-776(%rbp), %r15
	movq	%r15, -752(%rbp)
	movq	-752(%rbp), %r15
	movq	(%r15), %r12
	movq	%r12, -792(%rbp)
	movq	8(%r15), %r15
	movq	%r15, -784(%rbp)
	movq	-792(%rbp), %r15
	movq	-784(%rbp), %r12
	movq	%r15, -728(%rbp)
	movq	%r12, -720(%rbp)
	movq	%rax, -736(%rbp)
	movq	%r14, -744(%rbp)
	movq	-736(%rbp), %rax
	movq	-744(%rbp), %r14
	movq	-728(%rbp), %r15
	movq	-720(%rbp), %r12
	movq	%r15, -680(%rbp)
	movq	%r12, -672(%rbp)
	movq	%rax, -688(%rbp)
	movq	%r14, -696(%rbp)
	movq	-688(%rbp), %rax
	movq	%rbx, -664(%rbp)
	movq	-664(%rbp), %rbx
	movq	(%rbx), %rbx
	movq	%r11, -608(%rbp)
	movq	-608(%rbp), %r11
	movq	(%r11), %r14
	movq	%r14, -712(%rbp)
	movq	8(%r11), %r11
	movq	%r11, -704(%rbp)
	movq	-712(%rbp), %r11
	movq	-704(%rbp), %r14
	movq	%r11, -640(%rbp)
	movq	%r14, -632(%rbp)
	movq	%rax, -648(%rbp)
	movq	%rbx, -656(%rbp)
	movq	-648(%rbp), %rax
	movq	%r10, -624(%rbp)
	movq	-624(%rbp), %r10
	movq	(%r10), %r10
	movq	%r10, (%rax)
	movq	%r9, -616(%rbp)
	movq	-616(%rbp), %r9
	movq	(%r9), %r10
	movq	%r10, 8(%rax)
	movq	8(%r9), %r9
	movq	%r9, 16(%rax)
	movq	-1280(%rbp), %rax
	movq	%r8, -64(%rbp)
	movq	-64(%rbp), %r8
	movq	%r8, -56(%rbp)
	movq	-56(%rbp), %r8
	movq	%r8, -48(%rbp)
	movq	-48(%rbp), %r8
	movq	(%r8), %r8
	addq	$16, %r8
	movq	%r8, -72(%rbp)
	movq	-72(%rbp), %r8
	movq	-1272(%rbp), %r9
	movq	%r9, -80(%rbp)
	movq	-80(%rbp), %r9
	movq	%rax, -400(%rbp)
	movq	%r8, -408(%rbp)
	movq	%r9, -416(%rbp)
	movq	-400(%rbp), %rax
	movq	-408(%rbp), %r8
	movq	-416(%rbp), %r9
	movq	%r9, -392(%rbp)
	movq	-392(%rbp), %r9
	movq	%rax, -368(%rbp)
	movq	%r8, -376(%rbp)
	movq	%r9, -384(%rbp)
	movq	-368(%rbp), %rax
	movq	-376(%rbp), %r8
	movq	-384(%rbp), %r9
	movq	%r9, -352(%rbp)
	movq	-352(%rbp), %r9
	movq	%rax, -328(%rbp)
	movq	%r8, -336(%rbp)
	movq	%r9, -344(%rbp)
	movq	-336(%rbp), %rax
	movq	-344(%rbp), %r8
	movq	%r8, -320(%rbp)
	movq	-320(%rbp), %r8
	movq	%rax, -304(%rbp)
	movq	%r8, -312(%rbp)
	movq	-304(%rbp), %rax
	movq	-312(%rbp), %r8
	movq	%rax, -280(%rbp)
	movq	%r8, -288(%rbp)
	movq	-280(%rbp), %rax
	movq	-288(%rbp), %r8
	movq	%r8, -264(%rbp)
	movq	-264(%rbp), %r8
	movq	%r8, -256(%rbp)
	movq	-256(%rbp), %r9
	movq	%r9, -248(%rbp)
	movq	-248(%rbp), %r9
	movq	(%r9), %r9
	movq	%r9, -272(%rbp)
	movq	%r8, -240(%rbp)
	movq	-240(%rbp), %r8
	movq	%r8, -232(%rbp)
	movq	-232(%rbp), %r8
	movq	$0, (%r8)
	movq	-272(%rbp), %r8
	movq	-288(%rbp), %r9
	movq	%r9, -224(%rbp)
	movq	-224(%rbp), %r9
	movq	%r9, -216(%rbp)
	movq	-216(%rbp), %r9
	movq	%r9, -208(%rbp)
	movq	-208(%rbp), %r9
	movq	%r9, -88(%rbp)
	movq	%rax, -192(%rbp)
	movq	%r8, -200(%rbp)
	movq	-192(%rbp), %rax
	movq	-200(%rbp), %r8
	movq	%rax, -160(%rbp)
	movq	%r8, -168(%rbp)
	movq	-160(%rbp), %rax
	movq	%rdx, -144(%rbp)
	movq	-144(%rbp), %rdx
	movq	(%rdx), %rdx
	movq	%rcx, -96(%rbp)
	movq	%rax, -128(%rbp)
	movq	%rdx, -136(%rbp)
	movq	-128(%rbp), %rax
	movq	%rdi, -112(%rbp)
	movq	%rsi, -104(%rbp)
	movq	-104(%rbp), %rcx
	movq	(%rcx), %rcx
	movq	%rcx, (%rax)
## BB#1:
	leaq	-1304(%rbp), %rax
	movq	%rax, -456(%rbp)
	movq	-456(%rbp), %rcx
	movq	%rcx, -448(%rbp)
	movq	-448(%rbp), %rcx
	movq	%rcx, -440(%rbp)
	movq	-440(%rbp), %rcx
	movq	(%rcx), %rcx
	movq	%rax, -480(%rbp)
	movq	-480(%rbp), %rax
	movq	%rax, -472(%rbp)
	movq	-472(%rbp), %rax
	movq	%rax, -464(%rbp)
	movq	-464(%rbp), %rax
	movq	(%rax), %rax
	movq	-1344(%rbp), %rdx       ## 8-byte Reload
	movq	%rdx, -512(%rbp)
	movq	%rcx, -520(%rbp)
	movq	%rax, -528(%rbp)
	movq	-512(%rbp), %rax
	movq	%rax, %rcx
	movq	%rcx, -504(%rbp)
	movq	-504(%rbp), %rcx
	movq	%rcx, -496(%rbp)
	movq	-496(%rbp), %rcx
	movq	%rcx, -488(%rbp)
	movq	-488(%rbp), %rcx
	movq	-528(%rbp), %rsi
	movq	%rcx, 8(%rsi)
	movq	(%rax), %rcx
	movq	-520(%rbp), %rsi
	movq	%rcx, (%rsi)
	movq	-520(%rbp), %rcx
	movq	-520(%rbp), %rsi
	movq	(%rsi), %rsi
	movq	%rcx, 8(%rsi)
	movq	-528(%rbp), %rcx
	movq	%rcx, (%rax)
## BB#2:
	leaq	-1304(%rbp), %rax
	movq	-1344(%rbp), %rcx       ## 8-byte Reload
	movq	%rcx, -552(%rbp)
	movq	-552(%rbp), %rcx
	addq	$16, %rcx
	movq	%rcx, -544(%rbp)
	movq	-544(%rbp), %rcx
	movq	%rcx, -536(%rbp)
	movq	-536(%rbp), %rcx
	movq	(%rcx), %rdx
	addq	$1, %rdx
	movq	%rdx, (%rcx)
	movq	%rax, -592(%rbp)
	movq	-592(%rbp), %rcx
	movq	%rcx, -584(%rbp)
	movq	-584(%rbp), %rdx
	movq	%rdx, -576(%rbp)
	movq	-576(%rbp), %rdx
	movq	(%rdx), %rdx
	movq	%rdx, -600(%rbp)
	movq	%rcx, -568(%rbp)
	movq	-568(%rbp), %rcx
	movq	%rcx, -560(%rbp)
	movq	-560(%rbp), %rcx
	movq	$0, (%rcx)
	movq	%rax, -976(%rbp)
	movq	-976(%rbp), %rax
	movq	%rax, -968(%rbp)
	movq	-968(%rbp), %rax
	movq	%rax, -944(%rbp)
	movq	$0, -952(%rbp)
	movq	-944(%rbp), %rax
	movq	%rax, -936(%rbp)
	movq	-936(%rbp), %rcx
	movq	%rcx, -928(%rbp)
	movq	-928(%rbp), %rcx
	movq	(%rcx), %rcx
	movq	%rcx, -960(%rbp)
	movq	-952(%rbp), %rcx
	movq	%rax, -848(%rbp)
	movq	-848(%rbp), %rdx
	movq	%rdx, -840(%rbp)
	movq	-840(%rbp), %rdx
	movq	%rcx, (%rdx)
	cmpq	$0, -960(%rbp)
	movq	%rax, -1352(%rbp)       ## 8-byte Spill
	je	LBB6_4
## BB#3:
	movq	-1352(%rbp), %rax       ## 8-byte Reload
	movq	%rax, -832(%rbp)
	movq	-832(%rbp), %rcx
	movq	%rcx, -824(%rbp)
	movq	-824(%rbp), %rcx
	addq	$8, %rcx
	movq	-960(%rbp), %rdx
	movq	%rcx, -912(%rbp)
	movq	%rdx, -920(%rbp)
	movq	-912(%rbp), %rcx
	movq	(%rcx), %rdx
	movq	-920(%rbp), %rsi
	movq	8(%rcx), %rcx
	movq	%rdx, -888(%rbp)
	movq	%rsi, -896(%rbp)
	movq	%rcx, -904(%rbp)
	movq	-888(%rbp), %rcx
	movq	-896(%rbp), %rdx
	movq	-904(%rbp), %rsi
	movq	%rcx, -864(%rbp)
	movq	%rdx, -872(%rbp)
	movq	%rsi, -880(%rbp)
	movq	-872(%rbp), %rcx
	movq	%rcx, -856(%rbp)
	movq	-856(%rbp), %rdi
	callq	__ZdlPv
LBB6_4:                                 ## %_ZNSt3__110unique_ptrINS_11__list_nodeINS0_IN5lunar12green_thread7contextENS_14default_deleteIS4_EEEEPvEENS_22__allocator_destructorINS_9allocatorIS9_EEEEED1Ev.exit2
	addq	$1320, %rsp             ## imm = 0x528
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
	.cfi_endproc

	.section	__TEXT,__text,regular,pure_instructions
	.globl	__ZN5lunar12green_thread3runEv
	.align	4, 0x90
__ZN5lunar12green_thread3runEv:         ## @_ZN5lunar12green_thread3runEv
	.cfi_startproc
## BB#0:
	pushq	%rbp
Ltmp39:
	.cfi_def_cfa_offset 16
Ltmp40:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
Ltmp41:
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rdi
	movq	%rdi, -16(%rbp)         ## 8-byte Spill
	callq	_setjmp
	cmpl	$0, %eax
	jne	LBB7_2
## BB#1:
	movq	-16(%rbp), %rdi         ## 8-byte Reload
	callq	__ZN5lunar12green_thread5yieldEv
LBB7_2:
	addq	$16, %rsp
	popq	%rbp
	retq
	.cfi_endproc

	.section	__TEXT,__textcoal_nt,coalesced,pure_instructions
	.globl	__ZNSt3__14listINS_10unique_ptrIN5lunar12green_thread7contextENS_14default_deleteIS4_EEEENS_9allocatorIS7_EEE6spliceENS_21__list_const_iteratorIS7_PvEERSA_SD_
	.weak_def_can_be_hidden	__ZNSt3__14listINS_10unique_ptrIN5lunar12green_thread7contextENS_14default_deleteIS4_EEEENS_9allocatorIS7_EEE6spliceENS_21__list_const_iteratorIS7_PvEERSA_SD_
	.align	4, 0x90
__ZNSt3__14listINS_10unique_ptrIN5lunar12green_thread7contextENS_14default_deleteIS4_EEEENS_9allocatorIS7_EEE6spliceENS_21__list_const_iteratorIS7_PvEERSA_SD_: ## @_ZNSt3__14listINS_10unique_ptrIN5lunar12green_thread7contextENS_14default_deleteIS4_EEEENS_9allocatorIS7_EEE6spliceENS_21__list_const_iteratorIS7_PvEERSA_SD_
	.cfi_startproc
## BB#0:
	pushq	%rbp
Ltmp42:
	.cfi_def_cfa_offset 16
Ltmp43:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
Ltmp44:
	.cfi_def_cfa_register %rbp
	pushq	%rax
	movq	%rsi, -96(%rbp)
	movq	%rcx, -104(%rbp)
	movq	%rdi, -112(%rbp)
	movq	%rdx, -120(%rbp)
	movq	-112(%rbp), %rcx
	movq	-96(%rbp), %rdx
	cmpq	-104(%rbp), %rdx
	movq	%rcx, -136(%rbp)        ## 8-byte Spill
	je	LBB8_3
## BB#1:
	movq	-96(%rbp), %rax
	movq	-104(%rbp), %rcx
	cmpq	8(%rcx), %rax
	je	LBB8_3
## BB#2:
	movq	-104(%rbp), %rax
	movq	%rax, -128(%rbp)
	movq	-128(%rbp), %rax
	movq	-128(%rbp), %rcx
	movq	%rax, -80(%rbp)
	movq	%rcx, -88(%rbp)
	movq	-88(%rbp), %rax
	movq	8(%rax), %rax
	movq	-80(%rbp), %rcx
	movq	(%rcx), %rcx
	movq	%rax, 8(%rcx)
	movq	-80(%rbp), %rax
	movq	(%rax), %rax
	movq	-88(%rbp), %rcx
	movq	8(%rcx), %rcx
	movq	%rax, (%rcx)
	movq	-96(%rbp), %rax
	movq	-128(%rbp), %rcx
	movq	-128(%rbp), %rdx
	movq	%rax, -8(%rbp)
	movq	%rcx, -16(%rbp)
	movq	%rdx, -24(%rbp)
	movq	-16(%rbp), %rax
	movq	-8(%rbp), %rcx
	movq	(%rcx), %rcx
	movq	%rax, 8(%rcx)
	movq	-8(%rbp), %rax
	movq	(%rax), %rax
	movq	-16(%rbp), %rcx
	movq	%rax, (%rcx)
	movq	-24(%rbp), %rax
	movq	-8(%rbp), %rcx
	movq	%rax, (%rcx)
	movq	-8(%rbp), %rax
	movq	-24(%rbp), %rcx
	movq	%rax, 8(%rcx)
	movq	-120(%rbp), %rax
	movq	%rax, -48(%rbp)
	movq	-48(%rbp), %rax
	addq	$16, %rax
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	movq	%rax, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	(%rax), %rcx
	addq	$-1, %rcx
	movq	%rcx, (%rax)
	movq	-136(%rbp), %rax        ## 8-byte Reload
	movq	%rax, -72(%rbp)
	movq	-72(%rbp), %rax
	addq	$16, %rax
	movq	%rax, -64(%rbp)
	movq	-64(%rbp), %rax
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	movq	(%rax), %rcx
	addq	$1, %rcx
	movq	%rcx, (%rax)
LBB8_3:
	addq	$8, %rsp
	popq	%rbp
	retq
	.cfi_endproc

	.globl	__ZNSt3__14listINS_10unique_ptrIN5lunar12green_thread7contextENS_14default_deleteIS4_EEEENS_9allocatorIS7_EEE5eraseENS_21__list_const_iteratorIS7_PvEE
	.weak_def_can_be_hidden	__ZNSt3__14listINS_10unique_ptrIN5lunar12green_thread7contextENS_14default_deleteIS4_EEEENS_9allocatorIS7_EEE5eraseENS_21__list_const_iteratorIS7_PvEE
	.align	4, 0x90
__ZNSt3__14listINS_10unique_ptrIN5lunar12green_thread7contextENS_14default_deleteIS4_EEEENS_9allocatorIS7_EEE5eraseENS_21__list_const_iteratorIS7_PvEE: ## @_ZNSt3__14listINS_10unique_ptrIN5lunar12green_thread7contextENS_14default_deleteIS4_EEEENS_9allocatorIS7_EEE5eraseENS_21__list_const_iteratorIS7_PvEE
	.cfi_startproc
## BB#0:
	pushq	%rbp
Ltmp45:
	.cfi_def_cfa_offset 16
Ltmp46:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
Ltmp47:
	.cfi_def_cfa_register %rbp
	subq	$384, %rsp              ## imm = 0x180
	movq	%rsi, -336(%rbp)
	movq	%rdi, -344(%rbp)
	movq	-344(%rbp), %rsi
	movq	%rsi, %rdi
	movq	%rdi, -320(%rbp)
	movq	-320(%rbp), %rdi
	addq	$16, %rdi
	movq	%rdi, -312(%rbp)
	movq	-312(%rbp), %rdi
	movq	%rdi, -304(%rbp)
	movq	-304(%rbp), %rdi
	movq	%rdi, -352(%rbp)
	movq	-336(%rbp), %rdi
	movq	%rdi, -360(%rbp)
	movq	-360(%rbp), %rdi
	movq	8(%rdi), %rdi
	movq	%rdi, -368(%rbp)
	movq	-360(%rbp), %rdi
	movq	-360(%rbp), %rax
	movq	%rdi, -200(%rbp)
	movq	%rax, -208(%rbp)
	movq	-208(%rbp), %rax
	movq	8(%rax), %rax
	movq	-200(%rbp), %rdi
	movq	(%rdi), %rdi
	movq	%rax, 8(%rdi)
	movq	-200(%rbp), %rax
	movq	(%rax), %rax
	movq	-208(%rbp), %rdi
	movq	8(%rdi), %rdi
	movq	%rax, (%rdi)
	movq	%rsi, -32(%rbp)
	movq	-32(%rbp), %rax
	addq	$16, %rax
	movq	%rax, -24(%rbp)
	movq	-24(%rbp), %rax
	movq	%rax, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	(%rax), %rsi
	addq	$-1, %rsi
	movq	%rsi, (%rax)
	movq	-352(%rbp), %rax
	movq	-360(%rbp), %rsi
	addq	$16, %rsi
	movq	%rsi, -8(%rbp)
	movq	-8(%rbp), %rsi
	movq	%rax, -168(%rbp)
	movq	%rsi, -176(%rbp)
	movq	-168(%rbp), %rax
	movq	-176(%rbp), %rsi
	movq	%rax, -152(%rbp)
	movq	%rsi, -160(%rbp)
	movq	-160(%rbp), %rax
	movq	%rax, -136(%rbp)
	movq	-136(%rbp), %rax
	movq	%rax, -128(%rbp)
	movq	-128(%rbp), %rax
	movq	%rax, -104(%rbp)
	movq	$0, -112(%rbp)
	movq	-104(%rbp), %rax
	movq	%rax, -96(%rbp)
	movq	-96(%rbp), %rsi
	movq	%rsi, -88(%rbp)
	movq	-88(%rbp), %rsi
	movq	(%rsi), %rsi
	movq	%rsi, -120(%rbp)
	movq	-112(%rbp), %rsi
	movq	%rax, -64(%rbp)
	movq	-64(%rbp), %rdi
	movq	%rdi, -56(%rbp)
	movq	-56(%rbp), %rdi
	movq	%rsi, (%rdi)
	cmpq	$0, -120(%rbp)
	movq	%rax, -376(%rbp)        ## 8-byte Spill
	je	LBB9_4
## BB#1:
	movq	-376(%rbp), %rax        ## 8-byte Reload
	movq	%rax, -48(%rbp)
	movq	-48(%rbp), %rcx
	movq	%rcx, -40(%rbp)
	movq	-40(%rbp), %rcx
	movq	-120(%rbp), %rdx
	movq	%rcx, -72(%rbp)
	movq	%rdx, -80(%rbp)
	movq	-80(%rbp), %rcx
	cmpq	$0, %rcx
	movq	%rcx, -384(%rbp)        ## 8-byte Spill
	je	LBB9_3
## BB#2:
	movq	-384(%rbp), %rdi        ## 8-byte Reload
	callq	__ZN5lunar12green_thread7contextD1Ev
	movq	-384(%rbp), %rdi        ## 8-byte Reload
	callq	__ZdlPv
LBB9_3:                                 ## %_ZNKSt3__114default_deleteIN5lunar12green_thread7contextEEclEPS3_.exit.i.i.i.i.i
	jmp	LBB9_4
LBB9_4:                                 ## %_ZNSt3__116allocator_traitsINS_9allocatorINS_11__list_nodeINS_10unique_ptrIN5lunar12green_thread7contextENS_14default_deleteIS6_EEEEPvEEEEE7destroyIS9_EEvRSC_PT_.exit
	movq	-352(%rbp), %rax
	movq	-360(%rbp), %rcx
	movq	%rax, -248(%rbp)
	movq	%rcx, -256(%rbp)
	movq	$1, -264(%rbp)
	movq	-248(%rbp), %rax
	movq	-256(%rbp), %rcx
	movq	-264(%rbp), %rdx
	movq	%rax, -224(%rbp)
	movq	%rcx, -232(%rbp)
	movq	%rdx, -240(%rbp)
	movq	-232(%rbp), %rax
	movq	%rax, -216(%rbp)
	movq	-216(%rbp), %rdi
	callq	__ZdlPv
	leaq	-328(%rbp), %rax
	movq	-368(%rbp), %rcx
	movq	%rax, -288(%rbp)
	movq	%rcx, -296(%rbp)
	movq	-288(%rbp), %rax
	movq	-296(%rbp), %rcx
	movq	%rax, -272(%rbp)
	movq	%rcx, -280(%rbp)
	movq	-272(%rbp), %rax
	movq	-280(%rbp), %rcx
	movq	%rcx, (%rax)
	movq	-328(%rbp), %rax
	addq	$384, %rsp              ## imm = 0x180
	popq	%rbp
	retq
	.cfi_endproc

	.private_extern	__ZN5lunar12green_thread7contextC1Ev
	.globl	__ZN5lunar12green_thread7contextC1Ev
	.weak_def_can_be_hidden	__ZN5lunar12green_thread7contextC1Ev
	.align	4, 0x90
__ZN5lunar12green_thread7contextC1Ev:   ## @_ZN5lunar12green_thread7contextC1Ev
	.cfi_startproc
## BB#0:
	pushq	%rbp
Ltmp48:
	.cfi_def_cfa_offset 16
Ltmp49:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
Ltmp50:
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rdi
	callq	__ZN5lunar12green_thread7contextC2Ev
	addq	$16, %rsp
	popq	%rbp
	retq
	.cfi_endproc

	.private_extern	__ZN5lunar12green_thread7contextC2Ev
	.globl	__ZN5lunar12green_thread7contextC2Ev
	.weak_def_can_be_hidden	__ZN5lunar12green_thread7contextC2Ev
	.align	4, 0x90
__ZN5lunar12green_thread7contextC2Ev:   ## @_ZN5lunar12green_thread7contextC2Ev
	.cfi_startproc
## BB#0:
	pushq	%rbp
Ltmp51:
	.cfi_def_cfa_offset 16
Ltmp52:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
Ltmp53:
	.cfi_def_cfa_register %rbp
	leaq	-32(%rbp), %rax
	leaq	-56(%rbp), %rcx
	movq	%rdi, -112(%rbp)
	movq	-112(%rbp), %rdi
	addq	$160, %rdi
	movq	%rdi, -104(%rbp)
	movq	-104(%rbp), %rdi
	movq	%rdi, -96(%rbp)
	movq	-96(%rbp), %rdi
	movq	%rdi, -88(%rbp)
	movq	-88(%rbp), %rdi
	movq	%rdi, %rdx
	movq	%rdx, -80(%rbp)
	movq	$0, (%rdi)
	movq	$0, 8(%rdi)
	addq	$16, %rdi
	movq	%rdi, -64(%rbp)
	movq	$0, -72(%rbp)
	movq	-64(%rbp), %rdx
	movq	-72(%rbp), %rdi
	movq	%rdx, -48(%rbp)
	movq	%rdi, -56(%rbp)
	movq	-48(%rbp), %rdx
	movq	%rcx, -40(%rbp)
	movq	-40(%rbp), %rcx
	movq	(%rcx), %rcx
	movq	%rdx, -24(%rbp)
	movq	%rcx, -32(%rbp)
	movq	-24(%rbp), %rcx
	movq	%rcx, %rdx
	movq	%rdx, -16(%rbp)
	movq	%rax, -8(%rbp)
	movq	-8(%rbp), %rax
	movq	(%rax), %rax
	movq	%rax, (%rcx)
	popq	%rbp
	retq
	.cfi_endproc

	.private_extern	___clang_call_terminate
	.globl	___clang_call_terminate
	.weak_def_can_be_hidden	___clang_call_terminate
	.align	4, 0x90
___clang_call_terminate:                ## @__clang_call_terminate
## BB#0:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	callq	___cxa_begin_catch
	movq	%rax, -8(%rbp)          ## 8-byte Spill
	callq	__ZSt9terminatev

	.private_extern	__ZN5lunar12green_thread7contextD1Ev
	.globl	__ZN5lunar12green_thread7contextD1Ev
	.weak_def_can_be_hidden	__ZN5lunar12green_thread7contextD1Ev
	.align	4, 0x90
__ZN5lunar12green_thread7contextD1Ev:   ## @_ZN5lunar12green_thread7contextD1Ev
	.cfi_startproc
## BB#0:
	pushq	%rbp
Ltmp54:
	.cfi_def_cfa_offset 16
Ltmp55:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
Ltmp56:
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rdi
	callq	__ZN5lunar12green_thread7contextD2Ev
	addq	$16, %rsp
	popq	%rbp
	retq
	.cfi_endproc

	.private_extern	__ZN5lunar12green_thread7contextD2Ev
	.globl	__ZN5lunar12green_thread7contextD2Ev
	.weak_def_can_be_hidden	__ZN5lunar12green_thread7contextD2Ev
	.align	4, 0x90
__ZN5lunar12green_thread7contextD2Ev:   ## @_ZN5lunar12green_thread7contextD2Ev
	.cfi_startproc
## BB#0:
	pushq	%rbp
Ltmp57:
	.cfi_def_cfa_offset 16
Ltmp58:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
Ltmp59:
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rdi
	addq	$160, %rdi
	callq	__ZNSt3__16vectorIyNS_9allocatorIyEEED1Ev
	addq	$16, %rsp
	popq	%rbp
	retq
	.cfi_endproc

	.private_extern	__ZNSt3__16vectorIyNS_9allocatorIyEEED1Ev
	.globl	__ZNSt3__16vectorIyNS_9allocatorIyEEED1Ev
	.weak_def_can_be_hidden	__ZNSt3__16vectorIyNS_9allocatorIyEEED1Ev
	.align	4, 0x90
__ZNSt3__16vectorIyNS_9allocatorIyEEED1Ev: ## @_ZNSt3__16vectorIyNS_9allocatorIyEEED1Ev
	.cfi_startproc
## BB#0:
	pushq	%rbp
Ltmp60:
	.cfi_def_cfa_offset 16
Ltmp61:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
Ltmp62:
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rdi
	callq	__ZNSt3__16vectorIyNS_9allocatorIyEEED2Ev
	addq	$16, %rsp
	popq	%rbp
	retq
	.cfi_endproc

	.private_extern	__ZNSt3__16vectorIyNS_9allocatorIyEEED2Ev
	.globl	__ZNSt3__16vectorIyNS_9allocatorIyEEED2Ev
	.weak_def_can_be_hidden	__ZNSt3__16vectorIyNS_9allocatorIyEEED2Ev
	.align	4, 0x90
__ZNSt3__16vectorIyNS_9allocatorIyEEED2Ev: ## @_ZNSt3__16vectorIyNS_9allocatorIyEEED2Ev
	.cfi_startproc
## BB#0:
	pushq	%rbp
Ltmp63:
	.cfi_def_cfa_offset 16
Ltmp64:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
Ltmp65:
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rdi
	callq	__ZNSt3__113__vector_baseIyNS_9allocatorIyEEED2Ev
	addq	$16, %rsp
	popq	%rbp
	retq
	.cfi_endproc

	.globl	__ZNSt3__113__vector_baseIyNS_9allocatorIyEEED2Ev
	.weak_def_can_be_hidden	__ZNSt3__113__vector_baseIyNS_9allocatorIyEEED2Ev
	.align	4, 0x90
__ZNSt3__113__vector_baseIyNS_9allocatorIyEEED2Ev: ## @_ZNSt3__113__vector_baseIyNS_9allocatorIyEEED2Ev
	.cfi_startproc
## BB#0:
	pushq	%rbp
Ltmp66:
	.cfi_def_cfa_offset 16
Ltmp67:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
Ltmp68:
	.cfi_def_cfa_register %rbp
	subq	$272, %rsp              ## imm = 0x110
	movq	%rdi, -248(%rbp)
	movq	-248(%rbp), %rdi
	cmpq	$0, (%rdi)
	movq	%rdi, -256(%rbp)        ## 8-byte Spill
	je	LBB17_5
## BB#1:
	movq	-256(%rbp), %rax        ## 8-byte Reload
	movq	%rax, -240(%rbp)
	movq	-240(%rbp), %rcx
	movq	(%rcx), %rdx
	movq	%rcx, -224(%rbp)
	movq	%rdx, -232(%rbp)
	movq	-224(%rbp), %rcx
	movq	%rcx, -264(%rbp)        ## 8-byte Spill
LBB17_2:                                ## =>This Inner Loop Header: Depth=1
	movq	-232(%rbp), %rax
	movq	-264(%rbp), %rcx        ## 8-byte Reload
	cmpq	8(%rcx), %rax
	je	LBB17_4
## BB#3:                                ##   in Loop: Header=BB17_2 Depth=1
	movq	-264(%rbp), %rax        ## 8-byte Reload
	movq	%rax, -216(%rbp)
	movq	-216(%rbp), %rcx
	addq	$16, %rcx
	movq	%rcx, -208(%rbp)
	movq	-208(%rbp), %rcx
	movq	%rcx, -200(%rbp)
	movq	-200(%rbp), %rcx
	movq	8(%rax), %rdx
	addq	$-8, %rdx
	movq	%rdx, 8(%rax)
	movq	%rdx, -192(%rbp)
	movq	-192(%rbp), %rdx
	movq	%rcx, -160(%rbp)
	movq	%rdx, -168(%rbp)
	movq	-160(%rbp), %rcx
	movq	-168(%rbp), %rdx
	movq	%rcx, -144(%rbp)
	movq	%rdx, -152(%rbp)
	movq	-144(%rbp), %rcx
	movq	-152(%rbp), %rdx
	movq	%rcx, -120(%rbp)
	movq	%rdx, -128(%rbp)
	jmp	LBB17_2
LBB17_4:                                ## %_ZNSt3__113__vector_baseIyNS_9allocatorIyEEE5clearEv.exit
	movq	-256(%rbp), %rax        ## 8-byte Reload
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rcx
	addq	$16, %rcx
	movq	%rcx, -48(%rbp)
	movq	-48(%rbp), %rcx
	movq	%rcx, -40(%rbp)
	movq	-40(%rbp), %rcx
	movq	(%rax), %rdx
	movq	%rax, -32(%rbp)
	movq	-32(%rbp), %rsi
	movq	%rsi, -24(%rbp)
	movq	-24(%rbp), %rdi
	addq	$16, %rdi
	movq	%rdi, -16(%rbp)
	movq	-16(%rbp), %rdi
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rdi
	movq	(%rdi), %rdi
	movq	(%rsi), %rsi
	subq	%rsi, %rdi
	sarq	$3, %rdi
	movq	%rcx, -96(%rbp)
	movq	%rdx, -104(%rbp)
	movq	%rdi, -112(%rbp)
	movq	-96(%rbp), %rcx
	movq	-104(%rbp), %rdx
	movq	-112(%rbp), %rsi
	movq	%rcx, -72(%rbp)
	movq	%rdx, -80(%rbp)
	movq	%rsi, -88(%rbp)
	movq	-80(%rbp), %rcx
	movq	%rcx, -64(%rbp)
	movq	-64(%rbp), %rdi
	callq	__ZdlPv
LBB17_5:
	addq	$272, %rsp              ## imm = 0x110
	popq	%rbp
	retq
	.cfi_endproc

	.globl	__ZNSt3__16vectorIyNS_9allocatorIyEEE8__appendEm
	.weak_def_can_be_hidden	__ZNSt3__16vectorIyNS_9allocatorIyEEE8__appendEm
	.align	4, 0x90
__ZNSt3__16vectorIyNS_9allocatorIyEEE8__appendEm: ## @_ZNSt3__16vectorIyNS_9allocatorIyEEE8__appendEm
Lfunc_begin18:
	.cfi_startproc
	.cfi_personality 155, ___gxx_personality_v0
	.cfi_lsda 16, Lexception18
## BB#0:
	pushq	%rbp
Ltmp74:
	.cfi_def_cfa_offset 16
Ltmp75:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
Ltmp76:
	.cfi_def_cfa_register %rbp
	subq	$336, %rsp              ## imm = 0x150
	movq	%rdi, -232(%rbp)
	movq	%rsi, -240(%rbp)
	movq	-232(%rbp), %rsi
	movq	%rsi, %rdi
	movq	%rdi, -224(%rbp)
	movq	-224(%rbp), %rdi
	addq	$16, %rdi
	movq	%rdi, -216(%rbp)
	movq	-216(%rbp), %rdi
	movq	%rdi, -208(%rbp)
	movq	-208(%rbp), %rdi
	movq	(%rdi), %rdi
	movq	8(%rsi), %rax
	subq	%rax, %rdi
	sarq	$3, %rdi
	cmpq	-240(%rbp), %rdi
	movq	%rsi, -312(%rbp)        ## 8-byte Spill
	jb	LBB18_2
## BB#1:
	movq	-240(%rbp), %rsi
	movq	-312(%rbp), %rdi        ## 8-byte Reload
	callq	__ZNSt3__16vectorIyNS_9allocatorIyEEE18__construct_at_endEm
	jmp	LBB18_14
LBB18_2:
	movq	-312(%rbp), %rax        ## 8-byte Reload
	movq	%rax, -200(%rbp)
	movq	-200(%rbp), %rax
	addq	$16, %rax
	movq	%rax, -192(%rbp)
	movq	-192(%rbp), %rax
	movq	%rax, -184(%rbp)
	movq	-184(%rbp), %rax
	movq	%rax, -248(%rbp)
	movq	-312(%rbp), %rax        ## 8-byte Reload
	movq	%rax, -176(%rbp)
	movq	-176(%rbp), %rcx
	movq	8(%rcx), %rdx
	movq	(%rcx), %rcx
	subq	%rcx, %rdx
	sarq	$3, %rdx
	addq	-240(%rbp), %rdx
	movq	%rax, -136(%rbp)
	movq	%rdx, -144(%rbp)
	movq	-136(%rbp), %rcx
	movq	%rcx, %rdi
	movq	%rcx, -320(%rbp)        ## 8-byte Spill
	callq	__ZNKSt3__16vectorIyNS_9allocatorIyEEE8max_sizeEv
	movq	%rax, -152(%rbp)
	movq	-144(%rbp), %rax
	cmpq	-152(%rbp), %rax
	jbe	LBB18_4
## BB#3:
	movq	-320(%rbp), %rax        ## 8-byte Reload
	movq	%rax, %rdi
	callq	__ZNKSt3__120__vector_base_commonILb1EE20__throw_length_errorEv
LBB18_4:
	movq	-320(%rbp), %rax        ## 8-byte Reload
	movq	%rax, -120(%rbp)
	movq	-120(%rbp), %rcx
	movq	%rcx, -112(%rbp)
	movq	-112(%rbp), %rcx
	movq	%rcx, -104(%rbp)
	movq	-104(%rbp), %rdx
	addq	$16, %rdx
	movq	%rdx, -96(%rbp)
	movq	-96(%rbp), %rdx
	movq	%rdx, -88(%rbp)
	movq	-88(%rbp), %rdx
	movq	(%rdx), %rdx
	movq	(%rcx), %rcx
	subq	%rcx, %rdx
	sarq	$3, %rdx
	movq	%rdx, -160(%rbp)
	movq	-160(%rbp), %rcx
	movq	-152(%rbp), %rdx
	shrq	$1, %rdx
	cmpq	%rdx, %rcx
	jb	LBB18_6
## BB#5:
	movq	-152(%rbp), %rax
	movq	%rax, -128(%rbp)
	jmp	LBB18_10
LBB18_6:
	leaq	-40(%rbp), %rax
	leaq	-144(%rbp), %rcx
	leaq	-168(%rbp), %rdx
	movq	-160(%rbp), %rsi
	shlq	$1, %rsi
	movq	%rsi, -168(%rbp)
	movq	%rdx, -64(%rbp)
	movq	%rcx, -72(%rbp)
	movq	-64(%rbp), %rcx
	movq	-72(%rbp), %rdx
	movq	%rcx, -48(%rbp)
	movq	%rdx, -56(%rbp)
	movq	-48(%rbp), %rcx
	movq	-56(%rbp), %rdx
	movq	%rax, -16(%rbp)
	movq	%rcx, -24(%rbp)
	movq	%rdx, -32(%rbp)
	movq	-24(%rbp), %rax
	movq	(%rax), %rax
	movq	-32(%rbp), %rcx
	cmpq	(%rcx), %rax
	jae	LBB18_8
## BB#7:
	movq	-56(%rbp), %rax
	movq	%rax, -328(%rbp)        ## 8-byte Spill
	jmp	LBB18_9
LBB18_8:
	movq	-48(%rbp), %rax
	movq	%rax, -328(%rbp)        ## 8-byte Spill
LBB18_9:                                ## %_ZNSt3__13maxImEERKT_S3_S3_.exit.i
	movq	-328(%rbp), %rax        ## 8-byte Reload
	movq	(%rax), %rax
	movq	%rax, -128(%rbp)
LBB18_10:                               ## %_ZNKSt3__16vectorIyNS_9allocatorIyEEE11__recommendEm.exit
	movq	-128(%rbp), %rsi
	movq	-312(%rbp), %rax        ## 8-byte Reload
	movq	%rax, -8(%rbp)
	movq	(%rax), %rcx
	movq	8(%rax), %rdx
	subq	%rcx, %rdx
	sarq	$3, %rdx
	movq	-248(%rbp), %rcx
	leaq	-288(%rbp), %rdi
	movq	%rdi, -336(%rbp)        ## 8-byte Spill
	callq	__ZNSt3__114__split_bufferIyRNS_9allocatorIyEEEC1EmmS3_
	movq	-240(%rbp), %rsi
Ltmp69:
	movq	-336(%rbp), %rdi        ## 8-byte Reload
	callq	__ZNSt3__114__split_bufferIyRNS_9allocatorIyEEE18__construct_at_endEm
Ltmp70:
	jmp	LBB18_11
LBB18_11:
Ltmp71:
	leaq	-288(%rbp), %rsi
	movq	-312(%rbp), %rdi        ## 8-byte Reload
	callq	__ZNSt3__16vectorIyNS_9allocatorIyEEE26__swap_out_circular_bufferERNS_14__split_bufferIyRS2_EE
Ltmp72:
	jmp	LBB18_12
LBB18_12:
	leaq	-288(%rbp), %rdi
	callq	__ZNSt3__114__split_bufferIyRNS_9allocatorIyEEED1Ev
	jmp	LBB18_14
LBB18_13:
Ltmp73:
	leaq	-288(%rbp), %rdi
	movl	%edx, %ecx
	movq	%rax, -296(%rbp)
	movl	%ecx, -300(%rbp)
	callq	__ZNSt3__114__split_bufferIyRNS_9allocatorIyEEED1Ev
	jmp	LBB18_15
LBB18_14:
	addq	$336, %rsp              ## imm = 0x150
	popq	%rbp
	retq
LBB18_15:
	movq	-296(%rbp), %rdi
	callq	__Unwind_Resume
Lfunc_end18:
	.cfi_endproc
	.section	__TEXT,__gcc_except_tab
	.align	2
GCC_except_table18:
Lexception18:
	.byte	255                     ## @LPStart Encoding = omit
	.byte	155                     ## @TType Encoding = indirect pcrel sdata4
	.byte	41                      ## @TType base offset
	.byte	3                       ## Call site Encoding = udata4
	.byte	39                      ## Call site table length
Lset19 = Lfunc_begin18-Lfunc_begin18    ## >> Call Site 1 <<
	.long	Lset19
Lset20 = Ltmp69-Lfunc_begin18           ##   Call between Lfunc_begin18 and Ltmp69
	.long	Lset20
	.long	0                       ##     has no landing pad
	.byte	0                       ##   On action: cleanup
Lset21 = Ltmp69-Lfunc_begin18           ## >> Call Site 2 <<
	.long	Lset21
Lset22 = Ltmp72-Ltmp69                  ##   Call between Ltmp69 and Ltmp72
	.long	Lset22
Lset23 = Ltmp73-Lfunc_begin18           ##     jumps to Ltmp73
	.long	Lset23
	.byte	0                       ##   On action: cleanup
Lset24 = Ltmp72-Lfunc_begin18           ## >> Call Site 3 <<
	.long	Lset24
Lset25 = Lfunc_end18-Ltmp72             ##   Call between Ltmp72 and Lfunc_end18
	.long	Lset25
	.long	0                       ##     has no landing pad
	.byte	0                       ##   On action: cleanup
	.align	2

	.section	__TEXT,__textcoal_nt,coalesced,pure_instructions
	.globl	__ZNSt3__16vectorIyNS_9allocatorIyEEE18__construct_at_endEm
	.weak_def_can_be_hidden	__ZNSt3__16vectorIyNS_9allocatorIyEEE18__construct_at_endEm
	.align	4, 0x90
__ZNSt3__16vectorIyNS_9allocatorIyEEE18__construct_at_endEm: ## @_ZNSt3__16vectorIyNS_9allocatorIyEEE18__construct_at_endEm
	.cfi_startproc
## BB#0:
	pushq	%rbp
Ltmp77:
	.cfi_def_cfa_offset 16
Ltmp78:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
Ltmp79:
	.cfi_def_cfa_register %rbp
	subq	$144, %rsp
	movq	%rdi, -112(%rbp)
	movq	%rsi, -120(%rbp)
	movq	-112(%rbp), %rsi
	movq	%rsi, %rdi
	movq	%rdi, -104(%rbp)
	movq	-104(%rbp), %rdi
	addq	$16, %rdi
	movq	%rdi, -96(%rbp)
	movq	-96(%rbp), %rdi
	movq	%rdi, -88(%rbp)
	movq	-88(%rbp), %rdi
	movq	%rdi, -128(%rbp)
	movq	%rsi, -144(%rbp)        ## 8-byte Spill
LBB19_1:                                ## =>This Inner Loop Header: Depth=1
	leaq	-136(%rbp), %rdi
	movl	$1, %eax
	movl	%eax, %edx
	movq	-144(%rbp), %rsi        ## 8-byte Reload
	callq	__ZNSt3__16vectorIyNS_9allocatorIyEEE24__RAII_IncreaseAnnotatorC1ERKS3_m
	leaq	-136(%rbp), %rdi
	movq	-128(%rbp), %rdx
	movq	-144(%rbp), %rsi        ## 8-byte Reload
	movq	8(%rsi), %rcx
	movq	%rcx, -80(%rbp)
	movq	-80(%rbp), %rcx
	movq	%rdx, -48(%rbp)
	movq	%rcx, -56(%rbp)
	movq	-48(%rbp), %rcx
	movq	-56(%rbp), %rdx
	movq	%rcx, -32(%rbp)
	movq	%rdx, -40(%rbp)
	movq	-32(%rbp), %rcx
	movq	-40(%rbp), %rdx
	movq	%rcx, -8(%rbp)
	movq	%rdx, -16(%rbp)
	movq	-16(%rbp), %rcx
	movq	$0, (%rcx)
	movq	8(%rsi), %rcx
	addq	$8, %rcx
	movq	%rcx, 8(%rsi)
	movq	-120(%rbp), %rcx
	addq	$-1, %rcx
	movq	%rcx, -120(%rbp)
	callq	__ZNSt3__16vectorIyNS_9allocatorIyEEE24__RAII_IncreaseAnnotator6__doneEv
## BB#2:                                ##   in Loop: Header=BB19_1 Depth=1
	cmpq	$0, -120(%rbp)
	ja	LBB19_1
## BB#3:
	addq	$144, %rsp
	popq	%rbp
	retq
	.cfi_endproc

	.globl	__ZNSt3__114__split_bufferIyRNS_9allocatorIyEEEC1EmmS3_
	.weak_def_can_be_hidden	__ZNSt3__114__split_bufferIyRNS_9allocatorIyEEEC1EmmS3_
	.align	4, 0x90
__ZNSt3__114__split_bufferIyRNS_9allocatorIyEEEC1EmmS3_: ## @_ZNSt3__114__split_bufferIyRNS_9allocatorIyEEEC1EmmS3_
	.cfi_startproc
## BB#0:
	pushq	%rbp
Ltmp80:
	.cfi_def_cfa_offset 16
Ltmp81:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
Ltmp82:
	.cfi_def_cfa_register %rbp
	subq	$32, %rsp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	%rdx, -24(%rbp)
	movq	%rcx, -32(%rbp)
	movq	-8(%rbp), %rdi
	movq	-16(%rbp), %rsi
	movq	-24(%rbp), %rdx
	movq	-32(%rbp), %rcx
	callq	__ZNSt3__114__split_bufferIyRNS_9allocatorIyEEEC2EmmS3_
	addq	$32, %rsp
	popq	%rbp
	retq
	.cfi_endproc

	.globl	__ZNSt3__114__split_bufferIyRNS_9allocatorIyEEE18__construct_at_endEm
	.weak_def_can_be_hidden	__ZNSt3__114__split_bufferIyRNS_9allocatorIyEEE18__construct_at_endEm
	.align	4, 0x90
__ZNSt3__114__split_bufferIyRNS_9allocatorIyEEE18__construct_at_endEm: ## @_ZNSt3__114__split_bufferIyRNS_9allocatorIyEEE18__construct_at_endEm
	.cfi_startproc
## BB#0:
	pushq	%rbp
Ltmp83:
	.cfi_def_cfa_offset 16
Ltmp84:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
Ltmp85:
	.cfi_def_cfa_register %rbp
	pushq	%rax
	movq	%rdi, -112(%rbp)
	movq	%rsi, -120(%rbp)
	movq	-112(%rbp), %rsi
	movq	%rsi, -104(%rbp)
	movq	-104(%rbp), %rdi
	addq	$24, %rdi
	movq	%rdi, -96(%rbp)
	movq	-96(%rbp), %rdi
	movq	%rdi, -88(%rbp)
	movq	-88(%rbp), %rdi
	movq	8(%rdi), %rdi
	movq	%rdi, -128(%rbp)
	movq	%rsi, -136(%rbp)        ## 8-byte Spill
LBB21_1:                                ## =>This Inner Loop Header: Depth=1
	movq	-128(%rbp), %rax
	movq	-136(%rbp), %rcx        ## 8-byte Reload
	movq	16(%rcx), %rdx
	movq	%rdx, -8(%rbp)
	movq	-8(%rbp), %rdx
	movq	%rax, -56(%rbp)
	movq	%rdx, -64(%rbp)
	movq	-56(%rbp), %rax
	movq	-64(%rbp), %rdx
	movq	%rax, -40(%rbp)
	movq	%rdx, -48(%rbp)
	movq	-40(%rbp), %rax
	movq	-48(%rbp), %rdx
	movq	%rax, -16(%rbp)
	movq	%rdx, -24(%rbp)
	movq	-24(%rbp), %rax
	movq	$0, (%rax)
	movq	16(%rcx), %rax
	addq	$8, %rax
	movq	%rax, 16(%rcx)
	movq	-120(%rbp), %rax
	addq	$-1, %rax
	movq	%rax, -120(%rbp)
## BB#2:                                ##   in Loop: Header=BB21_1 Depth=1
	cmpq	$0, -120(%rbp)
	ja	LBB21_1
## BB#3:
	addq	$8, %rsp
	popq	%rbp
	retq
	.cfi_endproc

	.globl	__ZNSt3__16vectorIyNS_9allocatorIyEEE26__swap_out_circular_bufferERNS_14__split_bufferIyRS2_EE
	.weak_def_can_be_hidden	__ZNSt3__16vectorIyNS_9allocatorIyEEE26__swap_out_circular_bufferERNS_14__split_bufferIyRS2_EE
	.align	4, 0x90
__ZNSt3__16vectorIyNS_9allocatorIyEEE26__swap_out_circular_bufferERNS_14__split_bufferIyRS2_EE: ## @_ZNSt3__16vectorIyNS_9allocatorIyEEE26__swap_out_circular_bufferERNS_14__split_bufferIyRS2_EE
	.cfi_startproc
## BB#0:
	pushq	%rbp
Ltmp86:
	.cfi_def_cfa_offset 16
Ltmp87:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
Ltmp88:
	.cfi_def_cfa_register %rbp
	pushq	%rbx
	subq	$328, %rsp              ## imm = 0x148
Ltmp89:
	.cfi_offset %rbx, -24
	movq	%rdi, -288(%rbp)
	movq	%rsi, -296(%rbp)
	movq	-288(%rbp), %rsi
	movq	%rsi, %rdi
	movq	%rsi, -304(%rbp)        ## 8-byte Spill
	callq	__ZNKSt3__16vectorIyNS_9allocatorIyEEE17__annotate_deleteEv
	leaq	-200(%rbp), %rsi
	leaq	-104(%rbp), %rdi
	leaq	-56(%rbp), %rax
	xorl	%ecx, %ecx
	movl	%ecx, %edx
	movq	-304(%rbp), %r8         ## 8-byte Reload
	movq	%r8, -280(%rbp)
	movq	-280(%rbp), %r8
	addq	$16, %r8
	movq	%r8, -272(%rbp)
	movq	-272(%rbp), %r8
	movq	%r8, -264(%rbp)
	movq	-264(%rbp), %r8
	movq	-304(%rbp), %r9         ## 8-byte Reload
	movq	(%r9), %r10
	movq	8(%r9), %r11
	movq	-296(%rbp), %rbx
	addq	$8, %rbx
	movq	%r8, -216(%rbp)
	movq	%r10, -224(%rbp)
	movq	%r11, -232(%rbp)
	movq	%rbx, -240(%rbp)
	movq	-232(%rbp), %r8
	movq	-224(%rbp), %r10
	subq	%r10, %r8
	sarq	$3, %r8
	movq	%r8, -248(%rbp)
	movq	-248(%rbp), %r8
	movq	-240(%rbp), %r10
	movq	(%r10), %r11
	subq	%r8, %rdx
	shlq	$3, %rdx
	addq	%rdx, %r11
	movq	%r11, (%r10)
	movq	-240(%rbp), %rdx
	movq	(%rdx), %rdx
	movq	-224(%rbp), %r8
	movq	-248(%rbp), %r10
	shlq	$3, %r10
	movq	%rdi, -312(%rbp)        ## 8-byte Spill
	movq	%rdx, %rdi
	movq	%rsi, -320(%rbp)        ## 8-byte Spill
	movq	%r8, %rsi
	movq	%r10, %rdx
	movq	%rax, -328(%rbp)        ## 8-byte Spill
	callq	_memcpy
	movq	-304(%rbp), %rax        ## 8-byte Reload
	movq	-296(%rbp), %rdx
	addq	$8, %rdx
	movq	%rax, -40(%rbp)
	movq	%rdx, -48(%rbp)
	movq	-40(%rbp), %rax
	movq	%rax, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	(%rax), %rax
	movq	%rax, -56(%rbp)
	movq	-48(%rbp), %rax
	movq	%rax, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	(%rax), %rax
	movq	-40(%rbp), %rdx
	movq	%rax, (%rdx)
	movq	-328(%rbp), %rax        ## 8-byte Reload
	movq	%rax, -24(%rbp)
	movq	-24(%rbp), %rdx
	movq	(%rdx), %rdx
	movq	-48(%rbp), %rsi
	movq	%rdx, (%rsi)
	movq	-304(%rbp), %rdx        ## 8-byte Reload
	addq	$8, %rdx
	movq	-296(%rbp), %rsi
	addq	$16, %rsi
	movq	%rdx, -88(%rbp)
	movq	%rsi, -96(%rbp)
	movq	-88(%rbp), %rdx
	movq	%rdx, -80(%rbp)
	movq	-80(%rbp), %rdx
	movq	(%rdx), %rdx
	movq	%rdx, -104(%rbp)
	movq	-96(%rbp), %rdx
	movq	%rdx, -64(%rbp)
	movq	-64(%rbp), %rdx
	movq	(%rdx), %rdx
	movq	-88(%rbp), %rsi
	movq	%rdx, (%rsi)
	movq	-312(%rbp), %rdx        ## 8-byte Reload
	movq	%rdx, -72(%rbp)
	movq	-72(%rbp), %rsi
	movq	(%rsi), %rsi
	movq	-96(%rbp), %rdi
	movq	%rsi, (%rdi)
	movq	-304(%rbp), %rsi        ## 8-byte Reload
	movq	%rsi, -128(%rbp)
	movq	-128(%rbp), %rsi
	addq	$16, %rsi
	movq	%rsi, -120(%rbp)
	movq	-120(%rbp), %rsi
	movq	%rsi, -112(%rbp)
	movq	-112(%rbp), %rsi
	movq	-296(%rbp), %rdi
	movq	%rdi, -152(%rbp)
	movq	-152(%rbp), %rdi
	addq	$24, %rdi
	movq	%rdi, -144(%rbp)
	movq	-144(%rbp), %rdi
	movq	%rdi, -136(%rbp)
	movq	-136(%rbp), %rdi
	movq	%rsi, -184(%rbp)
	movq	%rdi, -192(%rbp)
	movq	-184(%rbp), %rsi
	movq	%rsi, -176(%rbp)
	movq	-176(%rbp), %rsi
	movq	(%rsi), %rsi
	movq	%rsi, -200(%rbp)
	movq	-192(%rbp), %rsi
	movq	%rsi, -160(%rbp)
	movq	-160(%rbp), %rsi
	movq	(%rsi), %rsi
	movq	-184(%rbp), %rdi
	movq	%rsi, (%rdi)
	movq	-320(%rbp), %rsi        ## 8-byte Reload
	movq	%rsi, -168(%rbp)
	movq	-168(%rbp), %rdi
	movq	(%rdi), %rdi
	movq	-192(%rbp), %r8
	movq	%rdi, (%r8)
	movq	-296(%rbp), %rdi
	movq	8(%rdi), %rdi
	movq	-296(%rbp), %r8
	movq	%rdi, (%r8)
	movq	-304(%rbp), %rdi        ## 8-byte Reload
	movq	%rdi, -208(%rbp)
	movq	-208(%rbp), %r8
	movq	8(%r8), %r9
	movq	(%r8), %r8
	subq	%r8, %r9
	sarq	$3, %r9
	movq	%r9, %rsi
	callq	__ZNKSt3__16vectorIyNS_9allocatorIyEEE14__annotate_newEm
	movq	-304(%rbp), %rax        ## 8-byte Reload
	movq	%rax, -256(%rbp)
	addq	$328, %rsp              ## imm = 0x148
	popq	%rbx
	popq	%rbp
	retq
	.cfi_endproc

	.globl	__ZNSt3__114__split_bufferIyRNS_9allocatorIyEEED1Ev
	.weak_def_can_be_hidden	__ZNSt3__114__split_bufferIyRNS_9allocatorIyEEED1Ev
	.align	4, 0x90
__ZNSt3__114__split_bufferIyRNS_9allocatorIyEEED1Ev: ## @_ZNSt3__114__split_bufferIyRNS_9allocatorIyEEED1Ev
	.cfi_startproc
## BB#0:
	pushq	%rbp
Ltmp90:
	.cfi_def_cfa_offset 16
Ltmp91:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
Ltmp92:
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rdi
	callq	__ZNSt3__114__split_bufferIyRNS_9allocatorIyEEED2Ev
	addq	$16, %rsp
	popq	%rbp
	retq
	.cfi_endproc

	.private_extern	__ZNSt3__16vectorIyNS_9allocatorIyEEE24__RAII_IncreaseAnnotatorC1ERKS3_m
	.globl	__ZNSt3__16vectorIyNS_9allocatorIyEEE24__RAII_IncreaseAnnotatorC1ERKS3_m
	.weak_def_can_be_hidden	__ZNSt3__16vectorIyNS_9allocatorIyEEE24__RAII_IncreaseAnnotatorC1ERKS3_m
	.align	4, 0x90
__ZNSt3__16vectorIyNS_9allocatorIyEEE24__RAII_IncreaseAnnotatorC1ERKS3_m: ## @_ZNSt3__16vectorIyNS_9allocatorIyEEE24__RAII_IncreaseAnnotatorC1ERKS3_m
	.cfi_startproc
## BB#0:
	pushq	%rbp
Ltmp93:
	.cfi_def_cfa_offset 16
Ltmp94:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
Ltmp95:
	.cfi_def_cfa_register %rbp
	subq	$32, %rsp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	%rdx, -24(%rbp)
	movq	-8(%rbp), %rdi
	movq	-24(%rbp), %rdx
	movq	-16(%rbp), %rsi
	callq	__ZNSt3__16vectorIyNS_9allocatorIyEEE24__RAII_IncreaseAnnotatorC2ERKS3_m
	addq	$32, %rsp
	popq	%rbp
	retq
	.cfi_endproc

	.private_extern	__ZNSt3__16vectorIyNS_9allocatorIyEEE24__RAII_IncreaseAnnotator6__doneEv
	.globl	__ZNSt3__16vectorIyNS_9allocatorIyEEE24__RAII_IncreaseAnnotator6__doneEv
	.weak_def_can_be_hidden	__ZNSt3__16vectorIyNS_9allocatorIyEEE24__RAII_IncreaseAnnotator6__doneEv
	.align	4, 0x90
__ZNSt3__16vectorIyNS_9allocatorIyEEE24__RAII_IncreaseAnnotator6__doneEv: ## @_ZNSt3__16vectorIyNS_9allocatorIyEEE24__RAII_IncreaseAnnotator6__doneEv
	.cfi_startproc
## BB#0:
	pushq	%rbp
Ltmp96:
	.cfi_def_cfa_offset 16
Ltmp97:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
Ltmp98:
	.cfi_def_cfa_register %rbp
	movq	%rdi, -8(%rbp)
	popq	%rbp
	retq
	.cfi_endproc

	.private_extern	__ZNSt3__16vectorIyNS_9allocatorIyEEE24__RAII_IncreaseAnnotatorC2ERKS3_m
	.globl	__ZNSt3__16vectorIyNS_9allocatorIyEEE24__RAII_IncreaseAnnotatorC2ERKS3_m
	.weak_def_can_be_hidden	__ZNSt3__16vectorIyNS_9allocatorIyEEE24__RAII_IncreaseAnnotatorC2ERKS3_m
	.align	4, 0x90
__ZNSt3__16vectorIyNS_9allocatorIyEEE24__RAII_IncreaseAnnotatorC2ERKS3_m: ## @_ZNSt3__16vectorIyNS_9allocatorIyEEE24__RAII_IncreaseAnnotatorC2ERKS3_m
	.cfi_startproc
## BB#0:
	pushq	%rbp
Ltmp99:
	.cfi_def_cfa_offset 16
Ltmp100:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
Ltmp101:
	.cfi_def_cfa_register %rbp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	%rdx, -24(%rbp)
	popq	%rbp
	retq
	.cfi_endproc

	.globl	__ZNKSt3__16vectorIyNS_9allocatorIyEEE8max_sizeEv
	.weak_def_can_be_hidden	__ZNKSt3__16vectorIyNS_9allocatorIyEEE8max_sizeEv
	.align	4, 0x90
__ZNKSt3__16vectorIyNS_9allocatorIyEEE8max_sizeEv: ## @_ZNKSt3__16vectorIyNS_9allocatorIyEEE8max_sizeEv
	.cfi_startproc
## BB#0:
	pushq	%rbp
Ltmp102:
	.cfi_def_cfa_offset 16
Ltmp103:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
Ltmp104:
	.cfi_def_cfa_register %rbp
	subq	$56, %rsp
	leaq	-32(%rbp), %rax
	leaq	-168(%rbp), %rcx
	leaq	-160(%rbp), %rdx
	movq	$-1, %rsi
	movabsq	$2305843009213693951, %r8 ## imm = 0x1FFFFFFFFFFFFFFF
	movq	%rdi, -152(%rbp)
	movq	-152(%rbp), %rdi
	movq	%rdi, -144(%rbp)
	movq	-144(%rbp), %rdi
	addq	$16, %rdi
	movq	%rdi, -136(%rbp)
	movq	-136(%rbp), %rdi
	movq	%rdi, -128(%rbp)
	movq	-128(%rbp), %rdi
	movq	%rdi, -104(%rbp)
	movq	-104(%rbp), %rdi
	movq	%rdi, -96(%rbp)
	movq	-96(%rbp), %rdi
	movq	%rdi, -80(%rbp)
	movq	%r8, -160(%rbp)
	shrq	$1, %rsi
	movq	%rsi, -168(%rbp)
	movq	%rdx, -56(%rbp)
	movq	%rcx, -64(%rbp)
	movq	-56(%rbp), %rcx
	movq	-64(%rbp), %rdx
	movq	%rcx, -40(%rbp)
	movq	%rdx, -48(%rbp)
	movq	-48(%rbp), %rcx
	movq	-40(%rbp), %rdx
	movq	%rax, -8(%rbp)
	movq	%rcx, -16(%rbp)
	movq	%rdx, -24(%rbp)
	movq	-16(%rbp), %rax
	movq	(%rax), %rax
	movq	-24(%rbp), %rcx
	cmpq	(%rcx), %rax
	jae	LBB27_2
## BB#1:
	movq	-48(%rbp), %rax
	movq	%rax, -176(%rbp)        ## 8-byte Spill
	jmp	LBB27_3
LBB27_2:
	movq	-40(%rbp), %rax
	movq	%rax, -176(%rbp)        ## 8-byte Spill
LBB27_3:                                ## %_ZNSt3__13minImEERKT_S3_S3_.exit
	movq	-176(%rbp), %rax        ## 8-byte Reload
	movq	%rax, -184(%rbp)        ## 8-byte Spill
## BB#4:
	movq	-184(%rbp), %rax        ## 8-byte Reload
	movq	(%rax), %rax
	addq	$56, %rsp
	popq	%rbp
	retq
	.cfi_endproc

	.globl	__ZNSt3__114__split_bufferIyRNS_9allocatorIyEEEC2EmmS3_
	.weak_def_can_be_hidden	__ZNSt3__114__split_bufferIyRNS_9allocatorIyEEEC2EmmS3_
	.align	4, 0x90
__ZNSt3__114__split_bufferIyRNS_9allocatorIyEEEC2EmmS3_: ## @_ZNSt3__114__split_bufferIyRNS_9allocatorIyEEEC2EmmS3_
	.cfi_startproc
## BB#0:
	pushq	%rbp
Ltmp105:
	.cfi_def_cfa_offset 16
Ltmp106:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
Ltmp107:
	.cfi_def_cfa_register %rbp
	subq	$256, %rsp              ## imm = 0x100
	leaq	-136(%rbp), %rax
	leaq	-168(%rbp), %r8
	movq	%rdi, -208(%rbp)
	movq	%rsi, -216(%rbp)
	movq	%rdx, -224(%rbp)
	movq	%rcx, -232(%rbp)
	movq	-208(%rbp), %rcx
	movq	%rcx, %rdx
	addq	$24, %rdx
	movq	-232(%rbp), %rsi
	movq	%rdx, -184(%rbp)
	movq	$0, -192(%rbp)
	movq	%rsi, -200(%rbp)
	movq	-184(%rbp), %rdx
	movq	-192(%rbp), %rsi
	movq	-200(%rbp), %rdi
	movq	%rdx, -160(%rbp)
	movq	%rsi, -168(%rbp)
	movq	%rdi, -176(%rbp)
	movq	-160(%rbp), %rdx
	movq	%r8, -152(%rbp)
	movq	-152(%rbp), %rsi
	movq	(%rsi), %rsi
	movq	-176(%rbp), %rdi
	movq	%rdi, -104(%rbp)
	movq	-104(%rbp), %rdi
	movq	%rdx, -128(%rbp)
	movq	%rsi, -136(%rbp)
	movq	%rdi, -144(%rbp)
	movq	-128(%rbp), %rdx
	movq	%rax, -120(%rbp)
	movq	-120(%rbp), %rax
	movq	(%rax), %rax
	movq	%rax, (%rdx)
	movq	-144(%rbp), %rax
	movq	%rax, -112(%rbp)
	movq	-112(%rbp), %rax
	movq	%rax, 8(%rdx)
	cmpq	$0, -216(%rbp)
	movq	%rcx, -240(%rbp)        ## 8-byte Spill
	je	LBB28_2
## BB#1:
	movq	-240(%rbp), %rax        ## 8-byte Reload
	movq	%rax, -24(%rbp)
	movq	-24(%rbp), %rcx
	addq	$24, %rcx
	movq	%rcx, -16(%rbp)
	movq	-16(%rbp), %rcx
	movq	%rcx, -8(%rbp)
	movq	-8(%rbp), %rcx
	movq	8(%rcx), %rcx
	movq	-216(%rbp), %rdx
	movq	%rcx, -64(%rbp)
	movq	%rdx, -72(%rbp)
	movq	-64(%rbp), %rcx
	movq	-72(%rbp), %rdx
	movq	%rcx, -40(%rbp)
	movq	%rdx, -48(%rbp)
	movq	$0, -56(%rbp)
	movq	-48(%rbp), %rcx
	shlq	$3, %rcx
	movq	%rcx, -32(%rbp)
	movq	-32(%rbp), %rdi
	callq	__Znwm
	movq	%rax, -248(%rbp)        ## 8-byte Spill
	jmp	LBB28_3
LBB28_2:
	xorl	%eax, %eax
	movl	%eax, %ecx
	movq	%rcx, -248(%rbp)        ## 8-byte Spill
	jmp	LBB28_3
LBB28_3:
	movq	-248(%rbp), %rax        ## 8-byte Reload
	movq	-240(%rbp), %rcx        ## 8-byte Reload
	movq	%rax, (%rcx)
	movq	(%rcx), %rax
	movq	-224(%rbp), %rdx
	shlq	$3, %rdx
	addq	%rdx, %rax
	movq	%rax, 16(%rcx)
	movq	%rax, 8(%rcx)
	movq	(%rcx), %rax
	movq	-216(%rbp), %rdx
	shlq	$3, %rdx
	addq	%rdx, %rax
	movq	%rcx, -96(%rbp)
	movq	-96(%rbp), %rdx
	addq	$24, %rdx
	movq	%rdx, -88(%rbp)
	movq	-88(%rbp), %rdx
	movq	%rdx, -80(%rbp)
	movq	-80(%rbp), %rdx
	movq	%rax, (%rdx)
	addq	$256, %rsp              ## imm = 0x100
	popq	%rbp
	retq
	.cfi_endproc

	.private_extern	__ZNKSt3__16vectorIyNS_9allocatorIyEEE17__annotate_deleteEv
	.globl	__ZNKSt3__16vectorIyNS_9allocatorIyEEE17__annotate_deleteEv
	.weak_def_can_be_hidden	__ZNKSt3__16vectorIyNS_9allocatorIyEEE17__annotate_deleteEv
	.align	4, 0x90
__ZNKSt3__16vectorIyNS_9allocatorIyEEE17__annotate_deleteEv: ## @_ZNKSt3__16vectorIyNS_9allocatorIyEEE17__annotate_deleteEv
	.cfi_startproc
## BB#0:
	pushq	%rbp
Ltmp108:
	.cfi_def_cfa_offset 16
Ltmp109:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
Ltmp110:
	.cfi_def_cfa_register %rbp
	subq	$176, %rsp
	movq	%rdi, -160(%rbp)
	movq	-160(%rbp), %rdi
	movq	%rdi, -152(%rbp)
	movq	-152(%rbp), %rax
	movq	(%rax), %rax
	movq	%rax, -144(%rbp)
	movq	-144(%rbp), %rax
	movq	%rdi, -136(%rbp)
	movq	-136(%rbp), %rcx
	movq	(%rcx), %rcx
	movq	%rcx, -128(%rbp)
	movq	-128(%rbp), %rcx
	movq	%rdi, -40(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rdx, -24(%rbp)
	movq	-24(%rbp), %rsi
	addq	$16, %rsi
	movq	%rsi, -16(%rbp)
	movq	-16(%rbp), %rsi
	movq	%rsi, -8(%rbp)
	movq	-8(%rbp), %rsi
	movq	(%rsi), %rsi
	movq	(%rdx), %rdx
	subq	%rdx, %rsi
	sarq	$3, %rsi
	shlq	$3, %rsi
	addq	%rsi, %rcx
	movq	%rdi, -56(%rbp)
	movq	-56(%rbp), %rdx
	movq	(%rdx), %rdx
	movq	%rdx, -48(%rbp)
	movq	-48(%rbp), %rdx
	movq	%rdi, -64(%rbp)
	movq	-64(%rbp), %rsi
	movq	8(%rsi), %r8
	movq	(%rsi), %rsi
	subq	%rsi, %r8
	sarq	$3, %r8
	shlq	$3, %r8
	addq	%r8, %rdx
	movq	%rdi, -80(%rbp)
	movq	-80(%rbp), %rsi
	movq	(%rsi), %rsi
	movq	%rsi, -72(%rbp)
	movq	-72(%rbp), %rsi
	movq	%rdi, -120(%rbp)
	movq	-120(%rbp), %r8
	movq	%r8, -112(%rbp)
	movq	-112(%rbp), %r8
	movq	%r8, -104(%rbp)
	movq	-104(%rbp), %r9
	addq	$16, %r9
	movq	%r9, -96(%rbp)
	movq	-96(%rbp), %r9
	movq	%r9, -88(%rbp)
	movq	-88(%rbp), %r9
	movq	(%r9), %r9
	movq	(%r8), %r8
	subq	%r8, %r9
	sarq	$3, %r9
	shlq	$3, %r9
	addq	%r9, %rsi
	movq	%rsi, -168(%rbp)        ## 8-byte Spill
	movq	%rax, %rsi
	movq	%rdx, -176(%rbp)        ## 8-byte Spill
	movq	%rcx, %rdx
	movq	-176(%rbp), %rcx        ## 8-byte Reload
	movq	-168(%rbp), %r8         ## 8-byte Reload
	callq	__ZNKSt3__16vectorIyNS_9allocatorIyEEE31__annotate_contiguous_containerEPKvS5_S5_S5_
	addq	$176, %rsp
	popq	%rbp
	retq
	.cfi_endproc

	.private_extern	__ZNKSt3__16vectorIyNS_9allocatorIyEEE14__annotate_newEm
	.globl	__ZNKSt3__16vectorIyNS_9allocatorIyEEE14__annotate_newEm
	.weak_def_can_be_hidden	__ZNKSt3__16vectorIyNS_9allocatorIyEEE14__annotate_newEm
	.align	4, 0x90
__ZNKSt3__16vectorIyNS_9allocatorIyEEE14__annotate_newEm: ## @_ZNKSt3__16vectorIyNS_9allocatorIyEEE14__annotate_newEm
	.cfi_startproc
## BB#0:
	pushq	%rbp
Ltmp111:
	.cfi_def_cfa_offset 16
Ltmp112:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
Ltmp113:
	.cfi_def_cfa_register %rbp
	subq	$176, %rsp
	movq	%rdi, -152(%rbp)
	movq	%rsi, -160(%rbp)
	movq	-152(%rbp), %rsi
	movq	%rsi, -144(%rbp)
	movq	-144(%rbp), %rdi
	movq	(%rdi), %rdi
	movq	%rdi, -136(%rbp)
	movq	-136(%rbp), %rdi
	movq	%rsi, -128(%rbp)
	movq	-128(%rbp), %rax
	movq	(%rax), %rax
	movq	%rax, -120(%rbp)
	movq	-120(%rbp), %rax
	movq	%rsi, -40(%rbp)
	movq	-40(%rbp), %rcx
	movq	%rcx, -32(%rbp)
	movq	-32(%rbp), %rcx
	movq	%rcx, -24(%rbp)
	movq	-24(%rbp), %rdx
	addq	$16, %rdx
	movq	%rdx, -16(%rbp)
	movq	-16(%rbp), %rdx
	movq	%rdx, -8(%rbp)
	movq	-8(%rbp), %rdx
	movq	(%rdx), %rdx
	movq	(%rcx), %rcx
	subq	%rcx, %rdx
	sarq	$3, %rdx
	shlq	$3, %rdx
	addq	%rdx, %rax
	movq	%rsi, -56(%rbp)
	movq	-56(%rbp), %rcx
	movq	(%rcx), %rcx
	movq	%rcx, -48(%rbp)
	movq	-48(%rbp), %rcx
	movq	%rsi, -96(%rbp)
	movq	-96(%rbp), %rdx
	movq	%rdx, -88(%rbp)
	movq	-88(%rbp), %rdx
	movq	%rdx, -80(%rbp)
	movq	-80(%rbp), %r8
	addq	$16, %r8
	movq	%r8, -72(%rbp)
	movq	-72(%rbp), %r8
	movq	%r8, -64(%rbp)
	movq	-64(%rbp), %r8
	movq	(%r8), %r8
	movq	(%rdx), %rdx
	subq	%rdx, %r8
	sarq	$3, %r8
	shlq	$3, %r8
	addq	%r8, %rcx
	movq	%rsi, -112(%rbp)
	movq	-112(%rbp), %rdx
	movq	(%rdx), %rdx
	movq	%rdx, -104(%rbp)
	movq	-104(%rbp), %rdx
	movq	-160(%rbp), %r8
	shlq	$3, %r8
	addq	%r8, %rdx
	movq	%rdi, -168(%rbp)        ## 8-byte Spill
	movq	%rsi, %rdi
	movq	-168(%rbp), %rsi        ## 8-byte Reload
	movq	%rdx, -176(%rbp)        ## 8-byte Spill
	movq	%rax, %rdx
	movq	-176(%rbp), %r8         ## 8-byte Reload
	callq	__ZNKSt3__16vectorIyNS_9allocatorIyEEE31__annotate_contiguous_containerEPKvS5_S5_S5_
	addq	$176, %rsp
	popq	%rbp
	retq
	.cfi_endproc

	.private_extern	__ZNKSt3__16vectorIyNS_9allocatorIyEEE31__annotate_contiguous_containerEPKvS5_S5_S5_
	.globl	__ZNKSt3__16vectorIyNS_9allocatorIyEEE31__annotate_contiguous_containerEPKvS5_S5_S5_
	.weak_def_can_be_hidden	__ZNKSt3__16vectorIyNS_9allocatorIyEEE31__annotate_contiguous_containerEPKvS5_S5_S5_
	.align	4, 0x90
__ZNKSt3__16vectorIyNS_9allocatorIyEEE31__annotate_contiguous_containerEPKvS5_S5_S5_: ## @_ZNKSt3__16vectorIyNS_9allocatorIyEEE31__annotate_contiguous_containerEPKvS5_S5_S5_
	.cfi_startproc
## BB#0:
	pushq	%rbp
Ltmp114:
	.cfi_def_cfa_offset 16
Ltmp115:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
Ltmp116:
	.cfi_def_cfa_register %rbp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movq	%rdx, -24(%rbp)
	movq	%rcx, -32(%rbp)
	movq	%r8, -40(%rbp)
	popq	%rbp
	retq
	.cfi_endproc

	.globl	__ZNSt3__114__split_bufferIyRNS_9allocatorIyEEED2Ev
	.weak_def_can_be_hidden	__ZNSt3__114__split_bufferIyRNS_9allocatorIyEEED2Ev
	.align	4, 0x90
__ZNSt3__114__split_bufferIyRNS_9allocatorIyEEED2Ev: ## @_ZNSt3__114__split_bufferIyRNS_9allocatorIyEEED2Ev
	.cfi_startproc
## BB#0:
	pushq	%rbp
Ltmp117:
	.cfi_def_cfa_offset 16
Ltmp118:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
Ltmp119:
	.cfi_def_cfa_register %rbp
	subq	$320, %rsp              ## imm = 0x140
	movq	%rdi, -280(%rbp)
	movq	-280(%rbp), %rdi
	movq	%rdi, -272(%rbp)
	movq	-272(%rbp), %rax
	movq	8(%rax), %rcx
	movq	%rax, -248(%rbp)
	movq	%rcx, -256(%rbp)
	movq	-248(%rbp), %rax
	movq	-256(%rbp), %rcx
	movq	%rax, -232(%rbp)
	movq	%rcx, -240(%rbp)
	movq	-232(%rbp), %rax
	movq	%rdi, -288(%rbp)        ## 8-byte Spill
	movq	%rax, -296(%rbp)        ## 8-byte Spill
LBB32_1:                                ## =>This Inner Loop Header: Depth=1
	movq	-240(%rbp), %rax
	movq	-296(%rbp), %rcx        ## 8-byte Reload
	cmpq	16(%rcx), %rax
	je	LBB32_3
## BB#2:                                ##   in Loop: Header=BB32_1 Depth=1
	movq	-296(%rbp), %rax        ## 8-byte Reload
	movq	%rax, -216(%rbp)
	movq	-216(%rbp), %rcx
	addq	$24, %rcx
	movq	%rcx, -208(%rbp)
	movq	-208(%rbp), %rcx
	movq	%rcx, -200(%rbp)
	movq	-200(%rbp), %rcx
	movq	8(%rcx), %rcx
	movq	16(%rax), %rdx
	addq	$-8, %rdx
	movq	%rdx, 16(%rax)
	movq	%rdx, -192(%rbp)
	movq	-192(%rbp), %rdx
	movq	%rcx, -160(%rbp)
	movq	%rdx, -168(%rbp)
	movq	-160(%rbp), %rcx
	movq	-168(%rbp), %rdx
	movq	%rcx, -144(%rbp)
	movq	%rdx, -152(%rbp)
	movq	-144(%rbp), %rcx
	movq	-152(%rbp), %rdx
	movq	%rcx, -120(%rbp)
	movq	%rdx, -128(%rbp)
	jmp	LBB32_1
LBB32_3:                                ## %_ZNSt3__114__split_bufferIyRNS_9allocatorIyEEE5clearEv.exit
	movq	-288(%rbp), %rax        ## 8-byte Reload
	cmpq	$0, (%rax)
	je	LBB32_6
## BB#4:
	movq	-288(%rbp), %rax        ## 8-byte Reload
	movq	%rax, -112(%rbp)
	movq	-112(%rbp), %rcx
	addq	$24, %rcx
	movq	%rcx, -104(%rbp)
	movq	-104(%rbp), %rcx
	movq	%rcx, -96(%rbp)
	movq	-96(%rbp), %rcx
	movq	8(%rcx), %rcx
	movq	(%rax), %rdx
	movq	%rax, -32(%rbp)
	movq	-32(%rbp), %rsi
	movq	%rsi, -24(%rbp)
	movq	-24(%rbp), %rdi
	addq	$24, %rdi
	movq	%rdi, -16(%rbp)
	movq	-16(%rbp), %rdi
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rdi
	movq	(%rdi), %rdi
	movq	(%rsi), %rsi
	subq	%rsi, %rdi
	sarq	$3, %rdi
	movq	%rcx, -304(%rbp)        ## 8-byte Spill
	movq	%rdx, -312(%rbp)        ## 8-byte Spill
	movq	%rdi, -320(%rbp)        ## 8-byte Spill
## BB#5:
	movq	-304(%rbp), %rax        ## 8-byte Reload
	movq	%rax, -72(%rbp)
	movq	-312(%rbp), %rcx        ## 8-byte Reload
	movq	%rcx, -80(%rbp)
	movq	-320(%rbp), %rdx        ## 8-byte Reload
	movq	%rdx, -88(%rbp)
	movq	-72(%rbp), %rsi
	movq	-80(%rbp), %rdi
	movq	-88(%rbp), %r8
	movq	%rsi, -48(%rbp)
	movq	%rdi, -56(%rbp)
	movq	%r8, -64(%rbp)
	movq	-56(%rbp), %rsi
	movq	%rsi, -40(%rbp)
	movq	-40(%rbp), %rdi
	callq	__ZdlPv
LBB32_6:
	addq	$320, %rsp              ## imm = 0x140
	popq	%rbp
	retq
	.cfi_endproc

	.private_extern	__ZNKSt3__16vectorIyNS_9allocatorIyEEE17__annotate_shrinkEm
	.globl	__ZNKSt3__16vectorIyNS_9allocatorIyEEE17__annotate_shrinkEm
	.weak_def_can_be_hidden	__ZNKSt3__16vectorIyNS_9allocatorIyEEE17__annotate_shrinkEm
	.align	4, 0x90
__ZNKSt3__16vectorIyNS_9allocatorIyEEE17__annotate_shrinkEm: ## @_ZNKSt3__16vectorIyNS_9allocatorIyEEE17__annotate_shrinkEm
	.cfi_startproc
## BB#0:
	pushq	%rbp
Ltmp120:
	.cfi_def_cfa_offset 16
Ltmp121:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
Ltmp122:
	.cfi_def_cfa_register %rbp
	subq	$144, %rsp
	movq	%rdi, -120(%rbp)
	movq	%rsi, -128(%rbp)
	movq	-120(%rbp), %rsi
	movq	%rsi, -112(%rbp)
	movq	-112(%rbp), %rdi
	movq	(%rdi), %rdi
	movq	%rdi, -104(%rbp)
	movq	-104(%rbp), %rdi
	movq	%rsi, -96(%rbp)
	movq	-96(%rbp), %rax
	movq	(%rax), %rax
	movq	%rax, -88(%rbp)
	movq	-88(%rbp), %rax
	movq	%rsi, -40(%rbp)
	movq	-40(%rbp), %rcx
	movq	%rcx, -32(%rbp)
	movq	-32(%rbp), %rcx
	movq	%rcx, -24(%rbp)
	movq	-24(%rbp), %rdx
	addq	$16, %rdx
	movq	%rdx, -16(%rbp)
	movq	-16(%rbp), %rdx
	movq	%rdx, -8(%rbp)
	movq	-8(%rbp), %rdx
	movq	(%rdx), %rdx
	movq	(%rcx), %rcx
	subq	%rcx, %rdx
	sarq	$3, %rdx
	shlq	$3, %rdx
	addq	%rdx, %rax
	movq	%rsi, -56(%rbp)
	movq	-56(%rbp), %rcx
	movq	(%rcx), %rcx
	movq	%rcx, -48(%rbp)
	movq	-48(%rbp), %rcx
	movq	-128(%rbp), %rdx
	shlq	$3, %rdx
	addq	%rdx, %rcx
	movq	%rsi, -72(%rbp)
	movq	-72(%rbp), %rdx
	movq	(%rdx), %rdx
	movq	%rdx, -64(%rbp)
	movq	-64(%rbp), %rdx
	movq	%rsi, -80(%rbp)
	movq	-80(%rbp), %r8
	movq	8(%r8), %r9
	movq	(%r8), %r8
	subq	%r8, %r9
	sarq	$3, %r9
	shlq	$3, %r9
	addq	%r9, %rdx
	movq	%rdi, -136(%rbp)        ## 8-byte Spill
	movq	%rsi, %rdi
	movq	-136(%rbp), %rsi        ## 8-byte Reload
	movq	%rdx, -144(%rbp)        ## 8-byte Spill
	movq	%rax, %rdx
	movq	-144(%rbp), %r8         ## 8-byte Reload
	callq	__ZNKSt3__16vectorIyNS_9allocatorIyEEE31__annotate_contiguous_containerEPKvS5_S5_S5_
	addq	$144, %rsp
	popq	%rbp
	retq
	.cfi_endproc

	.globl	__ZNSt3__113unordered_mapIiPN5lunar12green_thread7contextENS_4hashIiEENS_8equal_toIiEENS_9allocatorINS_4pairIKiS4_EEEEE25__construct_node_with_keyERSB_
	.weak_def_can_be_hidden	__ZNSt3__113unordered_mapIiPN5lunar12green_thread7contextENS_4hashIiEENS_8equal_toIiEENS_9allocatorINS_4pairIKiS4_EEEEE25__construct_node_with_keyERSB_
	.align	4, 0x90
__ZNSt3__113unordered_mapIiPN5lunar12green_thread7contextENS_4hashIiEENS_8equal_toIiEENS_9allocatorINS_4pairIKiS4_EEEEE25__construct_node_with_keyERSB_: ## @_ZNSt3__113unordered_mapIiPN5lunar12green_thread7contextENS_4hashIiEENS_8equal_toIiEENS_9allocatorINS_4pairIKiS4_EEEEE25__construct_node_with_keyERSB_
	.cfi_startproc
## BB#0:
	pushq	%rbp
Ltmp123:
	.cfi_def_cfa_offset 16
Ltmp124:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
Ltmp125:
	.cfi_def_cfa_register %rbp
	pushq	%rbx
	subq	$1560, %rsp             ## imm = 0x618
Ltmp126:
	.cfi_offset %rbx, -24
	movq	%rdi, %rax
	movq	%rsi, -1464(%rbp)
	movq	%rdx, -1472(%rbp)
	movq	-1464(%rbp), %rdx
	movq	%rdx, -1456(%rbp)
	movq	-1456(%rbp), %rdx
	addq	$16, %rdx
	movq	%rdx, -1448(%rbp)
	movq	-1448(%rbp), %rdx
	movq	%rdx, -1440(%rbp)
	movq	-1440(%rbp), %rdx
	movq	%rdx, -1480(%rbp)
	movq	-1480(%rbp), %rdx
	movq	%rdx, -1152(%rbp)
	movq	$1, -1160(%rbp)
	movq	-1152(%rbp), %rdx
	movq	-1160(%rbp), %rsi
	movq	%rdx, -1128(%rbp)
	movq	%rsi, -1136(%rbp)
	movq	$0, -1144(%rbp)
	movq	-1136(%rbp), %rdx
	shlq	$5, %rdx
	movq	%rdx, -1120(%rbp)
	movq	-1120(%rbp), %rdx
	movq	%rdi, -1544(%rbp)       ## 8-byte Spill
	movq	%rdx, %rdi
	movq	%rax, -1552(%rbp)       ## 8-byte Spill
	callq	__Znwm
	leaq	-1504(%rbp), %rdx
	leaq	-632(%rbp), %rsi
	leaq	-648(%rbp), %rdi
	leaq	-672(%rbp), %rcx
	leaq	-688(%rbp), %r8
	leaq	-1520(%rbp), %r9
	movq	-1480(%rbp), %r10
	movq	%r9, -1104(%rbp)
	movq	%r10, -1112(%rbp)
	movq	-1104(%rbp), %r10
	movq	-1112(%rbp), %r11
	movq	%r10, -1088(%rbp)
	movq	%r11, -1096(%rbp)
	movq	-1088(%rbp), %r10
	movq	-1096(%rbp), %r11
	movq	%r11, (%r10)
	movb	$0, 8(%r10)
	movb	$0, 9(%r10)
	movq	%rdx, -792(%rbp)
	movq	%rax, -800(%rbp)
	movq	%r9, -808(%rbp)
	movq	-792(%rbp), %rax
	movq	-800(%rbp), %r9
	movq	-808(%rbp), %r10
	movq	%rax, -752(%rbp)
	movq	%r9, -760(%rbp)
	movq	%r10, -768(%rbp)
	movq	-752(%rbp), %rax
	movq	-760(%rbp), %r9
	movq	-768(%rbp), %r10
	movq	%r10, -744(%rbp)
	movq	-744(%rbp), %r10
	movq	(%r10), %r11
	movq	%r11, -784(%rbp)
	movq	8(%r10), %r10
	movq	%r10, -776(%rbp)
	movq	-784(%rbp), %r10
	movq	-776(%rbp), %r11
	movq	%r10, -720(%rbp)
	movq	%r11, -712(%rbp)
	movq	%rax, -728(%rbp)
	movq	%r9, -736(%rbp)
	movq	-728(%rbp), %rax
	movq	-736(%rbp), %r9
	movq	-720(%rbp), %r10
	movq	-712(%rbp), %r11
	movq	%r10, -672(%rbp)
	movq	%r11, -664(%rbp)
	movq	%rax, -680(%rbp)
	movq	%r9, -688(%rbp)
	movq	-680(%rbp), %rax
	movq	%r8, -656(%rbp)
	movq	-656(%rbp), %r8
	movq	(%r8), %r8
	movq	%rcx, -600(%rbp)
	movq	-600(%rbp), %rcx
	movq	(%rcx), %r9
	movq	%r9, -704(%rbp)
	movq	8(%rcx), %rcx
	movq	%rcx, -696(%rbp)
	movq	-704(%rbp), %rcx
	movq	-696(%rbp), %r9
	movq	%rcx, -632(%rbp)
	movq	%r9, -624(%rbp)
	movq	%rax, -640(%rbp)
	movq	%r8, -648(%rbp)
	movq	-640(%rbp), %rax
	movq	%rdi, -616(%rbp)
	movq	-616(%rbp), %rcx
	movq	(%rcx), %rcx
	movq	%rcx, (%rax)
	movq	%rsi, -608(%rbp)
	movq	-608(%rbp), %rcx
	movq	(%rcx), %rsi
	movq	%rsi, 8(%rax)
	movq	8(%rcx), %rcx
	movq	%rcx, 16(%rax)
	movq	-1480(%rbp), %rax
	movq	%rdx, -32(%rbp)
	movq	-32(%rbp), %rcx
	movq	%rcx, -24(%rbp)
	movq	-24(%rbp), %rcx
	movq	%rcx, -16(%rbp)
	movq	-16(%rbp), %rcx
	movq	(%rcx), %rcx
	addq	$16, %rcx
	movq	%rcx, -40(%rbp)
	movq	-40(%rbp), %rcx
	movq	-1472(%rbp), %rdx
	movq	%rax, -128(%rbp)
	movq	%rcx, -136(%rbp)
	movq	%rdx, -144(%rbp)
	movq	-128(%rbp), %rax
	movq	-136(%rbp), %rcx
	movq	-144(%rbp), %rdx
	movq	%rdx, -120(%rbp)
	movq	-120(%rbp), %rdx
	movq	%rax, -96(%rbp)
	movq	%rcx, -104(%rbp)
	movq	%rdx, -112(%rbp)
	movq	-96(%rbp), %rax
	movq	-104(%rbp), %rcx
	movq	-112(%rbp), %rdx
	movq	%rdx, -80(%rbp)
	movq	-80(%rbp), %rdx
	movq	%rax, -56(%rbp)
	movq	%rcx, -64(%rbp)
	movq	%rdx, -72(%rbp)
	movq	-64(%rbp), %rax
	movq	-72(%rbp), %rcx
	movq	%rcx, -48(%rbp)
	movq	-48(%rbp), %rcx
	movl	(%rcx), %ebx
	movl	%ebx, (%rax)
## BB#1:
	leaq	-1504(%rbp), %rax
	movq	%rax, -184(%rbp)
	movq	-184(%rbp), %rcx
	movq	%rcx, -176(%rbp)
	movq	-176(%rbp), %rcx
	movq	%rcx, -168(%rbp)
	movq	-168(%rbp), %rcx
	movb	$1, 16(%rcx)
	movq	-1480(%rbp), %rcx
	movq	%rax, -208(%rbp)
	movq	-208(%rbp), %rax
	movq	%rax, -200(%rbp)
	movq	-200(%rbp), %rax
	movq	%rax, -192(%rbp)
	movq	-192(%rbp), %rax
	movq	(%rax), %rax
	addq	$16, %rax
	addq	$8, %rax
	movq	%rax, -216(%rbp)
	movq	-216(%rbp), %rax
	movq	%rcx, -264(%rbp)
	movq	%rax, -272(%rbp)
	movq	-264(%rbp), %rax
	movq	-272(%rbp), %rcx
	movq	%rax, -248(%rbp)
	movq	%rcx, -256(%rbp)
	movq	-248(%rbp), %rax
	movq	-256(%rbp), %rcx
	movq	%rax, -224(%rbp)
	movq	%rcx, -232(%rbp)
	movq	-232(%rbp), %rax
	movq	$0, (%rax)
## BB#2:
	leaq	-1504(%rbp), %rax
	leaq	-368(%rbp), %rcx
	leaq	-384(%rbp), %rdx
	leaq	-408(%rbp), %rsi
	leaq	-424(%rbp), %rdi
	movq	%rax, -312(%rbp)
	movq	-312(%rbp), %r8
	movq	%r8, -304(%rbp)
	movq	-304(%rbp), %r8
	movq	%r8, -296(%rbp)
	movq	-296(%rbp), %r8
	movb	$1, 17(%r8)
	movq	%rax, -320(%rbp)
	movq	-320(%rbp), %r8
	movq	-1544(%rbp), %r9        ## 8-byte Reload
	movq	%r9, -584(%rbp)
	movq	%r8, -592(%rbp)
	movq	-584(%rbp), %r8
	movq	-592(%rbp), %r10
	movq	%r8, -552(%rbp)
	movq	%r10, -560(%rbp)
	movq	-552(%rbp), %r8
	movq	-560(%rbp), %r10
	movq	%r10, -536(%rbp)
	movq	-536(%rbp), %r10
	movq	%r10, -528(%rbp)
	movq	-528(%rbp), %r11
	movq	%r11, -520(%rbp)
	movq	-520(%rbp), %r11
	movq	(%r11), %r11
	movq	%r11, -544(%rbp)
	movq	%r10, -512(%rbp)
	movq	-512(%rbp), %r10
	movq	%r10, -504(%rbp)
	movq	-504(%rbp), %r10
	movq	$0, (%r10)
	movq	-544(%rbp), %r10
	movq	-560(%rbp), %r11
	movq	%r11, -496(%rbp)
	movq	-496(%rbp), %r11
	movq	%r11, -488(%rbp)
	movq	-488(%rbp), %r11
	movq	%r11, -480(%rbp)
	movq	-480(%rbp), %r11
	addq	$8, %r11
	movq	%r11, -328(%rbp)
	movq	-328(%rbp), %r11
	movq	(%r11), %rbx
	movq	%rbx, -576(%rbp)
	movq	8(%r11), %r11
	movq	%r11, -568(%rbp)
	movq	-576(%rbp), %r11
	movq	-568(%rbp), %rbx
	movq	%r11, -456(%rbp)
	movq	%rbx, -448(%rbp)
	movq	%r8, -464(%rbp)
	movq	%r10, -472(%rbp)
	movq	-464(%rbp), %r8
	movq	-472(%rbp), %r10
	movq	-456(%rbp), %r11
	movq	-448(%rbp), %rbx
	movq	%r11, -408(%rbp)
	movq	%rbx, -400(%rbp)
	movq	%r8, -416(%rbp)
	movq	%r10, -424(%rbp)
	movq	-416(%rbp), %r8
	movq	%rdi, -392(%rbp)
	movq	-392(%rbp), %rdi
	movq	(%rdi), %rdi
	movq	%rsi, -336(%rbp)
	movq	-336(%rbp), %rsi
	movq	(%rsi), %r10
	movq	%r10, -440(%rbp)
	movq	8(%rsi), %rsi
	movq	%rsi, -432(%rbp)
	movq	-440(%rbp), %rsi
	movq	-432(%rbp), %r10
	movq	%rsi, -368(%rbp)
	movq	%r10, -360(%rbp)
	movq	%r8, -376(%rbp)
	movq	%rdi, -384(%rbp)
	movq	-376(%rbp), %rsi
	movq	%rdx, -352(%rbp)
	movq	-352(%rbp), %rdx
	movq	(%rdx), %rdx
	movq	%rdx, (%rsi)
	movq	%rcx, -344(%rbp)
	movq	-344(%rbp), %rcx
	movq	(%rcx), %rdx
	movq	%rdx, 8(%rsi)
	movq	8(%rcx), %rcx
	movq	%rcx, 16(%rsi)
	movl	$1, -1536(%rbp)
	movq	%rax, -1080(%rbp)
	movq	-1080(%rbp), %rax
	movq	%rax, -1072(%rbp)
	movq	-1072(%rbp), %rax
	movq	%rax, -1048(%rbp)
	movq	$0, -1056(%rbp)
	movq	-1048(%rbp), %rax
	movq	%rax, -1040(%rbp)
	movq	-1040(%rbp), %rcx
	movq	%rcx, -1032(%rbp)
	movq	-1032(%rbp), %rcx
	movq	(%rcx), %rcx
	movq	%rcx, -1064(%rbp)
	movq	-1056(%rbp), %rcx
	movq	%rax, -840(%rbp)
	movq	-840(%rbp), %rdx
	movq	%rdx, -832(%rbp)
	movq	-832(%rbp), %rdx
	movq	%rcx, (%rdx)
	cmpq	$0, -1064(%rbp)
	movq	%rax, -1560(%rbp)       ## 8-byte Spill
	je	LBB34_10
## BB#3:
	movq	-1560(%rbp), %rax       ## 8-byte Reload
	movq	%rax, -824(%rbp)
	movq	-824(%rbp), %rcx
	movq	%rcx, -816(%rbp)
	movq	-816(%rbp), %rcx
	addq	$8, %rcx
	movq	-1064(%rbp), %rdx
	movq	%rcx, -1016(%rbp)
	movq	%rdx, -1024(%rbp)
	movq	-1016(%rbp), %rcx
	testb	$1, 9(%rcx)
	movq	%rcx, -1568(%rbp)       ## 8-byte Spill
	je	LBB34_5
## BB#4:
	movq	-1568(%rbp), %rax       ## 8-byte Reload
	movq	(%rax), %rcx
	movq	-1024(%rbp), %rdx
	addq	$16, %rdx
	addq	$8, %rdx
	movq	%rdx, -1008(%rbp)
	movq	-1008(%rbp), %rdx
	movq	%rcx, -976(%rbp)
	movq	%rdx, -984(%rbp)
	movq	-976(%rbp), %rcx
	movq	-984(%rbp), %rdx
	movq	%rcx, -960(%rbp)
	movq	%rdx, -968(%rbp)
LBB34_5:
	movq	-1568(%rbp), %rax       ## 8-byte Reload
	testb	$1, 8(%rax)
	je	LBB34_7
## BB#6:
	movq	-1568(%rbp), %rax       ## 8-byte Reload
	movq	(%rax), %rcx
	movq	-1024(%rbp), %rdx
	addq	$16, %rdx
	movq	%rdx, -896(%rbp)
	movq	-896(%rbp), %rdx
	movq	%rcx, -864(%rbp)
	movq	%rdx, -872(%rbp)
	movq	-864(%rbp), %rcx
	movq	-872(%rbp), %rdx
	movq	%rcx, -848(%rbp)
	movq	%rdx, -856(%rbp)
LBB34_7:
	cmpq	$0, -1024(%rbp)
	je	LBB34_9
## BB#8:
	movq	-1568(%rbp), %rax       ## 8-byte Reload
	movq	(%rax), %rcx
	movq	-1024(%rbp), %rdx
	movq	%rcx, -936(%rbp)
	movq	%rdx, -944(%rbp)
	movq	$1, -952(%rbp)
	movq	-936(%rbp), %rcx
	movq	-944(%rbp), %rdx
	movq	-952(%rbp), %rsi
	movq	%rcx, -912(%rbp)
	movq	%rdx, -920(%rbp)
	movq	%rsi, -928(%rbp)
	movq	-920(%rbp), %rcx
	movq	%rcx, -904(%rbp)
	movq	-904(%rbp), %rdi
	callq	__ZdlPv
LBB34_9:                                ## %_ZNSt3__126__hash_map_node_destructorINS_9allocatorINS_11__hash_nodeINS_17__hash_value_typeIiPN5lunar12green_thread7contextEEEPvEEEEEclEPSA_.exit.i.i.i2
	jmp	LBB34_10
LBB34_10:                               ## %_ZNSt3__110unique_ptrINS_11__hash_nodeINS_17__hash_value_typeIiPN5lunar12green_thread7contextEEEPvEENS_26__hash_map_node_destructorINS_9allocatorIS9_EEEEED1Ev.exit3
	movq	-1552(%rbp), %rax       ## 8-byte Reload
	addq	$1560, %rsp             ## imm = 0x618
	popq	%rbx
	popq	%rbp
	retq
	.cfi_endproc

	.section	__TEXT,__literal4,4byte_literals
	.align	2
LCPI35_0:
	.long	1593835520              ## float 9.22337203E+18
	.section	__TEXT,__textcoal_nt,coalesced,pure_instructions
	.globl	__ZNSt3__112__hash_tableINS_17__hash_value_typeIiPN5lunar12green_thread7contextEEENS_22__unordered_map_hasherIiS6_NS_4hashIiEELb1EEENS_21__unordered_map_equalIiS6_NS_8equal_toIiEELb1EEENS_9allocatorIS6_EEE20__node_insert_uniqueEPNS_11__hash_nodeIS6_PvEE
	.weak_def_can_be_hidden	__ZNSt3__112__hash_tableINS_17__hash_value_typeIiPN5lunar12green_thread7contextEEENS_22__unordered_map_hasherIiS6_NS_4hashIiEELb1EEENS_21__unordered_map_equalIiS6_NS_8equal_toIiEELb1EEENS_9allocatorIS6_EEE20__node_insert_uniqueEPNS_11__hash_nodeIS6_PvEE
	.align	4, 0x90
__ZNSt3__112__hash_tableINS_17__hash_value_typeIiPN5lunar12green_thread7contextEEENS_22__unordered_map_hasherIiS6_NS_4hashIiEELb1EEENS_21__unordered_map_equalIiS6_NS_8equal_toIiEELb1EEENS_9allocatorIS6_EEE20__node_insert_uniqueEPNS_11__hash_nodeIS6_PvEE: ## @_ZNSt3__112__hash_tableINS_17__hash_value_typeIiPN5lunar12green_thread7contextEEENS_22__unordered_map_hasherIiS6_NS_4hashIiEELb1EEENS_21__unordered_map_equalIiS6_NS_8equal_toIiEELb1EEENS_9allocatorIS6_EEE20__node_insert_uniqueEPNS_11__hash_nodeIS6_PvEE
	.cfi_startproc
## BB#0:
	pushq	%rbp
Ltmp127:
	.cfi_def_cfa_offset 16
Ltmp128:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
Ltmp129:
	.cfi_def_cfa_register %rbp
	subq	$1008, %rsp             ## imm = 0x3F0
	movq	%rdi, -792(%rbp)
	movq	%rsi, -800(%rbp)
	movq	-792(%rbp), %rsi
	movq	%rsi, -768(%rbp)
	movq	-768(%rbp), %rdi
	addq	$24, %rdi
	movq	%rdi, -760(%rbp)
	movq	-760(%rbp), %rdi
	movq	%rdi, -752(%rbp)
	movq	-752(%rbp), %rdi
	movq	-800(%rbp), %rax
	addq	$16, %rax
	movq	%rdi, -400(%rbp)
	movq	%rax, -408(%rbp)
	movq	-400(%rbp), %rax
	movq	-408(%rbp), %rdi
	movl	(%rdi), %ecx
	movq	%rax, -384(%rbp)
	movl	%ecx, -388(%rbp)
	movslq	-388(%rbp), %rax
	movq	-800(%rbp), %rdi
	movq	%rax, 8(%rdi)
	movq	%rsi, -296(%rbp)
	movq	-296(%rbp), %rax
	movq	%rax, -288(%rbp)
	movq	-288(%rbp), %rax
	movq	%rax, -280(%rbp)
	movq	-280(%rbp), %rax
	movq	%rax, -272(%rbp)
	movq	-272(%rbp), %rax
	addq	$8, %rax
	movq	%rax, -264(%rbp)
	movq	-264(%rbp), %rax
	movq	%rax, -256(%rbp)
	movq	-256(%rbp), %rax
	movq	%rax, -248(%rbp)
	movq	-248(%rbp), %rax
	movq	(%rax), %rax
	movq	%rax, -808(%rbp)
	movb	$0, -809(%rbp)
	cmpq	$0, -808(%rbp)
	movq	%rsi, -872(%rbp)        ## 8-byte Spill
	je	LBB35_18
## BB#1:
	movq	-800(%rbp), %rax
	movq	8(%rax), %rax
	movq	-808(%rbp), %rcx
	movq	%rax, -8(%rbp)
	movq	%rcx, -16(%rbp)
	movq	-16(%rbp), %rax
	movq	-16(%rbp), %rcx
	subq	$1, %rcx
	andq	%rcx, %rax
	cmpq	$0, %rax
	jne	LBB35_3
## BB#2:
	movq	-8(%rbp), %rax
	movq	-16(%rbp), %rcx
	subq	$1, %rcx
	andq	%rcx, %rax
	movq	%rax, -880(%rbp)        ## 8-byte Spill
	jmp	LBB35_4
LBB35_3:
	movq	-8(%rbp), %rax
	xorl	%ecx, %ecx
	movl	%ecx, %edx
	divq	-16(%rbp)
	movq	%rdx, -880(%rbp)        ## 8-byte Spill
LBB35_4:                                ## %_ZNSt3__116__constrain_hashEmm.exit3
	movq	-880(%rbp), %rax        ## 8-byte Reload
	movq	%rax, -832(%rbp)
	movq	-832(%rbp), %rax
	movq	-872(%rbp), %rcx        ## 8-byte Reload
	movq	%rcx, -40(%rbp)
	movq	%rax, -48(%rbp)
	movq	-40(%rbp), %rax
	movq	-48(%rbp), %rdx
	movq	%rax, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	%rax, -24(%rbp)
	movq	-24(%rbp), %rax
	movq	(%rax), %rax
	movq	(%rax,%rdx,8), %rax
	movq	%rax, -824(%rbp)
	cmpq	$0, -824(%rbp)
	je	LBB35_17
## BB#5:
	movq	-824(%rbp), %rax
	movq	(%rax), %rax
	movq	%rax, -824(%rbp)
LBB35_6:                                ## =>This Inner Loop Header: Depth=1
	xorl	%eax, %eax
	movb	%al, %cl
	cmpq	$0, -824(%rbp)
	movb	%cl, -881(%rbp)         ## 1-byte Spill
	je	LBB35_11
## BB#7:                                ##   in Loop: Header=BB35_6 Depth=1
	movq	-824(%rbp), %rax
	movq	8(%rax), %rax
	movq	-808(%rbp), %rcx
	movq	%rax, -56(%rbp)
	movq	%rcx, -64(%rbp)
	movq	-64(%rbp), %rax
	movq	-64(%rbp), %rcx
	subq	$1, %rcx
	andq	%rcx, %rax
	cmpq	$0, %rax
	jne	LBB35_9
## BB#8:                                ##   in Loop: Header=BB35_6 Depth=1
	movq	-56(%rbp), %rax
	movq	-64(%rbp), %rcx
	subq	$1, %rcx
	andq	%rcx, %rax
	movq	%rax, -896(%rbp)        ## 8-byte Spill
	jmp	LBB35_10
LBB35_9:                                ##   in Loop: Header=BB35_6 Depth=1
	movq	-56(%rbp), %rax
	xorl	%ecx, %ecx
	movl	%ecx, %edx
	divq	-64(%rbp)
	movq	%rdx, -896(%rbp)        ## 8-byte Spill
LBB35_10:                               ## %_ZNSt3__116__constrain_hashEmm.exit2
                                        ##   in Loop: Header=BB35_6 Depth=1
	movq	-896(%rbp), %rax        ## 8-byte Reload
	cmpq	-832(%rbp), %rax
	sete	%cl
	movb	%cl, -881(%rbp)         ## 1-byte Spill
LBB35_11:                               ##   in Loop: Header=BB35_6 Depth=1
	movb	-881(%rbp), %al         ## 1-byte Reload
	testb	$1, %al
	jne	LBB35_12
	jmp	LBB35_16
LBB35_12:                               ##   in Loop: Header=BB35_6 Depth=1
	movq	-872(%rbp), %rax        ## 8-byte Reload
	movq	%rax, -88(%rbp)
	movq	-88(%rbp), %rcx
	addq	$32, %rcx
	movq	%rcx, -80(%rbp)
	movq	-80(%rbp), %rcx
	movq	%rcx, -72(%rbp)
	movq	-72(%rbp), %rcx
	movq	-824(%rbp), %rdx
	addq	$16, %rdx
	movq	-800(%rbp), %rsi
	addq	$16, %rsi
	movq	%rcx, -120(%rbp)
	movq	%rdx, -128(%rbp)
	movq	%rsi, -136(%rbp)
	movq	-120(%rbp), %rcx
	movq	-128(%rbp), %rdx
	movq	-136(%rbp), %rsi
	movq	%rcx, -96(%rbp)
	movq	%rdx, -104(%rbp)
	movq	%rsi, -112(%rbp)
	movq	-104(%rbp), %rcx
	movl	(%rcx), %edi
	movq	-112(%rbp), %rcx
	cmpl	(%rcx), %edi
	jne	LBB35_14
## BB#13:
	jmp	LBB35_38
LBB35_14:                               ##   in Loop: Header=BB35_6 Depth=1
	jmp	LBB35_15
LBB35_15:                               ##   in Loop: Header=BB35_6 Depth=1
	movq	-824(%rbp), %rax
	movq	(%rax), %rax
	movq	%rax, -824(%rbp)
	jmp	LBB35_6
LBB35_16:
	jmp	LBB35_17
LBB35_17:
	jmp	LBB35_18
LBB35_18:
	movq	-872(%rbp), %rax        ## 8-byte Reload
	movq	%rax, -160(%rbp)
	addq	$24, %rax
	movq	%rax, -152(%rbp)
	movq	%rax, -144(%rbp)
	movq	-872(%rbp), %rax        ## 8-byte Reload
	movq	24(%rax), %rcx
	incq	%rcx
	movq	%rcx, %rdx
	shrq	%rdx
	movq	%rcx, %rsi
	andq	$1, %rsi
	orq	%rdx, %rsi
	cvtsi2ssq	%rsi, %xmm0
	addss	%xmm0, %xmm0
	cvtsi2ssq	%rcx, %xmm1
	testq	%rcx, %rcx
	movss	%xmm1, -900(%rbp)       ## 4-byte Spill
	movss	%xmm0, -904(%rbp)       ## 4-byte Spill
	js	LBB35_40
## BB#39:
	movss	-900(%rbp), %xmm0       ## 4-byte Reload
                                        ## xmm0 = mem[0],zero,zero,zero
	movss	%xmm0, -904(%rbp)       ## 4-byte Spill
LBB35_40:
	movss	-904(%rbp), %xmm0       ## 4-byte Reload
                                        ## xmm0 = mem[0],zero,zero,zero
	movq	-808(%rbp), %rax
	movq	%rax, %rcx
	shrq	%rcx
	movq	%rax, %rdx
	andq	$1, %rdx
	orq	%rcx, %rdx
	cvtsi2ssq	%rdx, %xmm1
	addss	%xmm1, %xmm1
	cvtsi2ssq	%rax, %xmm2
	testq	%rax, %rax
	movss	%xmm0, -908(%rbp)       ## 4-byte Spill
	movss	%xmm2, -912(%rbp)       ## 4-byte Spill
	movss	%xmm1, -916(%rbp)       ## 4-byte Spill
	js	LBB35_42
## BB#41:
	movss	-912(%rbp), %xmm0       ## 4-byte Reload
                                        ## xmm0 = mem[0],zero,zero,zero
	movss	%xmm0, -916(%rbp)       ## 4-byte Spill
LBB35_42:
	movss	-916(%rbp), %xmm0       ## 4-byte Reload
                                        ## xmm0 = mem[0],zero,zero,zero
	movq	-872(%rbp), %rax        ## 8-byte Reload
	movq	%rax, -184(%rbp)
	movq	-184(%rbp), %rcx
	addq	$32, %rcx
	movq	%rcx, -176(%rbp)
	movq	-176(%rbp), %rcx
	movq	%rcx, -168(%rbp)
	movq	-168(%rbp), %rcx
	mulss	(%rcx), %xmm0
	movss	-908(%rbp), %xmm1       ## 4-byte Reload
                                        ## xmm1 = mem[0],zero,zero,zero
	ucomiss	%xmm0, %xmm1
	ja	LBB35_20
## BB#19:
	cmpq	$0, -808(%rbp)
	jne	LBB35_29
LBB35_20:
	xorl	%eax, %eax
	movb	%al, %cl
	movq	-808(%rbp), %rdx
	shlq	$1, %rdx
	movq	-808(%rbp), %rsi
	movq	%rsi, -192(%rbp)
	cmpq	$2, -192(%rbp)
	movq	%rdx, -928(%rbp)        ## 8-byte Spill
	movb	%cl, -929(%rbp)         ## 1-byte Spill
	jbe	LBB35_22
## BB#21:
	movq	-192(%rbp), %rax
	movq	-192(%rbp), %rcx
	subq	$1, %rcx
	andq	%rcx, %rax
	cmpq	$0, %rax
	setne	%dl
	xorb	$1, %dl
	movb	%dl, -929(%rbp)         ## 1-byte Spill
LBB35_22:                               ## %_ZNSt3__116__is_hash_power2Em.exit
	movb	-929(%rbp), %al         ## 1-byte Reload
	leaq	-336(%rbp), %rcx
	leaq	-848(%rbp), %rdx
	leaq	-840(%rbp), %rsi
	notb	%al
	movzbl	%al, %edi
	movl	%edi, %r8d
	andq	$1, %r8
	movq	-928(%rbp), %r9         ## 8-byte Reload
	addq	%r8, %r9
	movq	%r9, -840(%rbp)
	movq	-872(%rbp), %r8         ## 8-byte Reload
	movq	%r8, -216(%rbp)
	addq	$24, %r8
	movq	%r8, -208(%rbp)
	movq	%r8, -200(%rbp)
	movq	-872(%rbp), %r8         ## 8-byte Reload
	movq	24(%r8), %r9
	incq	%r9
	movq	%r9, %r10
	shrq	%r10
	movq	%r9, %r11
	andq	$1, %r11
	orq	%r10, %r11
	cvtsi2ssq	%r11, %xmm0
	addss	%xmm0, %xmm0
	cvtsi2ssq	%r9, %xmm1
	testq	%r9, %r9
	movq	%rsi, -944(%rbp)        ## 8-byte Spill
	movq	%rcx, -952(%rbp)        ## 8-byte Spill
	movq	%rdx, -960(%rbp)        ## 8-byte Spill
	movss	%xmm1, -964(%rbp)       ## 4-byte Spill
	movss	%xmm0, -968(%rbp)       ## 4-byte Spill
	js	LBB35_44
## BB#43:                               ## %_ZNSt3__116__is_hash_power2Em.exit
	movss	-964(%rbp), %xmm0       ## 4-byte Reload
                                        ## xmm0 = mem[0],zero,zero,zero
	movss	%xmm0, -968(%rbp)       ## 4-byte Spill
LBB35_44:                               ## %_ZNSt3__116__is_hash_power2Em.exit
	movss	-968(%rbp), %xmm0       ## 4-byte Reload
                                        ## xmm0 = mem[0],zero,zero,zero
	movq	-872(%rbp), %rax        ## 8-byte Reload
	movq	%rax, -240(%rbp)
	addq	$32, %rax
	movq	%rax, -232(%rbp)
	movq	%rax, -224(%rbp)
	movq	-872(%rbp), %rax        ## 8-byte Reload
	movss	32(%rax), %xmm1         ## xmm1 = mem[0],zero,zero,zero
	divss	%xmm1, %xmm0
	movss	%xmm0, -300(%rbp)
	callq	_ceilf
	movss	LCPI35_0(%rip), %xmm1   ## xmm1 = mem[0],zero,zero,zero
	movaps	%xmm0, %xmm2
	subss	%xmm1, %xmm2
	cvttss2si	%xmm2, %rax
	movabsq	$-9223372036854775808, %rcx ## imm = 0x8000000000000000
	xorq	%rcx, %rax
	cvttss2si	%xmm0, %rcx
	ucomiss	%xmm1, %xmm0
	cmovbq	%rcx, %rax
	movq	%rax, -848(%rbp)
	movq	-944(%rbp), %rax        ## 8-byte Reload
	movq	%rax, -360(%rbp)
	movq	-960(%rbp), %rcx        ## 8-byte Reload
	movq	%rcx, -368(%rbp)
	movq	-360(%rbp), %rdx
	movq	-368(%rbp), %rsi
	movq	%rdx, -344(%rbp)
	movq	%rsi, -352(%rbp)
	movq	-344(%rbp), %rdx
	movq	-352(%rbp), %rsi
	movq	-952(%rbp), %rdi        ## 8-byte Reload
	movq	%rdi, -312(%rbp)
	movq	%rdx, -320(%rbp)
	movq	%rsi, -328(%rbp)
	movq	-320(%rbp), %rdx
	movq	(%rdx), %rdx
	movq	-328(%rbp), %rsi
	cmpq	(%rsi), %rdx
	jae	LBB35_24
## BB#23:
	movq	-352(%rbp), %rax
	movq	%rax, -976(%rbp)        ## 8-byte Spill
	jmp	LBB35_25
LBB35_24:
	movq	-344(%rbp), %rax
	movq	%rax, -976(%rbp)        ## 8-byte Spill
LBB35_25:                               ## %_ZNSt3__13maxImEERKT_S3_S3_.exit
	movq	-976(%rbp), %rax        ## 8-byte Reload
	movq	(%rax), %rsi
	movq	-872(%rbp), %rdi        ## 8-byte Reload
	callq	__ZNSt3__112__hash_tableINS_17__hash_value_typeIiPN5lunar12green_thread7contextEEENS_22__unordered_map_hasherIiS6_NS_4hashIiEELb1EEENS_21__unordered_map_equalIiS6_NS_8equal_toIiEELb1EEENS_9allocatorIS6_EEE6rehashEm
	movq	-872(%rbp), %rax        ## 8-byte Reload
	movq	%rax, -464(%rbp)
	movq	-464(%rbp), %rsi
	movq	%rsi, -456(%rbp)
	movq	-456(%rbp), %rsi
	movq	%rsi, -448(%rbp)
	movq	-448(%rbp), %rsi
	movq	%rsi, -440(%rbp)
	movq	-440(%rbp), %rsi
	addq	$8, %rsi
	movq	%rsi, -432(%rbp)
	movq	-432(%rbp), %rsi
	movq	%rsi, -424(%rbp)
	movq	-424(%rbp), %rsi
	movq	%rsi, -416(%rbp)
	movq	-416(%rbp), %rsi
	movq	(%rsi), %rsi
	movq	%rsi, -808(%rbp)
	movq	-800(%rbp), %rsi
	movq	8(%rsi), %rsi
	movq	-808(%rbp), %rdi
	movq	%rsi, -472(%rbp)
	movq	%rdi, -480(%rbp)
	movq	-480(%rbp), %rsi
	movq	-480(%rbp), %rdi
	subq	$1, %rdi
	andq	%rdi, %rsi
	cmpq	$0, %rsi
	jne	LBB35_27
## BB#26:
	movq	-472(%rbp), %rax
	movq	-480(%rbp), %rcx
	subq	$1, %rcx
	andq	%rcx, %rax
	movq	%rax, -984(%rbp)        ## 8-byte Spill
	jmp	LBB35_28
LBB35_27:
	movq	-472(%rbp), %rax
	xorl	%ecx, %ecx
	movl	%ecx, %edx
	divq	-480(%rbp)
	movq	%rdx, -984(%rbp)        ## 8-byte Spill
LBB35_28:                               ## %_ZNSt3__116__constrain_hashEmm.exit1
	movq	-984(%rbp), %rax        ## 8-byte Reload
	movq	%rax, -832(%rbp)
LBB35_29:
	movq	-832(%rbp), %rax
	movq	-872(%rbp), %rcx        ## 8-byte Reload
	movq	%rcx, -504(%rbp)
	movq	%rax, -512(%rbp)
	movq	-504(%rbp), %rax
	movq	-512(%rbp), %rdx
	movq	%rax, -496(%rbp)
	movq	-496(%rbp), %rax
	movq	%rax, -488(%rbp)
	movq	-488(%rbp), %rax
	movq	(%rax), %rax
	movq	(%rax,%rdx,8), %rax
	movq	%rax, -856(%rbp)
	cmpq	$0, -856(%rbp)
	jne	LBB35_36
## BB#30:
	movq	-872(%rbp), %rax        ## 8-byte Reload
	addq	$16, %rax
	movq	%rax, -528(%rbp)
	movq	-528(%rbp), %rax
	movq	%rax, -520(%rbp)
	movq	-520(%rbp), %rax
	movq	%rax, -544(%rbp)
	movq	-544(%rbp), %rax
	movq	%rax, -536(%rbp)
	movq	-536(%rbp), %rax
	movq	%rax, -856(%rbp)
	movq	-856(%rbp), %rax
	movq	(%rax), %rax
	movq	-800(%rbp), %rcx
	movq	%rax, (%rcx)
	movq	-800(%rbp), %rax
	movq	-856(%rbp), %rcx
	movq	%rax, (%rcx)
	movq	-856(%rbp), %rax
	movq	-832(%rbp), %rcx
	movq	-872(%rbp), %rdx        ## 8-byte Reload
	movq	%rdx, -568(%rbp)
	movq	%rcx, -576(%rbp)
	movq	-568(%rbp), %rcx
	movq	-576(%rbp), %rsi
	movq	%rcx, -560(%rbp)
	movq	-560(%rbp), %rcx
	movq	%rcx, -552(%rbp)
	movq	-552(%rbp), %rcx
	movq	(%rcx), %rcx
	movq	%rax, (%rcx,%rsi,8)
	movq	-800(%rbp), %rax
	cmpq	$0, (%rax)
	je	LBB35_35
## BB#31:
	movq	-800(%rbp), %rax
	movq	-800(%rbp), %rcx
	movq	(%rcx), %rcx
	movq	8(%rcx), %rcx
	movq	-808(%rbp), %rdx
	movq	%rcx, -584(%rbp)
	movq	%rdx, -592(%rbp)
	movq	-592(%rbp), %rcx
	movq	-592(%rbp), %rdx
	subq	$1, %rdx
	andq	%rdx, %rcx
	cmpq	$0, %rcx
	movq	%rax, -992(%rbp)        ## 8-byte Spill
	jne	LBB35_33
## BB#32:
	movq	-584(%rbp), %rax
	movq	-592(%rbp), %rcx
	subq	$1, %rcx
	andq	%rcx, %rax
	movq	%rax, -1000(%rbp)       ## 8-byte Spill
	jmp	LBB35_34
LBB35_33:
	movq	-584(%rbp), %rax
	xorl	%ecx, %ecx
	movl	%ecx, %edx
	divq	-592(%rbp)
	movq	%rdx, -1000(%rbp)       ## 8-byte Spill
LBB35_34:                               ## %_ZNSt3__116__constrain_hashEmm.exit
	movq	-1000(%rbp), %rax       ## 8-byte Reload
	movq	-872(%rbp), %rcx        ## 8-byte Reload
	movq	%rcx, -616(%rbp)
	movq	%rax, -624(%rbp)
	movq	-616(%rbp), %rax
	movq	-624(%rbp), %rdx
	movq	%rax, -608(%rbp)
	movq	-608(%rbp), %rax
	movq	%rax, -600(%rbp)
	movq	-600(%rbp), %rax
	movq	(%rax), %rax
	movq	-992(%rbp), %rsi        ## 8-byte Reload
	movq	%rsi, (%rax,%rdx,8)
LBB35_35:
	jmp	LBB35_37
LBB35_36:
	movq	-856(%rbp), %rax
	movq	(%rax), %rax
	movq	-800(%rbp), %rcx
	movq	%rax, (%rcx)
	movq	-800(%rbp), %rax
	movq	-856(%rbp), %rcx
	movq	%rax, (%rcx)
LBB35_37:
	movq	-800(%rbp), %rax
	movq	%rax, -824(%rbp)
	movq	-872(%rbp), %rax        ## 8-byte Reload
	movq	%rax, -648(%rbp)
	movq	-648(%rbp), %rcx
	addq	$24, %rcx
	movq	%rcx, -640(%rbp)
	movq	-640(%rbp), %rcx
	movq	%rcx, -632(%rbp)
	movq	-632(%rbp), %rcx
	movq	(%rcx), %rdx
	addq	$1, %rdx
	movq	%rdx, (%rcx)
	movb	$1, -809(%rbp)
LBB35_38:
	movq	-824(%rbp), %rax
	leaq	-864(%rbp), %rcx
	movq	%rcx, -672(%rbp)
	movq	%rax, -680(%rbp)
	movq	-672(%rbp), %rdx
	movq	%rdx, -656(%rbp)
	movq	%rax, -664(%rbp)
	movq	-656(%rbp), %rdx
	movq	%rax, (%rdx)
	leaq	-784(%rbp), %rax
	movq	%rax, -728(%rbp)
	movq	%rcx, -736(%rbp)
	leaq	-809(%rbp), %rax
	movq	%rax, -744(%rbp)
	movq	-728(%rbp), %rcx
	movq	-736(%rbp), %rdx
	movq	%rcx, -704(%rbp)
	movq	%rdx, -712(%rbp)
	movq	%rax, -720(%rbp)
	movq	-704(%rbp), %rax
	movq	-712(%rbp), %rcx
	movq	%rcx, -696(%rbp)
	movq	(%rcx), %rcx
	movq	%rcx, (%rax)
	movq	-720(%rbp), %rcx
	movq	%rcx, -688(%rbp)
	movb	(%rcx), %sil
	andb	$1, %sil
	movb	%sil, 8(%rax)
	movq	-784(%rbp), %rax
	movb	-776(%rbp), %dl
	addq	$1008, %rsp             ## imm = 0x3F0
	popq	%rbp
	retq
	.cfi_endproc

	.globl	__ZNSt3__112__hash_tableINS_17__hash_value_typeIiPN5lunar12green_thread7contextEEENS_22__unordered_map_hasherIiS6_NS_4hashIiEELb1EEENS_21__unordered_map_equalIiS6_NS_8equal_toIiEELb1EEENS_9allocatorIS6_EEE4findIiEENS_15__hash_iteratorIPNS_11__hash_nodeIS6_PvEEEERKT_
	.weak_def_can_be_hidden	__ZNSt3__112__hash_tableINS_17__hash_value_typeIiPN5lunar12green_thread7contextEEENS_22__unordered_map_hasherIiS6_NS_4hashIiEELb1EEENS_21__unordered_map_equalIiS6_NS_8equal_toIiEELb1EEENS_9allocatorIS6_EEE4findIiEENS_15__hash_iteratorIPNS_11__hash_nodeIS6_PvEEEERKT_
	.align	4, 0x90
__ZNSt3__112__hash_tableINS_17__hash_value_typeIiPN5lunar12green_thread7contextEEENS_22__unordered_map_hasherIiS6_NS_4hashIiEELb1EEENS_21__unordered_map_equalIiS6_NS_8equal_toIiEELb1EEENS_9allocatorIS6_EEE4findIiEENS_15__hash_iteratorIPNS_11__hash_nodeIS6_PvEEEERKT_: ## @_ZNSt3__112__hash_tableINS_17__hash_value_typeIiPN5lunar12green_thread7contextEEENS_22__unordered_map_hasherIiS6_NS_4hashIiEELb1EEENS_21__unordered_map_equalIiS6_NS_8equal_toIiEELb1EEENS_9allocatorIS6_EEE4findIiEENS_15__hash_iteratorIPNS_11__hash_nodeIS6_PvEEEERKT_
	.cfi_startproc
## BB#0:
	pushq	%rbp
Ltmp130:
	.cfi_def_cfa_offset 16
Ltmp131:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
Ltmp132:
	.cfi_def_cfa_register %rbp
	subq	$288, %rsp              ## imm = 0x120
	movq	%rdi, -344(%rbp)
	movq	%rsi, -352(%rbp)
	movq	-344(%rbp), %rsi
	movq	%rsi, -328(%rbp)
	movq	-328(%rbp), %rdi
	addq	$24, %rdi
	movq	%rdi, -320(%rbp)
	movq	-320(%rbp), %rdi
	movq	%rdi, -312(%rbp)
	movq	-312(%rbp), %rdi
	movq	-352(%rbp), %rax
	movq	%rdi, -24(%rbp)
	movq	%rax, -32(%rbp)
	movq	-24(%rbp), %rax
	movq	-32(%rbp), %rdi
	movl	(%rdi), %ecx
	movq	%rax, -8(%rbp)
	movl	%ecx, -12(%rbp)
	movslq	-12(%rbp), %rax
	movq	%rax, -360(%rbp)
	movq	%rsi, -88(%rbp)
	movq	-88(%rbp), %rax
	movq	%rax, -80(%rbp)
	movq	-80(%rbp), %rax
	movq	%rax, -72(%rbp)
	movq	-72(%rbp), %rax
	movq	%rax, -64(%rbp)
	movq	-64(%rbp), %rax
	addq	$8, %rax
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	movq	%rax, -48(%rbp)
	movq	-48(%rbp), %rax
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movq	%rax, -368(%rbp)
	cmpq	$0, -368(%rbp)
	movq	%rsi, -392(%rbp)        ## 8-byte Spill
	je	LBB36_18
## BB#1:
	movq	-360(%rbp), %rax
	movq	-368(%rbp), %rcx
	movq	%rax, -96(%rbp)
	movq	%rcx, -104(%rbp)
	movq	-104(%rbp), %rax
	movq	-104(%rbp), %rcx
	subq	$1, %rcx
	andq	%rcx, %rax
	cmpq	$0, %rax
	jne	LBB36_3
## BB#2:
	movq	-96(%rbp), %rax
	movq	-104(%rbp), %rcx
	subq	$1, %rcx
	andq	%rcx, %rax
	movq	%rax, -400(%rbp)        ## 8-byte Spill
	jmp	LBB36_4
LBB36_3:
	movq	-96(%rbp), %rax
	xorl	%ecx, %ecx
	movl	%ecx, %edx
	divq	-104(%rbp)
	movq	%rdx, -400(%rbp)        ## 8-byte Spill
LBB36_4:                                ## %_ZNSt3__116__constrain_hashEmm.exit1
	movq	-400(%rbp), %rax        ## 8-byte Reload
	movq	%rax, -376(%rbp)
	movq	-376(%rbp), %rax
	movq	-392(%rbp), %rcx        ## 8-byte Reload
	movq	%rcx, -128(%rbp)
	movq	%rax, -136(%rbp)
	movq	-128(%rbp), %rax
	movq	-136(%rbp), %rdx
	movq	%rax, -120(%rbp)
	movq	-120(%rbp), %rax
	movq	%rax, -112(%rbp)
	movq	-112(%rbp), %rax
	movq	(%rax), %rax
	movq	(%rax,%rdx,8), %rax
	movq	%rax, -384(%rbp)
	cmpq	$0, -384(%rbp)
	je	LBB36_17
## BB#5:
	movq	-384(%rbp), %rax
	movq	(%rax), %rax
	movq	%rax, -384(%rbp)
LBB36_6:                                ## =>This Inner Loop Header: Depth=1
	xorl	%eax, %eax
	movb	%al, %cl
	cmpq	$0, -384(%rbp)
	movb	%cl, -401(%rbp)         ## 1-byte Spill
	je	LBB36_11
## BB#7:                                ##   in Loop: Header=BB36_6 Depth=1
	movq	-384(%rbp), %rax
	movq	8(%rax), %rax
	movq	-368(%rbp), %rcx
	movq	%rax, -144(%rbp)
	movq	%rcx, -152(%rbp)
	movq	-152(%rbp), %rax
	movq	-152(%rbp), %rcx
	subq	$1, %rcx
	andq	%rcx, %rax
	cmpq	$0, %rax
	jne	LBB36_9
## BB#8:                                ##   in Loop: Header=BB36_6 Depth=1
	movq	-144(%rbp), %rax
	movq	-152(%rbp), %rcx
	subq	$1, %rcx
	andq	%rcx, %rax
	movq	%rax, -416(%rbp)        ## 8-byte Spill
	jmp	LBB36_10
LBB36_9:                                ##   in Loop: Header=BB36_6 Depth=1
	movq	-144(%rbp), %rax
	xorl	%ecx, %ecx
	movl	%ecx, %edx
	divq	-152(%rbp)
	movq	%rdx, -416(%rbp)        ## 8-byte Spill
LBB36_10:                               ## %_ZNSt3__116__constrain_hashEmm.exit
                                        ##   in Loop: Header=BB36_6 Depth=1
	movq	-416(%rbp), %rax        ## 8-byte Reload
	cmpq	-376(%rbp), %rax
	sete	%cl
	movb	%cl, -401(%rbp)         ## 1-byte Spill
LBB36_11:                               ##   in Loop: Header=BB36_6 Depth=1
	movb	-401(%rbp), %al         ## 1-byte Reload
	testb	$1, %al
	jne	LBB36_12
	jmp	LBB36_16
LBB36_12:                               ##   in Loop: Header=BB36_6 Depth=1
	movq	-392(%rbp), %rax        ## 8-byte Reload
	movq	%rax, -176(%rbp)
	movq	-176(%rbp), %rcx
	addq	$32, %rcx
	movq	%rcx, -168(%rbp)
	movq	-168(%rbp), %rcx
	movq	%rcx, -160(%rbp)
	movq	-160(%rbp), %rcx
	movq	-384(%rbp), %rdx
	addq	$16, %rdx
	movq	-352(%rbp), %rsi
	movq	%rcx, -208(%rbp)
	movq	%rdx, -216(%rbp)
	movq	%rsi, -224(%rbp)
	movq	-208(%rbp), %rcx
	movq	-216(%rbp), %rdx
	movq	-224(%rbp), %rsi
	movq	%rcx, -184(%rbp)
	movq	%rdx, -192(%rbp)
	movq	%rsi, -200(%rbp)
	movq	-192(%rbp), %rcx
	movl	(%rcx), %edi
	movq	-200(%rbp), %rcx
	cmpl	(%rcx), %edi
	jne	LBB36_14
## BB#13:
	leaq	-336(%rbp), %rax
	movq	-384(%rbp), %rcx
	movq	%rax, -248(%rbp)
	movq	%rcx, -256(%rbp)
	movq	-248(%rbp), %rax
	movq	-256(%rbp), %rcx
	movq	%rax, -232(%rbp)
	movq	%rcx, -240(%rbp)
	movq	-232(%rbp), %rax
	movq	-240(%rbp), %rcx
	movq	%rcx, (%rax)
	jmp	LBB36_19
LBB36_14:                               ##   in Loop: Header=BB36_6 Depth=1
	jmp	LBB36_15
LBB36_15:                               ##   in Loop: Header=BB36_6 Depth=1
	movq	-384(%rbp), %rax
	movq	(%rax), %rax
	movq	%rax, -384(%rbp)
	jmp	LBB36_6
LBB36_16:
	jmp	LBB36_17
LBB36_17:
	jmp	LBB36_18
LBB36_18:
	leaq	-296(%rbp), %rax
	movq	-392(%rbp), %rcx        ## 8-byte Reload
	movq	%rcx, -304(%rbp)
	movq	%rax, -280(%rbp)
	movq	$0, -288(%rbp)
	movq	-280(%rbp), %rax
	movq	-288(%rbp), %rdx
	movq	%rax, -264(%rbp)
	movq	%rdx, -272(%rbp)
	movq	-264(%rbp), %rax
	movq	-272(%rbp), %rdx
	movq	%rdx, (%rax)
	movq	-296(%rbp), %rax
	movq	%rax, -336(%rbp)
LBB36_19:
	movq	-336(%rbp), %rax
	addq	$288, %rsp              ## imm = 0x120
	popq	%rbp
	retq
	.cfi_endproc

	.section	__TEXT,__literal4,4byte_literals
	.align	2
LCPI37_0:
	.long	1593835520              ## float 9.22337203E+18
	.section	__TEXT,__textcoal_nt,coalesced,pure_instructions
	.globl	__ZNSt3__112__hash_tableINS_17__hash_value_typeIiPN5lunar12green_thread7contextEEENS_22__unordered_map_hasherIiS6_NS_4hashIiEELb1EEENS_21__unordered_map_equalIiS6_NS_8equal_toIiEELb1EEENS_9allocatorIS6_EEE6rehashEm
	.weak_def_can_be_hidden	__ZNSt3__112__hash_tableINS_17__hash_value_typeIiPN5lunar12green_thread7contextEEENS_22__unordered_map_hasherIiS6_NS_4hashIiEELb1EEENS_21__unordered_map_equalIiS6_NS_8equal_toIiEELb1EEENS_9allocatorIS6_EEE6rehashEm
	.align	4, 0x90
__ZNSt3__112__hash_tableINS_17__hash_value_typeIiPN5lunar12green_thread7contextEEENS_22__unordered_map_hasherIiS6_NS_4hashIiEELb1EEENS_21__unordered_map_equalIiS6_NS_8equal_toIiEELb1EEENS_9allocatorIS6_EEE6rehashEm: ## @_ZNSt3__112__hash_tableINS_17__hash_value_typeIiPN5lunar12green_thread7contextEEENS_22__unordered_map_hasherIiS6_NS_4hashIiEELb1EEENS_21__unordered_map_equalIiS6_NS_8equal_toIiEELb1EEENS_9allocatorIS6_EEE6rehashEm
	.cfi_startproc
## BB#0:
	pushq	%rbp
Ltmp133:
	.cfi_def_cfa_offset 16
Ltmp134:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
Ltmp135:
	.cfi_def_cfa_register %rbp
	subq	$368, %rsp              ## imm = 0x170
	movq	%rdi, -272(%rbp)
	movq	%rsi, -280(%rbp)
	movq	-272(%rbp), %rsi
	cmpq	$1, -280(%rbp)
	movq	%rsi, -304(%rbp)        ## 8-byte Spill
	jne	LBB37_2
## BB#1:
	movq	$2, -280(%rbp)
	jmp	LBB37_5
LBB37_2:
	movq	-280(%rbp), %rax
	movq	-280(%rbp), %rcx
	subq	$1, %rcx
	andq	%rcx, %rax
	cmpq	$0, %rax
	je	LBB37_4
## BB#3:
	movq	-280(%rbp), %rdi
	callq	__ZNSt3__112__next_primeEm
	movq	%rax, -280(%rbp)
LBB37_4:
	jmp	LBB37_5
LBB37_5:
	movq	-304(%rbp), %rax        ## 8-byte Reload
	movq	%rax, -264(%rbp)
	movq	-264(%rbp), %rcx
	movq	%rcx, -256(%rbp)
	movq	-256(%rbp), %rcx
	movq	%rcx, -248(%rbp)
	movq	-248(%rbp), %rcx
	movq	%rcx, -240(%rbp)
	movq	-240(%rbp), %rcx
	addq	$8, %rcx
	movq	%rcx, -232(%rbp)
	movq	-232(%rbp), %rcx
	movq	%rcx, -224(%rbp)
	movq	-224(%rbp), %rcx
	movq	%rcx, -216(%rbp)
	movq	-216(%rbp), %rcx
	movq	(%rcx), %rcx
	movq	%rcx, -288(%rbp)
	movq	-280(%rbp), %rcx
	cmpq	-288(%rbp), %rcx
	jbe	LBB37_7
## BB#6:
	movq	-280(%rbp), %rsi
	movq	-304(%rbp), %rdi        ## 8-byte Reload
	callq	__ZNSt3__112__hash_tableINS_17__hash_value_typeIiPN5lunar12green_thread7contextEEENS_22__unordered_map_hasherIiS6_NS_4hashIiEELb1EEENS_21__unordered_map_equalIiS6_NS_8equal_toIiEELb1EEENS_9allocatorIS6_EEE8__rehashEm
	jmp	LBB37_20
LBB37_7:
	movq	-280(%rbp), %rax
	cmpq	-288(%rbp), %rax
	jae	LBB37_19
## BB#8:
	xorl	%eax, %eax
	movb	%al, %cl
	movq	-288(%rbp), %rdx
	movq	%rdx, -208(%rbp)
	cmpq	$2, -208(%rbp)
	movb	%cl, -305(%rbp)         ## 1-byte Spill
	jbe	LBB37_10
## BB#9:
	movq	-208(%rbp), %rax
	movq	-208(%rbp), %rcx
	subq	$1, %rcx
	andq	%rcx, %rax
	cmpq	$0, %rax
	setne	%dl
	xorb	$1, %dl
	movb	%dl, -305(%rbp)         ## 1-byte Spill
LBB37_10:                               ## %_ZNSt3__116__is_hash_power2Em.exit
	movb	-305(%rbp), %al         ## 1-byte Reload
	testb	$1, %al
	jne	LBB37_11
	jmp	LBB37_12
LBB37_11:
	movl	$1, %eax
	movl	%eax, %ecx
	movl	$64, %eax
	movl	%eax, %edx
	movq	-304(%rbp), %rsi        ## 8-byte Reload
	movq	%rsi, -120(%rbp)
	addq	$24, %rsi
	movq	%rsi, -112(%rbp)
	movq	%rsi, -104(%rbp)
	movq	-304(%rbp), %rsi        ## 8-byte Reload
	movq	24(%rsi), %rdi
	movq	%rdi, %r8
	shrq	%r8
	movq	%rdi, %r9
	andq	$1, %r9
	orq	%r8, %r9
	cvtsi2ssq	%r9, %xmm0
	addss	%xmm0, %xmm0
	cvtsi2ssq	%rdi, %xmm1
	testq	%rdi, %rdi
	movq	%rcx, -320(%rbp)        ## 8-byte Spill
	movq	%rdx, -328(%rbp)        ## 8-byte Spill
	movss	%xmm1, -332(%rbp)       ## 4-byte Spill
	movss	%xmm0, -336(%rbp)       ## 4-byte Spill
	js	LBB37_22
## BB#21:
	movss	-332(%rbp), %xmm0       ## 4-byte Reload
                                        ## xmm0 = mem[0],zero,zero,zero
	movss	%xmm0, -336(%rbp)       ## 4-byte Spill
LBB37_22:
	movss	-336(%rbp), %xmm0       ## 4-byte Reload
                                        ## xmm0 = mem[0],zero,zero,zero
	movq	-304(%rbp), %rax        ## 8-byte Reload
	movq	%rax, -24(%rbp)
	addq	$32, %rax
	movq	%rax, -16(%rbp)
	movq	%rax, -8(%rbp)
	movq	-304(%rbp), %rax        ## 8-byte Reload
	movss	32(%rax), %xmm1         ## xmm1 = mem[0],zero,zero,zero
	divss	%xmm1, %xmm0
	movss	%xmm0, -28(%rbp)
	callq	_ceilf
	movss	LCPI37_0(%rip), %xmm1   ## xmm1 = mem[0],zero,zero,zero
	movaps	%xmm0, %xmm2
	subss	%xmm1, %xmm2
	cvttss2si	%xmm2, %rax
	movabsq	$-9223372036854775808, %rcx ## imm = 0x8000000000000000
	xorq	%rcx, %rax
	cvttss2si	%xmm0, %rcx
	ucomiss	%xmm1, %xmm0
	cmovbq	%rcx, %rax
	movq	%rax, -48(%rbp)
	movq	-48(%rbp), %rax
	subq	$1, %rax
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rax
	bsrq	%rax, %rax
	xorq	$63, %rax
	movl	%eax, %edx
	movslq	%edx, %rax
	movq	-328(%rbp), %rcx        ## 8-byte Reload
	subq	%rax, %rcx
                                        ## kill: CL<def> RCX<kill>
	movq	-320(%rbp), %rax        ## 8-byte Reload
	shlq	%cl, %rax
	movq	%rax, -344(%rbp)        ## 8-byte Spill
	jmp	LBB37_13
LBB37_12:
	movq	-304(%rbp), %rax        ## 8-byte Reload
	movq	%rax, -72(%rbp)
	addq	$24, %rax
	movq	%rax, -64(%rbp)
	movq	%rax, -56(%rbp)
	movq	-304(%rbp), %rax        ## 8-byte Reload
	movq	24(%rax), %rcx
	movq	%rcx, %rdx
	shrq	%rdx
	movq	%rcx, %rsi
	andq	$1, %rsi
	orq	%rdx, %rsi
	cvtsi2ssq	%rsi, %xmm0
	addss	%xmm0, %xmm0
	cvtsi2ssq	%rcx, %xmm1
	testq	%rcx, %rcx
	movss	%xmm1, -348(%rbp)       ## 4-byte Spill
	movss	%xmm0, -352(%rbp)       ## 4-byte Spill
	js	LBB37_24
## BB#23:
	movss	-348(%rbp), %xmm0       ## 4-byte Reload
                                        ## xmm0 = mem[0],zero,zero,zero
	movss	%xmm0, -352(%rbp)       ## 4-byte Spill
LBB37_24:
	movss	-352(%rbp), %xmm0       ## 4-byte Reload
                                        ## xmm0 = mem[0],zero,zero,zero
	movq	-304(%rbp), %rax        ## 8-byte Reload
	movq	%rax, -96(%rbp)
	addq	$32, %rax
	movq	%rax, -88(%rbp)
	movq	%rax, -80(%rbp)
	movq	-304(%rbp), %rax        ## 8-byte Reload
	movss	32(%rax), %xmm1         ## xmm1 = mem[0],zero,zero,zero
	divss	%xmm1, %xmm0
	movss	%xmm0, -124(%rbp)
	callq	_ceilf
	movss	LCPI37_0(%rip), %xmm1   ## xmm1 = mem[0],zero,zero,zero
	movaps	%xmm0, %xmm2
	subss	%xmm1, %xmm2
	cvttss2si	%xmm2, %rax
	movabsq	$-9223372036854775808, %rcx ## imm = 0x8000000000000000
	xorq	%rcx, %rax
	cvttss2si	%xmm0, %rcx
	ucomiss	%xmm1, %xmm0
	cmovbq	%rcx, %rax
	movq	%rax, %rdi
	callq	__ZNSt3__112__next_primeEm
	movq	%rax, -344(%rbp)        ## 8-byte Spill
LBB37_13:
	movq	-344(%rbp), %rax        ## 8-byte Reload
	leaq	-160(%rbp), %rcx
	leaq	-296(%rbp), %rdx
	leaq	-280(%rbp), %rsi
	movq	%rax, -296(%rbp)
	movq	%rsi, -184(%rbp)
	movq	%rdx, -192(%rbp)
	movq	-184(%rbp), %rax
	movq	-192(%rbp), %rdx
	movq	%rax, -168(%rbp)
	movq	%rdx, -176(%rbp)
	movq	-168(%rbp), %rax
	movq	-176(%rbp), %rdx
	movq	%rcx, -136(%rbp)
	movq	%rax, -144(%rbp)
	movq	%rdx, -152(%rbp)
	movq	-144(%rbp), %rax
	movq	(%rax), %rax
	movq	-152(%rbp), %rcx
	cmpq	(%rcx), %rax
	jae	LBB37_15
## BB#14:
	movq	-176(%rbp), %rax
	movq	%rax, -360(%rbp)        ## 8-byte Spill
	jmp	LBB37_16
LBB37_15:
	movq	-168(%rbp), %rax
	movq	%rax, -360(%rbp)        ## 8-byte Spill
LBB37_16:                               ## %_ZNSt3__13maxImEERKT_S3_S3_.exit
	movq	-360(%rbp), %rax        ## 8-byte Reload
	movq	(%rax), %rax
	movq	%rax, -280(%rbp)
	movq	-280(%rbp), %rax
	cmpq	-288(%rbp), %rax
	jae	LBB37_18
## BB#17:
	movq	-280(%rbp), %rsi
	movq	-304(%rbp), %rdi        ## 8-byte Reload
	callq	__ZNSt3__112__hash_tableINS_17__hash_value_typeIiPN5lunar12green_thread7contextEEENS_22__unordered_map_hasherIiS6_NS_4hashIiEELb1EEENS_21__unordered_map_equalIiS6_NS_8equal_toIiEELb1EEENS_9allocatorIS6_EEE8__rehashEm
LBB37_18:
	jmp	LBB37_19
LBB37_19:
	jmp	LBB37_20
LBB37_20:
	addq	$368, %rsp              ## imm = 0x170
	popq	%rbp
	retq
	.cfi_endproc

	.globl	__ZNSt3__112__hash_tableINS_17__hash_value_typeIiPN5lunar12green_thread7contextEEENS_22__unordered_map_hasherIiS6_NS_4hashIiEELb1EEENS_21__unordered_map_equalIiS6_NS_8equal_toIiEELb1EEENS_9allocatorIS6_EEE8__rehashEm
	.weak_def_can_be_hidden	__ZNSt3__112__hash_tableINS_17__hash_value_typeIiPN5lunar12green_thread7contextEEENS_22__unordered_map_hasherIiS6_NS_4hashIiEELb1EEENS_21__unordered_map_equalIiS6_NS_8equal_toIiEELb1EEENS_9allocatorIS6_EEE8__rehashEm
	.align	4, 0x90
__ZNSt3__112__hash_tableINS_17__hash_value_typeIiPN5lunar12green_thread7contextEEENS_22__unordered_map_hasherIiS6_NS_4hashIiEELb1EEENS_21__unordered_map_equalIiS6_NS_8equal_toIiEELb1EEENS_9allocatorIS6_EEE8__rehashEm: ## @_ZNSt3__112__hash_tableINS_17__hash_value_typeIiPN5lunar12green_thread7contextEEENS_22__unordered_map_hasherIiS6_NS_4hashIiEELb1EEENS_21__unordered_map_equalIiS6_NS_8equal_toIiEELb1EEENS_9allocatorIS6_EEE8__rehashEm
	.cfi_startproc
## BB#0:
	pushq	%rbp
Ltmp136:
	.cfi_def_cfa_offset 16
Ltmp137:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
Ltmp138:
	.cfi_def_cfa_register %rbp
	subq	$784, %rsp              ## imm = 0x310
	movq	%rdi, -672(%rbp)
	movq	%rsi, -680(%rbp)
	movq	-672(%rbp), %rsi
	movq	%rsi, -664(%rbp)
	movq	-664(%rbp), %rdi
	movq	%rdi, -656(%rbp)
	movq	-656(%rbp), %rdi
	movq	%rdi, -648(%rbp)
	movq	-648(%rbp), %rdi
	addq	$8, %rdi
	movq	%rdi, -72(%rbp)
	movq	-72(%rbp), %rdi
	movq	%rdi, -64(%rbp)
	movq	-64(%rbp), %rdi
	movq	%rdi, -56(%rbp)
	movq	-56(%rbp), %rdi
	movq	%rdi, -688(%rbp)
	cmpq	$0, -680(%rbp)
	movq	%rsi, -744(%rbp)        ## 8-byte Spill
	jbe	LBB38_2
## BB#1:
	movq	-688(%rbp), %rax
	movq	-680(%rbp), %rcx
	movq	%rax, -40(%rbp)
	movq	%rcx, -48(%rbp)
	movq	-40(%rbp), %rax
	movq	-48(%rbp), %rcx
	movq	%rax, -16(%rbp)
	movq	%rcx, -24(%rbp)
	movq	$0, -32(%rbp)
	movq	-24(%rbp), %rax
	shlq	$3, %rax
	movq	%rax, -8(%rbp)
	movq	-8(%rbp), %rdi
	callq	__Znwm
	movq	%rax, -752(%rbp)        ## 8-byte Spill
	jmp	LBB38_3
LBB38_2:
	xorl	%eax, %eax
	movl	%eax, %ecx
	movq	%rcx, -752(%rbp)        ## 8-byte Spill
	jmp	LBB38_3
LBB38_3:
	movq	-752(%rbp), %rax        ## 8-byte Reload
	movq	-744(%rbp), %rcx        ## 8-byte Reload
	movq	%rcx, -248(%rbp)
	movq	%rax, -256(%rbp)
	movq	-248(%rbp), %rax
	movq	%rax, -240(%rbp)
	movq	-240(%rbp), %rdx
	movq	%rdx, -232(%rbp)
	movq	-232(%rbp), %rdx
	movq	(%rdx), %rdx
	movq	%rdx, -264(%rbp)
	movq	-256(%rbp), %rdx
	movq	%rax, -104(%rbp)
	movq	-104(%rbp), %rsi
	movq	%rsi, -96(%rbp)
	movq	-96(%rbp), %rsi
	movq	%rdx, (%rsi)
	cmpq	$0, -264(%rbp)
	movq	%rax, -760(%rbp)        ## 8-byte Spill
	je	LBB38_5
## BB#4:
	movq	-760(%rbp), %rax        ## 8-byte Reload
	movq	%rax, -88(%rbp)
	movq	-88(%rbp), %rcx
	movq	%rcx, -80(%rbp)
	movq	-80(%rbp), %rcx
	addq	$8, %rcx
	movq	-264(%rbp), %rdx
	movq	%rcx, -216(%rbp)
	movq	%rdx, -224(%rbp)
	movq	-216(%rbp), %rcx
	movq	%rcx, -208(%rbp)
	movq	-208(%rbp), %rdx
	movq	%rdx, -200(%rbp)
	movq	-200(%rbp), %rdx
	movq	%rdx, -192(%rbp)
	movq	-192(%rbp), %rdx
	movq	-224(%rbp), %rsi
	movq	%rcx, -128(%rbp)
	movq	-128(%rbp), %rcx
	movq	%rcx, -120(%rbp)
	movq	-120(%rbp), %rcx
	movq	%rcx, -112(%rbp)
	movq	-112(%rbp), %rcx
	movq	(%rcx), %rcx
	movq	%rdx, -168(%rbp)
	movq	%rsi, -176(%rbp)
	movq	%rcx, -184(%rbp)
	movq	-168(%rbp), %rcx
	movq	-176(%rbp), %rdx
	movq	-184(%rbp), %rsi
	movq	%rcx, -144(%rbp)
	movq	%rdx, -152(%rbp)
	movq	%rsi, -160(%rbp)
	movq	-152(%rbp), %rcx
	movq	%rcx, -136(%rbp)
	movq	-136(%rbp), %rdi
	callq	__ZdlPv
LBB38_5:                                ## %_ZNSt3__110unique_ptrIA_PNS_11__hash_nodeINS_17__hash_value_typeIiPN5lunar12green_thread7contextEEEPvEENS_25__bucket_list_deallocatorINS_9allocatorISA_EEEEE5resetIPSA_EENS_9enable_ifIXsr27__same_or_less_cv_qualifiedIT_SI_EE5valueEvE4typeESK_.exit
	movq	-680(%rbp), %rax
	movq	-744(%rbp), %rcx        ## 8-byte Reload
	movq	%rcx, -288(%rbp)
	movq	-288(%rbp), %rdx
	movq	%rdx, -280(%rbp)
	movq	-280(%rbp), %rdx
	movq	%rdx, -272(%rbp)
	movq	-272(%rbp), %rdx
	addq	$8, %rdx
	movq	%rdx, -312(%rbp)
	movq	-312(%rbp), %rdx
	movq	%rdx, -304(%rbp)
	movq	-304(%rbp), %rdx
	movq	%rdx, -296(%rbp)
	movq	-296(%rbp), %rdx
	movq	%rax, (%rdx)
	cmpq	$0, -680(%rbp)
	jbe	LBB38_35
## BB#6:
	movq	$0, -696(%rbp)
LBB38_7:                                ## =>This Inner Loop Header: Depth=1
	movq	-696(%rbp), %rax
	cmpq	-680(%rbp), %rax
	jae	LBB38_10
## BB#8:                                ##   in Loop: Header=BB38_7 Depth=1
	movq	-696(%rbp), %rax
	movq	-744(%rbp), %rcx        ## 8-byte Reload
	movq	%rcx, -336(%rbp)
	movq	%rax, -344(%rbp)
	movq	-336(%rbp), %rax
	movq	-344(%rbp), %rdx
	movq	%rax, -328(%rbp)
	movq	-328(%rbp), %rax
	movq	%rax, -320(%rbp)
	movq	-320(%rbp), %rax
	movq	(%rax), %rax
	movq	$0, (%rax,%rdx,8)
## BB#9:                                ##   in Loop: Header=BB38_7 Depth=1
	movq	-696(%rbp), %rax
	addq	$1, %rax
	movq	%rax, -696(%rbp)
	jmp	LBB38_7
LBB38_10:
	movq	-744(%rbp), %rax        ## 8-byte Reload
	addq	$16, %rax
	movq	%rax, -360(%rbp)
	movq	-360(%rbp), %rax
	movq	%rax, -352(%rbp)
	movq	-352(%rbp), %rax
	movq	%rax, -376(%rbp)
	movq	-376(%rbp), %rax
	movq	%rax, -368(%rbp)
	movq	-368(%rbp), %rax
	movq	%rax, -704(%rbp)
	movq	-704(%rbp), %rax
	movq	(%rax), %rax
	movq	%rax, -712(%rbp)
	cmpq	$0, -712(%rbp)
	je	LBB38_34
## BB#11:
	movq	-712(%rbp), %rax
	movq	8(%rax), %rax
	movq	-680(%rbp), %rcx
	movq	%rax, -384(%rbp)
	movq	%rcx, -392(%rbp)
	movq	-392(%rbp), %rax
	movq	-392(%rbp), %rcx
	subq	$1, %rcx
	andq	%rcx, %rax
	cmpq	$0, %rax
	jne	LBB38_13
## BB#12:
	movq	-384(%rbp), %rax
	movq	-392(%rbp), %rcx
	subq	$1, %rcx
	andq	%rcx, %rax
	movq	%rax, -768(%rbp)        ## 8-byte Spill
	jmp	LBB38_14
LBB38_13:
	movq	-384(%rbp), %rax
	xorl	%ecx, %ecx
	movl	%ecx, %edx
	divq	-392(%rbp)
	movq	%rdx, -768(%rbp)        ## 8-byte Spill
LBB38_14:                               ## %_ZNSt3__116__constrain_hashEmm.exit1
	movq	-768(%rbp), %rax        ## 8-byte Reload
	movq	%rax, -720(%rbp)
	movq	-704(%rbp), %rax
	movq	-720(%rbp), %rcx
	movq	-744(%rbp), %rdx        ## 8-byte Reload
	movq	%rdx, -416(%rbp)
	movq	%rcx, -424(%rbp)
	movq	-416(%rbp), %rcx
	movq	-424(%rbp), %rsi
	movq	%rcx, -408(%rbp)
	movq	-408(%rbp), %rcx
	movq	%rcx, -400(%rbp)
	movq	-400(%rbp), %rcx
	movq	(%rcx), %rcx
	movq	%rax, (%rcx,%rsi,8)
	movq	-720(%rbp), %rax
	movq	%rax, -728(%rbp)
	movq	-712(%rbp), %rax
	movq	%rax, -704(%rbp)
	movq	-712(%rbp), %rax
	movq	(%rax), %rax
	movq	%rax, -712(%rbp)
LBB38_15:                               ## =>This Loop Header: Depth=1
                                        ##     Child Loop BB38_24 Depth 2
	cmpq	$0, -712(%rbp)
	je	LBB38_33
## BB#16:                               ##   in Loop: Header=BB38_15 Depth=1
	movq	-712(%rbp), %rax
	movq	8(%rax), %rax
	movq	-680(%rbp), %rcx
	movq	%rax, -432(%rbp)
	movq	%rcx, -440(%rbp)
	movq	-440(%rbp), %rax
	movq	-440(%rbp), %rcx
	subq	$1, %rcx
	andq	%rcx, %rax
	cmpq	$0, %rax
	jne	LBB38_18
## BB#17:                               ##   in Loop: Header=BB38_15 Depth=1
	movq	-432(%rbp), %rax
	movq	-440(%rbp), %rcx
	subq	$1, %rcx
	andq	%rcx, %rax
	movq	%rax, -776(%rbp)        ## 8-byte Spill
	jmp	LBB38_19
LBB38_18:                               ##   in Loop: Header=BB38_15 Depth=1
	movq	-432(%rbp), %rax
	xorl	%ecx, %ecx
	movl	%ecx, %edx
	divq	-440(%rbp)
	movq	%rdx, -776(%rbp)        ## 8-byte Spill
LBB38_19:                               ## %_ZNSt3__116__constrain_hashEmm.exit
                                        ##   in Loop: Header=BB38_15 Depth=1
	movq	-776(%rbp), %rax        ## 8-byte Reload
	movq	%rax, -720(%rbp)
	movq	-720(%rbp), %rax
	cmpq	-728(%rbp), %rax
	jne	LBB38_21
## BB#20:                               ##   in Loop: Header=BB38_15 Depth=1
	movq	-712(%rbp), %rax
	movq	%rax, -704(%rbp)
	jmp	LBB38_31
LBB38_21:                               ##   in Loop: Header=BB38_15 Depth=1
	movq	-720(%rbp), %rax
	movq	-744(%rbp), %rcx        ## 8-byte Reload
	movq	%rcx, -464(%rbp)
	movq	%rax, -472(%rbp)
	movq	-464(%rbp), %rax
	movq	-472(%rbp), %rdx
	movq	%rax, -456(%rbp)
	movq	-456(%rbp), %rax
	movq	%rax, -448(%rbp)
	movq	-448(%rbp), %rax
	movq	(%rax), %rax
	cmpq	$0, (%rax,%rdx,8)
	jne	LBB38_23
## BB#22:                               ##   in Loop: Header=BB38_15 Depth=1
	movq	-704(%rbp), %rax
	movq	-720(%rbp), %rcx
	movq	-744(%rbp), %rdx        ## 8-byte Reload
	movq	%rdx, -496(%rbp)
	movq	%rcx, -504(%rbp)
	movq	-496(%rbp), %rcx
	movq	-504(%rbp), %rsi
	movq	%rcx, -488(%rbp)
	movq	-488(%rbp), %rcx
	movq	%rcx, -480(%rbp)
	movq	-480(%rbp), %rcx
	movq	(%rcx), %rcx
	movq	%rax, (%rcx,%rsi,8)
	movq	-712(%rbp), %rax
	movq	%rax, -704(%rbp)
	movq	-720(%rbp), %rax
	movq	%rax, -728(%rbp)
	jmp	LBB38_30
LBB38_23:                               ##   in Loop: Header=BB38_15 Depth=1
	movq	-712(%rbp), %rax
	movq	%rax, -736(%rbp)
LBB38_24:                               ##   Parent Loop BB38_15 Depth=1
                                        ## =>  This Inner Loop Header: Depth=2
	xorl	%eax, %eax
	movb	%al, %cl
	movq	-736(%rbp), %rdx
	cmpq	$0, (%rdx)
	movb	%cl, -777(%rbp)         ## 1-byte Spill
	je	LBB38_26
## BB#25:                               ##   in Loop: Header=BB38_24 Depth=2
	movq	-744(%rbp), %rax        ## 8-byte Reload
	movq	%rax, -528(%rbp)
	movq	-528(%rbp), %rcx
	addq	$32, %rcx
	movq	%rcx, -520(%rbp)
	movq	-520(%rbp), %rcx
	movq	%rcx, -512(%rbp)
	movq	-512(%rbp), %rcx
	movq	-712(%rbp), %rdx
	addq	$16, %rdx
	movq	-736(%rbp), %rsi
	movq	(%rsi), %rsi
	addq	$16, %rsi
	movq	%rcx, -560(%rbp)
	movq	%rdx, -568(%rbp)
	movq	%rsi, -576(%rbp)
	movq	-560(%rbp), %rcx
	movq	-568(%rbp), %rdx
	movq	-576(%rbp), %rsi
	movq	%rcx, -536(%rbp)
	movq	%rdx, -544(%rbp)
	movq	%rsi, -552(%rbp)
	movq	-544(%rbp), %rcx
	movl	(%rcx), %edi
	movq	-552(%rbp), %rcx
	cmpl	(%rcx), %edi
	sete	%r8b
	movb	%r8b, -777(%rbp)        ## 1-byte Spill
LBB38_26:                               ##   in Loop: Header=BB38_24 Depth=2
	movb	-777(%rbp), %al         ## 1-byte Reload
	testb	$1, %al
	jne	LBB38_27
	jmp	LBB38_29
LBB38_27:                               ##   in Loop: Header=BB38_24 Depth=2
	jmp	LBB38_28
LBB38_28:                               ##   in Loop: Header=BB38_24 Depth=2
	movq	-736(%rbp), %rax
	movq	(%rax), %rax
	movq	%rax, -736(%rbp)
	jmp	LBB38_24
LBB38_29:                               ##   in Loop: Header=BB38_15 Depth=1
	movq	-736(%rbp), %rax
	movq	(%rax), %rax
	movq	-704(%rbp), %rcx
	movq	%rax, (%rcx)
	movq	-720(%rbp), %rax
	movq	-744(%rbp), %rcx        ## 8-byte Reload
	movq	%rcx, -600(%rbp)
	movq	%rax, -608(%rbp)
	movq	-600(%rbp), %rax
	movq	-608(%rbp), %rdx
	movq	%rax, -592(%rbp)
	movq	-592(%rbp), %rax
	movq	%rax, -584(%rbp)
	movq	-584(%rbp), %rax
	movq	(%rax), %rax
	movq	(%rax,%rdx,8), %rax
	movq	(%rax), %rax
	movq	-736(%rbp), %rdx
	movq	%rax, (%rdx)
	movq	-712(%rbp), %rax
	movq	-720(%rbp), %rdx
	movq	%rcx, -632(%rbp)
	movq	%rdx, -640(%rbp)
	movq	-632(%rbp), %rdx
	movq	-640(%rbp), %rsi
	movq	%rdx, -624(%rbp)
	movq	-624(%rbp), %rdx
	movq	%rdx, -616(%rbp)
	movq	-616(%rbp), %rdx
	movq	(%rdx), %rdx
	movq	(%rdx,%rsi,8), %rdx
	movq	%rax, (%rdx)
LBB38_30:                               ##   in Loop: Header=BB38_15 Depth=1
	jmp	LBB38_31
LBB38_31:                               ##   in Loop: Header=BB38_15 Depth=1
	jmp	LBB38_32
LBB38_32:                               ##   in Loop: Header=BB38_15 Depth=1
	movq	-704(%rbp), %rax
	movq	(%rax), %rax
	movq	%rax, -712(%rbp)
	jmp	LBB38_15
LBB38_33:
	jmp	LBB38_34
LBB38_34:
	jmp	LBB38_35
LBB38_35:
	addq	$784, %rsp              ## imm = 0x310
	popq	%rbp
	retq
	.cfi_endproc

	.globl	__ZNSt3__112__hash_tableINS_17__hash_value_typeIiPN5lunar12green_thread7contextEEENS_22__unordered_map_hasherIiS6_NS_4hashIiEELb1EEENS_21__unordered_map_equalIiS6_NS_8equal_toIiEELb1EEENS_9allocatorIS6_EEE14__erase_uniqueIiEEmRKT_
	.weak_def_can_be_hidden	__ZNSt3__112__hash_tableINS_17__hash_value_typeIiPN5lunar12green_thread7contextEEENS_22__unordered_map_hasherIiS6_NS_4hashIiEELb1EEENS_21__unordered_map_equalIiS6_NS_8equal_toIiEELb1EEENS_9allocatorIS6_EEE14__erase_uniqueIiEEmRKT_
	.align	4, 0x90
__ZNSt3__112__hash_tableINS_17__hash_value_typeIiPN5lunar12green_thread7contextEEENS_22__unordered_map_hasherIiS6_NS_4hashIiEELb1EEENS_21__unordered_map_equalIiS6_NS_8equal_toIiEELb1EEENS_9allocatorIS6_EEE14__erase_uniqueIiEEmRKT_: ## @_ZNSt3__112__hash_tableINS_17__hash_value_typeIiPN5lunar12green_thread7contextEEENS_22__unordered_map_hasherIiS6_NS_4hashIiEELb1EEENS_21__unordered_map_equalIiS6_NS_8equal_toIiEELb1EEENS_9allocatorIS6_EEE14__erase_uniqueIiEEmRKT_
	.cfi_startproc
## BB#0:
	pushq	%rbp
Ltmp139:
	.cfi_def_cfa_offset 16
Ltmp140:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
Ltmp141:
	.cfi_def_cfa_register %rbp
	subq	$176, %rsp
	movq	%rdi, -112(%rbp)
	movq	%rsi, -120(%rbp)
	movq	-112(%rbp), %rsi
	movq	-120(%rbp), %rdi
	movq	%rdi, -160(%rbp)        ## 8-byte Spill
	movq	%rsi, %rdi
	movq	-160(%rbp), %rax        ## 8-byte Reload
	movq	%rsi, -168(%rbp)        ## 8-byte Spill
	movq	%rax, %rsi
	callq	__ZNSt3__112__hash_tableINS_17__hash_value_typeIiPN5lunar12green_thread7contextEEENS_22__unordered_map_hasherIiS6_NS_4hashIiEELb1EEENS_21__unordered_map_equalIiS6_NS_8equal_toIiEELb1EEENS_9allocatorIS6_EEE4findIiEENS_15__hash_iteratorIPNS_11__hash_nodeIS6_PvEEEERKT_
	leaq	-136(%rbp), %rsi
	leaq	-128(%rbp), %rdi
	leaq	-88(%rbp), %rcx
	movq	%rax, -128(%rbp)
	movq	-168(%rbp), %rax        ## 8-byte Reload
	movq	%rax, -96(%rbp)
	movq	%rcx, -72(%rbp)
	movq	$0, -80(%rbp)
	movq	-72(%rbp), %rcx
	movq	-80(%rbp), %rdx
	movq	%rcx, -56(%rbp)
	movq	%rdx, -64(%rbp)
	movq	-56(%rbp), %rcx
	movq	-64(%rbp), %rdx
	movq	%rdx, (%rcx)
	movq	-88(%rbp), %rcx
	movq	%rcx, -136(%rbp)
	movq	%rdi, -40(%rbp)
	movq	%rsi, -48(%rbp)
	movq	-40(%rbp), %rcx
	movq	(%rcx), %rcx
	movq	-48(%rbp), %rdx
	cmpq	(%rdx), %rcx
	jne	LBB39_2
## BB#1:
	movq	$0, -104(%rbp)
	jmp	LBB39_3
LBB39_2:
	leaq	-128(%rbp), %rax
	leaq	-144(%rbp), %rcx
	movq	%rcx, -24(%rbp)
	movq	%rax, -32(%rbp)
	movq	-24(%rbp), %rax
	movq	-32(%rbp), %rcx
	movq	%rax, -8(%rbp)
	movq	%rcx, -16(%rbp)
	movq	-8(%rbp), %rax
	movq	-16(%rbp), %rcx
	movq	(%rcx), %rcx
	movq	%rcx, (%rax)
	movq	-144(%rbp), %rsi
	movq	-168(%rbp), %rdi        ## 8-byte Reload
	callq	__ZNSt3__112__hash_tableINS_17__hash_value_typeIiPN5lunar12green_thread7contextEEENS_22__unordered_map_hasherIiS6_NS_4hashIiEELb1EEENS_21__unordered_map_equalIiS6_NS_8equal_toIiEELb1EEENS_9allocatorIS6_EEE5eraseENS_21__hash_const_iteratorIPNS_11__hash_nodeIS6_PvEEEE
	movq	%rax, -152(%rbp)
	movq	$1, -104(%rbp)
LBB39_3:
	movq	-104(%rbp), %rax
	addq	$176, %rsp
	popq	%rbp
	retq
	.cfi_endproc

	.globl	__ZNSt3__112__hash_tableINS_17__hash_value_typeIiPN5lunar12green_thread7contextEEENS_22__unordered_map_hasherIiS6_NS_4hashIiEELb1EEENS_21__unordered_map_equalIiS6_NS_8equal_toIiEELb1EEENS_9allocatorIS6_EEE5eraseENS_21__hash_const_iteratorIPNS_11__hash_nodeIS6_PvEEEE
	.weak_def_can_be_hidden	__ZNSt3__112__hash_tableINS_17__hash_value_typeIiPN5lunar12green_thread7contextEEENS_22__unordered_map_hasherIiS6_NS_4hashIiEELb1EEENS_21__unordered_map_equalIiS6_NS_8equal_toIiEELb1EEENS_9allocatorIS6_EEE5eraseENS_21__hash_const_iteratorIPNS_11__hash_nodeIS6_PvEEEE
	.align	4, 0x90
__ZNSt3__112__hash_tableINS_17__hash_value_typeIiPN5lunar12green_thread7contextEEENS_22__unordered_map_hasherIiS6_NS_4hashIiEELb1EEENS_21__unordered_map_equalIiS6_NS_8equal_toIiEELb1EEENS_9allocatorIS6_EEE5eraseENS_21__hash_const_iteratorIPNS_11__hash_nodeIS6_PvEEEE: ## @_ZNSt3__112__hash_tableINS_17__hash_value_typeIiPN5lunar12green_thread7contextEEENS_22__unordered_map_hasherIiS6_NS_4hashIiEELb1EEENS_21__unordered_map_equalIiS6_NS_8equal_toIiEELb1EEENS_9allocatorIS6_EEE5eraseENS_21__hash_const_iteratorIPNS_11__hash_nodeIS6_PvEEEE
	.cfi_startproc
## BB#0:
	pushq	%rbp
Ltmp142:
	.cfi_def_cfa_offset 16
Ltmp143:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
Ltmp144:
	.cfi_def_cfa_register %rbp
	subq	$368, %rsp              ## imm = 0x170
	leaq	-336(%rbp), %rax
	leaq	-288(%rbp), %rcx
	movq	%rsi, -296(%rbp)
	movq	%rdi, -304(%rbp)
	movq	-304(%rbp), %rsi
	movq	-296(%rbp), %rdi
	movq	%rdi, -312(%rbp)
	movq	-312(%rbp), %rdi
	movq	%rcx, -272(%rbp)
	movq	%rdi, -280(%rbp)
	movq	-272(%rbp), %rdi
	movq	-280(%rbp), %rdx
	movq	%rdi, -256(%rbp)
	movq	%rdx, -264(%rbp)
	movq	-256(%rbp), %rdx
	movq	-264(%rbp), %rdi
	movq	%rdi, (%rdx)
	movq	%rcx, -8(%rbp)
	movq	-8(%rbp), %rcx
	movq	(%rcx), %rdx
	movq	(%rdx), %rdx
	movq	%rdx, (%rcx)
	movq	-296(%rbp), %rcx
	movq	%rcx, -344(%rbp)
	movq	-344(%rbp), %rdx
	movq	%rax, %rdi
	callq	__ZNSt3__112__hash_tableINS_17__hash_value_typeIiPN5lunar12green_thread7contextEEENS_22__unordered_map_hasherIiS6_NS_4hashIiEELb1EEENS_21__unordered_map_equalIiS6_NS_8equal_toIiEELb1EEENS_9allocatorIS6_EEE6removeENS_21__hash_const_iteratorIPNS_11__hash_nodeIS6_PvEEEE
	leaq	-336(%rbp), %rax
	movq	%rax, -248(%rbp)
	movq	-248(%rbp), %rax
	movq	%rax, -240(%rbp)
	movq	-240(%rbp), %rax
	movq	%rax, -216(%rbp)
	movq	$0, -224(%rbp)
	movq	-216(%rbp), %rax
	movq	%rax, -208(%rbp)
	movq	-208(%rbp), %rcx
	movq	%rcx, -200(%rbp)
	movq	-200(%rbp), %rcx
	movq	(%rcx), %rcx
	movq	%rcx, -232(%rbp)
	movq	-224(%rbp), %rcx
	movq	%rax, -40(%rbp)
	movq	-40(%rbp), %rdx
	movq	%rdx, -32(%rbp)
	movq	-32(%rbp), %rdx
	movq	%rcx, (%rdx)
	cmpq	$0, -232(%rbp)
	movq	%rax, -352(%rbp)        ## 8-byte Spill
	je	LBB40_6
## BB#1:
	movq	-352(%rbp), %rax        ## 8-byte Reload
	movq	%rax, -24(%rbp)
	movq	-24(%rbp), %rcx
	movq	%rcx, -16(%rbp)
	movq	-16(%rbp), %rcx
	addq	$8, %rcx
	movq	-232(%rbp), %rdx
	movq	%rcx, -184(%rbp)
	movq	%rdx, -192(%rbp)
	movq	-184(%rbp), %rcx
	testb	$1, 8(%rcx)
	movq	%rcx, -360(%rbp)        ## 8-byte Spill
	je	LBB40_3
## BB#2:
	movq	-360(%rbp), %rax        ## 8-byte Reload
	movq	(%rax), %rcx
	movq	-192(%rbp), %rdx
	addq	$16, %rdx
	movq	%rdx, -176(%rbp)
	movq	-176(%rbp), %rdx
	movq	%rcx, -144(%rbp)
	movq	%rdx, -152(%rbp)
	movq	-144(%rbp), %rcx
	movq	-152(%rbp), %rdx
	movq	%rcx, -128(%rbp)
	movq	%rdx, -136(%rbp)
	movq	-136(%rbp), %rcx
	movq	%rcx, -112(%rbp)
	movq	-112(%rbp), %rcx
	movq	%rcx, -104(%rbp)
LBB40_3:
	cmpq	$0, -192(%rbp)
	je	LBB40_5
## BB#4:
	movq	-360(%rbp), %rax        ## 8-byte Reload
	movq	(%rax), %rcx
	movq	-192(%rbp), %rdx
	movq	%rcx, -80(%rbp)
	movq	%rdx, -88(%rbp)
	movq	$1, -96(%rbp)
	movq	-80(%rbp), %rcx
	movq	-88(%rbp), %rdx
	movq	-96(%rbp), %rsi
	movq	%rcx, -56(%rbp)
	movq	%rdx, -64(%rbp)
	movq	%rsi, -72(%rbp)
	movq	-64(%rbp), %rcx
	movq	%rcx, -48(%rbp)
	movq	-48(%rbp), %rdi
	callq	__ZdlPv
LBB40_5:                                ## %_ZNSt3__122__hash_node_destructorINS_9allocatorINS_11__hash_nodeINS_17__hash_value_typeIiPN5lunar12green_thread7contextEEEPvEEEEEclEPSA_.exit.i.i.i
	jmp	LBB40_6
LBB40_6:                                ## %_ZNSt3__110unique_ptrINS_11__hash_nodeINS_17__hash_value_typeIiPN5lunar12green_thread7contextEEEPvEENS_22__hash_node_destructorINS_9allocatorIS9_EEEEED1Ev.exit
	movq	-288(%rbp), %rax
	addq	$368, %rsp              ## imm = 0x170
	popq	%rbp
	retq
	.cfi_endproc

	.globl	__ZNSt3__112__hash_tableINS_17__hash_value_typeIiPN5lunar12green_thread7contextEEENS_22__unordered_map_hasherIiS6_NS_4hashIiEELb1EEENS_21__unordered_map_equalIiS6_NS_8equal_toIiEELb1EEENS_9allocatorIS6_EEE6removeENS_21__hash_const_iteratorIPNS_11__hash_nodeIS6_PvEEEE
	.weak_def_can_be_hidden	__ZNSt3__112__hash_tableINS_17__hash_value_typeIiPN5lunar12green_thread7contextEEENS_22__unordered_map_hasherIiS6_NS_4hashIiEELb1EEENS_21__unordered_map_equalIiS6_NS_8equal_toIiEELb1EEENS_9allocatorIS6_EEE6removeENS_21__hash_const_iteratorIPNS_11__hash_nodeIS6_PvEEEE
	.align	4, 0x90
__ZNSt3__112__hash_tableINS_17__hash_value_typeIiPN5lunar12green_thread7contextEEENS_22__unordered_map_hasherIiS6_NS_4hashIiEELb1EEENS_21__unordered_map_equalIiS6_NS_8equal_toIiEELb1EEENS_9allocatorIS6_EEE6removeENS_21__hash_const_iteratorIPNS_11__hash_nodeIS6_PvEEEE: ## @_ZNSt3__112__hash_tableINS_17__hash_value_typeIiPN5lunar12green_thread7contextEEENS_22__unordered_map_hasherIiS6_NS_4hashIiEELb1EEENS_21__unordered_map_equalIiS6_NS_8equal_toIiEELb1EEENS_9allocatorIS6_EEE6removeENS_21__hash_const_iteratorIPNS_11__hash_nodeIS6_PvEEEE
	.cfi_startproc
## BB#0:
	pushq	%rbp
Ltmp145:
	.cfi_def_cfa_offset 16
Ltmp146:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
Ltmp147:
	.cfi_def_cfa_register %rbp
	pushq	%r14
	pushq	%rbx
	subq	$624, %rsp              ## imm = 0x270
Ltmp148:
	.cfi_offset %rbx, -32
Ltmp149:
	.cfi_offset %r14, -24
	movq	%rdi, %rax
	movq	%rdx, -584(%rbp)
	movq	%rsi, -592(%rbp)
	movq	-592(%rbp), %rdx
	movq	-584(%rbp), %rsi
	movq	%rsi, -600(%rbp)
	movq	%rdx, -576(%rbp)
	movq	-576(%rbp), %rsi
	movq	%rsi, -568(%rbp)
	movq	-568(%rbp), %rsi
	movq	%rsi, -560(%rbp)
	movq	-560(%rbp), %rsi
	movq	%rsi, -552(%rbp)
	movq	-552(%rbp), %rsi
	addq	$8, %rsi
	movq	%rsi, -544(%rbp)
	movq	-544(%rbp), %rsi
	movq	%rsi, -536(%rbp)
	movq	-536(%rbp), %rsi
	movq	%rsi, -528(%rbp)
	movq	-528(%rbp), %rsi
	movq	(%rsi), %rsi
	movq	%rsi, -608(%rbp)
	movq	-600(%rbp), %rsi
	movq	8(%rsi), %rsi
	movq	-608(%rbp), %rcx
	movq	%rsi, -512(%rbp)
	movq	%rcx, -520(%rbp)
	movq	-520(%rbp), %rcx
	movq	-520(%rbp), %rsi
	subq	$1, %rsi
	andq	%rsi, %rcx
	cmpq	$0, %rcx
	movq	%rax, -656(%rbp)        ## 8-byte Spill
	movq	%rdi, -664(%rbp)        ## 8-byte Spill
	movq	%rdx, -672(%rbp)        ## 8-byte Spill
	jne	LBB41_2
## BB#1:
	movq	-512(%rbp), %rax
	movq	-520(%rbp), %rcx
	subq	$1, %rcx
	andq	%rcx, %rax
	movq	%rax, -680(%rbp)        ## 8-byte Spill
	jmp	LBB41_3
LBB41_2:
	movq	-512(%rbp), %rax
	xorl	%ecx, %ecx
	movl	%ecx, %edx
	divq	-520(%rbp)
	movq	%rdx, -680(%rbp)        ## 8-byte Spill
LBB41_3:                                ## %_ZNSt3__116__constrain_hashEmm.exit
	movq	-680(%rbp), %rax        ## 8-byte Reload
	movq	%rax, -688(%rbp)        ## 8-byte Spill
## BB#4:
	movq	-688(%rbp), %rax        ## 8-byte Reload
	movq	%rax, -616(%rbp)
	movq	-616(%rbp), %rcx
	movq	-672(%rbp), %rdx        ## 8-byte Reload
	movq	%rdx, -40(%rbp)
	movq	%rcx, -48(%rbp)
	movq	-40(%rbp), %rcx
	movq	-48(%rbp), %rsi
	movq	%rcx, -32(%rbp)
	movq	-32(%rbp), %rcx
	movq	%rcx, -24(%rbp)
	movq	-24(%rbp), %rcx
	shlq	$3, %rsi
	addq	(%rcx), %rsi
	movq	%rsi, -696(%rbp)        ## 8-byte Spill
## BB#5:
	movq	-696(%rbp), %rax        ## 8-byte Reload
	movq	(%rax), %rcx
	movq	%rcx, -624(%rbp)
LBB41_6:                                ## =>This Inner Loop Header: Depth=1
	movq	-624(%rbp), %rax
	movq	(%rax), %rax
	cmpq	-600(%rbp), %rax
	je	LBB41_9
## BB#7:                                ##   in Loop: Header=BB41_6 Depth=1
	jmp	LBB41_8
LBB41_8:                                ##   in Loop: Header=BB41_6 Depth=1
	movq	-624(%rbp), %rax
	movq	(%rax), %rax
	movq	%rax, -624(%rbp)
	jmp	LBB41_6
LBB41_9:
	movq	-624(%rbp), %rax
	movq	-672(%rbp), %rcx        ## 8-byte Reload
	addq	$16, %rcx
	movq	%rcx, -64(%rbp)
	movq	-64(%rbp), %rcx
	movq	%rcx, -56(%rbp)
	movq	-56(%rbp), %rcx
	movq	%rcx, -80(%rbp)
	movq	-80(%rbp), %rcx
	movq	%rcx, -72(%rbp)
	movq	-72(%rbp), %rcx
	cmpq	%rcx, %rax
	je	LBB41_15
## BB#10:
	movq	-624(%rbp), %rax
	movq	8(%rax), %rax
	movq	-608(%rbp), %rcx
	movq	%rax, -88(%rbp)
	movq	%rcx, -96(%rbp)
	movq	-96(%rbp), %rax
	movq	-96(%rbp), %rcx
	subq	$1, %rcx
	andq	%rcx, %rax
	cmpq	$0, %rax
	jne	LBB41_12
## BB#11:
	movq	-88(%rbp), %rax
	movq	-96(%rbp), %rcx
	subq	$1, %rcx
	andq	%rcx, %rax
	movq	%rax, -704(%rbp)        ## 8-byte Spill
	jmp	LBB41_13
LBB41_12:
	movq	-88(%rbp), %rax
	xorl	%ecx, %ecx
	movl	%ecx, %edx
	divq	-96(%rbp)
	movq	%rdx, -704(%rbp)        ## 8-byte Spill
LBB41_13:                               ## %_ZNSt3__116__constrain_hashEmm.exit3
	movq	-704(%rbp), %rax        ## 8-byte Reload
	movq	%rax, -712(%rbp)        ## 8-byte Spill
## BB#14:
	movq	-712(%rbp), %rax        ## 8-byte Reload
	cmpq	-616(%rbp), %rax
	je	LBB41_24
LBB41_15:
	movq	-600(%rbp), %rax
	cmpq	$0, (%rax)
	je	LBB41_21
## BB#16:
	movq	-600(%rbp), %rax
	movq	(%rax), %rax
	movq	8(%rax), %rax
	movq	-608(%rbp), %rcx
	movq	%rax, -104(%rbp)
	movq	%rcx, -112(%rbp)
	movq	-112(%rbp), %rax
	movq	-112(%rbp), %rcx
	subq	$1, %rcx
	andq	%rcx, %rax
	cmpq	$0, %rax
	jne	LBB41_18
## BB#17:
	movq	-104(%rbp), %rax
	movq	-112(%rbp), %rcx
	subq	$1, %rcx
	andq	%rcx, %rax
	movq	%rax, -720(%rbp)        ## 8-byte Spill
	jmp	LBB41_19
LBB41_18:
	movq	-104(%rbp), %rax
	xorl	%ecx, %ecx
	movl	%ecx, %edx
	divq	-112(%rbp)
	movq	%rdx, -720(%rbp)        ## 8-byte Spill
LBB41_19:                               ## %_ZNSt3__116__constrain_hashEmm.exit2
	movq	-720(%rbp), %rax        ## 8-byte Reload
	movq	%rax, -728(%rbp)        ## 8-byte Spill
## BB#20:
	movq	-728(%rbp), %rax        ## 8-byte Reload
	cmpq	-616(%rbp), %rax
	je	LBB41_23
LBB41_21:
	movq	-616(%rbp), %rax
	movq	-672(%rbp), %rcx        ## 8-byte Reload
	movq	%rcx, -136(%rbp)
	movq	%rax, -144(%rbp)
	movq	-136(%rbp), %rax
	movq	-144(%rbp), %rdx
	movq	%rax, -128(%rbp)
	movq	-128(%rbp), %rax
	movq	%rax, -120(%rbp)
	movq	-120(%rbp), %rax
	shlq	$3, %rdx
	addq	(%rax), %rdx
	movq	%rdx, -736(%rbp)        ## 8-byte Spill
## BB#22:
	movq	-736(%rbp), %rax        ## 8-byte Reload
	movq	$0, (%rax)
LBB41_23:
	jmp	LBB41_24
LBB41_24:
	movq	-600(%rbp), %rax
	cmpq	$0, (%rax)
	je	LBB41_33
## BB#25:
	movq	-600(%rbp), %rax
	movq	(%rax), %rax
	movq	8(%rax), %rax
	movq	-608(%rbp), %rcx
	movq	%rax, -152(%rbp)
	movq	%rcx, -160(%rbp)
	movq	-160(%rbp), %rax
	movq	-160(%rbp), %rcx
	subq	$1, %rcx
	andq	%rcx, %rax
	cmpq	$0, %rax
	jne	LBB41_27
## BB#26:
	movq	-152(%rbp), %rax
	movq	-160(%rbp), %rcx
	subq	$1, %rcx
	andq	%rcx, %rax
	movq	%rax, -744(%rbp)        ## 8-byte Spill
	jmp	LBB41_28
LBB41_27:
	movq	-152(%rbp), %rax
	xorl	%ecx, %ecx
	movl	%ecx, %edx
	divq	-160(%rbp)
	movq	%rdx, -744(%rbp)        ## 8-byte Spill
LBB41_28:                               ## %_ZNSt3__116__constrain_hashEmm.exit1
	movq	-744(%rbp), %rax        ## 8-byte Reload
	movq	%rax, -752(%rbp)        ## 8-byte Spill
## BB#29:
	movq	-752(%rbp), %rax        ## 8-byte Reload
	movq	%rax, -632(%rbp)
	movq	-632(%rbp), %rcx
	cmpq	-616(%rbp), %rcx
	je	LBB41_32
## BB#30:
	movq	-624(%rbp), %rax
	movq	-632(%rbp), %rcx
	movq	-672(%rbp), %rdx        ## 8-byte Reload
	movq	%rdx, -184(%rbp)
	movq	%rcx, -192(%rbp)
	movq	-184(%rbp), %rcx
	movq	-192(%rbp), %rsi
	movq	%rcx, -176(%rbp)
	movq	-176(%rbp), %rcx
	movq	%rcx, -168(%rbp)
	movq	-168(%rbp), %rcx
	shlq	$3, %rsi
	addq	(%rcx), %rsi
	movq	%rax, -760(%rbp)        ## 8-byte Spill
	movq	%rsi, -768(%rbp)        ## 8-byte Spill
## BB#31:
	movq	-768(%rbp), %rax        ## 8-byte Reload
	movq	-760(%rbp), %rcx        ## 8-byte Reload
	movq	%rcx, (%rax)
LBB41_32:
	jmp	LBB41_33
LBB41_33:
	leaq	-328(%rbp), %rax
	leaq	-344(%rbp), %rcx
	leaq	-368(%rbp), %rdx
	leaq	-384(%rbp), %rsi
	leaq	-648(%rbp), %rdi
	movq	-600(%rbp), %r8
	movq	(%r8), %r8
	movq	-624(%rbp), %r9
	movq	%r8, (%r9)
	movq	-600(%rbp), %r8
	movq	$0, (%r8)
	movq	-672(%rbp), %r8         ## 8-byte Reload
	movq	%r8, -216(%rbp)
	movq	-216(%rbp), %r9
	addq	$24, %r9
	movq	%r9, -208(%rbp)
	movq	-208(%rbp), %r9
	movq	%r9, -200(%rbp)
	movq	-200(%rbp), %r9
	movq	(%r9), %r10
	addq	$-1, %r10
	movq	%r10, (%r9)
	movq	-600(%rbp), %r9
	movq	%r8, -240(%rbp)
	movq	-240(%rbp), %r10
	addq	$16, %r10
	movq	%r10, -232(%rbp)
	movq	-232(%rbp), %r10
	movq	%r10, -224(%rbp)
	movq	-224(%rbp), %r10
	movq	%rdi, -272(%rbp)
	movq	%r10, -280(%rbp)
	movb	$1, -281(%rbp)
	movq	-272(%rbp), %r10
	movb	-281(%rbp), %r11b
	movq	-280(%rbp), %rbx
	movq	%r10, -248(%rbp)
	movq	%rbx, -256(%rbp)
	andb	$1, %r11b
	movb	%r11b, -257(%rbp)
	movq	-248(%rbp), %r10
	movq	-256(%rbp), %rbx
	movq	%rbx, (%r10)
	movb	-257(%rbp), %r11b
	andb	$1, %r11b
	movb	%r11b, 8(%r10)
	movq	-664(%rbp), %r10        ## 8-byte Reload
	movq	%r10, -488(%rbp)
	movq	%r9, -496(%rbp)
	movq	%rdi, -504(%rbp)
	movq	-488(%rbp), %rdi
	movq	-496(%rbp), %r9
	movq	-504(%rbp), %rbx
	movq	%rdi, -448(%rbp)
	movq	%r9, -456(%rbp)
	movq	%rbx, -464(%rbp)
	movq	-448(%rbp), %rdi
	movq	-456(%rbp), %r9
	movq	-464(%rbp), %rbx
	movq	%rbx, -440(%rbp)
	movq	-440(%rbp), %rbx
	movq	(%rbx), %r14
	movq	%r14, -480(%rbp)
	movq	8(%rbx), %rbx
	movq	%rbx, -472(%rbp)
	movq	-480(%rbp), %rbx
	movb	-472(%rbp), %r11b
	movq	%rbx, -416(%rbp)
	movb	%r11b, -408(%rbp)
	movq	%rdi, -424(%rbp)
	movq	%r9, -432(%rbp)
	movq	-424(%rbp), %rdi
	movq	-432(%rbp), %r9
	movq	-416(%rbp), %rbx
	movb	-408(%rbp), %r11b
	movq	%rbx, -368(%rbp)
	movb	%r11b, -360(%rbp)
	movq	%rdi, -376(%rbp)
	movq	%r9, -384(%rbp)
	movq	-376(%rbp), %rdi
	movq	%rsi, -352(%rbp)
	movq	-352(%rbp), %rsi
	movq	(%rsi), %rsi
	movq	%rdx, -296(%rbp)
	movq	-296(%rbp), %rdx
	movq	(%rdx), %r9
	movq	%r9, -400(%rbp)
	movq	8(%rdx), %rdx
	movq	%rdx, -392(%rbp)
	movq	-400(%rbp), %rdx
	movb	-392(%rbp), %r11b
	movq	%rdx, -328(%rbp)
	movb	%r11b, -320(%rbp)
	movq	%rdi, -336(%rbp)
	movq	%rsi, -344(%rbp)
	movq	-336(%rbp), %rdx
	movq	%rcx, -312(%rbp)
	movq	-312(%rbp), %rcx
	movq	(%rcx), %rcx
	movq	%rcx, (%rdx)
	movq	%rax, -304(%rbp)
	movq	-304(%rbp), %rax
	movq	(%rax), %rcx
	movq	%rcx, 8(%rdx)
	movq	8(%rax), %rax
	movq	%rax, 16(%rdx)
	movq	-656(%rbp), %rax        ## 8-byte Reload
	addq	$624, %rsp              ## imm = 0x270
	popq	%rbx
	popq	%r14
	popq	%rbp
	retq
	.cfi_endproc


.subsections_via_symbols
