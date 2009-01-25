/* $Id$ */

#include <machine/asm.h>

#define TOS	r8		/* top of parameter stack lives in a register */
#define PSP	r9		/* parameter stack pointer */
#define RSP	r10		/* return stack pointer */
#define IP	r11		/* instruction pointer */
#define NEXT	r13		/* "next" address to avoid memory loads */

	.section ".bss"
	.balign	4096

stack_limit:
	.space	4096
stack_bottom:
	.space	4096		! hole
rstack_limit:
	.space	4096
rstack_bottom:


	.text
ENTRY(test)
	!! prologue
	mov.l	r8, @-r15
	mov.l	r9, @-r15
	mov.l	r10, @-r15
	mov.l	r11, @-r15
	mov.l	r12, @-r15
	mov.l	r13, @-r15
	mov.l	r13, @-r15
	mov.l	r14, @-r15
	sts.l	pr, @-r15
	mov	r15, r14
	!! end prologue

	mov	r4, TOS		! pass to forth our first argument

	!! stacks
	mov.l	.L_psp, PSP
	mov.l	.L_rsp, RSP

	!! to return from forth
	mova	1f, r0
	mov.l	r0, @-RSP	! for return_to_c

	!! to enter forth
	mov.l	.L_entry, IP
	mov.l	.L_next, NEXT

ALTENTRY(test_enter)		! for gdb breakpoing only
	jmp	@NEXT
	 nop

	.align	2		! mova target
1:
	mov	TOS, r0		! return value from the top of forth stack

	!! epilogue
	mov	r14, r15
	lds.l	@r15+, pr
	mov.l	@r15+, r14
	mov.l	@r15+, r13
	mov.l	@r15+, r12
	mov.l	@r15+, r11
	mov.l	@r15+, r10
	mov.l	@r15+, r9
	rts
	 mov.l	@r15+, r8

	.align	2
.L_psp:		.long	stack_bottom
.L_rsp:		.long	rstack_bottom
.L_entry: 	.long	entry_point
.L_next:	.long	next_code

!!! ====================================================================

#define NAME_FIELD(flags, name)   \
	.p2align 2, 0		; \
	.byte	flags | (2f-1f)	; \
1:	.ascii	name		; \
2:	.p2align 2, 0

#define CODE_ASM(label)		  \
	.align	2		; \
  label:			; \
	.long	asm_exec	; \
  label/**/_code:

#define CODE_4TH(label)		  \
	.align	2		; \
  label:			; \
	.long	call_code

#define EXIT_4TH		  \
	.long	exit_4th	; \
	.long	0xdeadbeef	/* sentinel */


!!! ====================================================================

	.data			! XXX: need writable+executable

	.align	2
	.global	heap
heap:
	.long	0xdeadbeef	! dummy

	!! This should eventually become the DOES> part of CODE
	.align	2
asm_exec:			! code field of asm words points here
	jmp	@TOS		! consume TOS
	 mov.l	@PSP+, TOS	! fill TOS

	!! This should eventually become the DOES> part of ":"
	.align	2
call_code:
	mov.l	IP, @-RSP	! save caller's IP
	bra	.L_next_code_no_spill
	 mov	TOS, IP		! set my IP (consume PFA from TOS)
	!! don't refill TOS here, just skip the spill in next_code
	!! and let next_code set vacant TOS to next word PFA


CODE_ASM(exit_4th)
	mov.l	@RSP+, IP	! pop return address
	!! FALLTHROUGH to "NEXT"
next_code:
	mov.l	TOS, @-PSP	! spill TOS
.L_next_code_no_spill:		! -- entry point for call_code to jump to
	mov.l	@IP+, r0	! r0 = CFA of the next word to execute
				!  and advance IP
	mov.l	@r0, r1		! r1 = *CFA (acctual code)
	mov	#4, TOS		!
	jmp	@r1		! execute word's code
	 add	r0, TOS		! TOS = PFA (for code to consume)


CODE_ASM(return_to_c)
	mov.l	@RSP+, r0
	jmp	@r0
	 nop


CODE_ASM(one)
	mov.l	TOS, @-PSP
	jmp	@NEXT
	 mov	#1, TOS


CODE_ASM(two)
	mov.l	TOS, @-PSP
	jmp	@NEXT
	 mov	#2, TOS


CODE_ASM(plus)
	mov.l	@PSP+, r0
	jmp	@NEXT
	 add	r0, TOS


CODE_ASM(minus)
	mov.l	@PSP+, r0
	sub	TOS, r0
	jmp	@NEXT
	 mov	r0, TOS


CODE_ASM(mul)
	mov.l	@PSP+, r0
	mul.l	r0, TOS
	jmp	@NEXT
	 sts	macl, TOS


CODE_ASM(div)
	!! sdivsi3 tweaked to return result in in TOS
	tst	TOS, TOS
	mov	TOS, r1
	bt/s	.L_divide_by_zero
	 mov.l	@PSP+, TOS

	mov	#0, r2
	div0s	r2, TOS
	subc	r3, r3
	subc	r2, TOS
	div0s	r1, r3
#define DIVSTEP		\
	rotcl	TOS;	\
	div1	r1, r3
	/* repeat 32 times */
	DIVSTEP; DIVSTEP; DIVSTEP; DIVSTEP; DIVSTEP; DIVSTEP; DIVSTEP; DIVSTEP;
	DIVSTEP; DIVSTEP; DIVSTEP; DIVSTEP; DIVSTEP; DIVSTEP; DIVSTEP; DIVSTEP;
	DIVSTEP; DIVSTEP; DIVSTEP; DIVSTEP; DIVSTEP; DIVSTEP; DIVSTEP; DIVSTEP;
	DIVSTEP; DIVSTEP; DIVSTEP; DIVSTEP; DIVSTEP; DIVSTEP; DIVSTEP; DIVSTEP;
#undef DIVSTEP
	rotcl	TOS
	jmp	@NEXT
	 addc	r2, TOS

.L_divide_by_zero:
	jmp	@NEXT
	 mov	#0, TOS


CODE_ASM(literal)
	mov.l	TOS, @-PSP
	jmp	@NEXT
	 mov.l	@IP+, TOS	! push literal compiled after us


CODE_4TH(one_plus_two)
	.long	literal
	.long	9
	.long	literal
	.long	3
	.long	div
	EXIT_4TH


CODE_4TH(plus_three)
	.long	one_plus_two
	.long	plus
	.long	literal
	.long	1000
	.long	mul
	EXIT_4TH


	.align	2
ALTENTRY(entry_point)		! call me via NEXT !!!
	.long	plus_three
	.long	return_to_c
	.long	0xdeadbeef
