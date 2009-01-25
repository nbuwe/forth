/* $Id$ */

#include <machine/asm.h>

#define TOS	r8		/* top of parameter stack lives in a register */
#define PSP	r9		/* parameter stack pointer */
#define RSP	r10		/* return stack pointer */
#define IP	r11		/* instruction pointer */
#define NEXT	r13		/* "next" address to avoid memory loads */

	.section ".bss"
	.balign	4096

	.global stack_bottom

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
	mov.l	TOS, @-PSP
	mov	PSP, r0		! return value from the top of forth stack

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

!!! ====================================================================


/**
 * Stack manipualtion
 */

CODE_ASM(drop)			! "drop" - CORE 6.1.1260
	jmp	@NEXT
	 mov.l	@PSP+, TOS


CODE_ASM(dup)			! "dup - CORE 6.1.1290
	jmp	@NEXT
	 mov.l	TOS, @-PSP


CODE_ASM(question_dup)		! "?dup" - CORE 6.1.0630
	tst	TOS, TOS
	bt	1f
	jmp	@NEXT
	 mov.l	TOS, @-PSP
1:	jmp	@NEXT
	 nop


CODE_ASM(over)			! "over" - CORE 6.1.1990
	mov.l	@PSP, r0
	mov.l	TOS, @-PSP
	jmp 	@NEXT
	 mov	r0, TOS


CODE_ASM(swap)			! "swap" - CORE 6.1.2260
	mov.l	@PSP, r0
	mov.l	TOS, @PSP
	jmp 	@NEXT
	 mov	r0, TOS
	

/*

 ROT - CORE 6.1.2160
 SWAP - CORE 6.1.2260

6.2.1930 NIP
6.2.2030 PICK
6.2.2150 ROLL
6.2.2300 TUCK

*/

/*

6.1.2170 S>D
6.1.0370 2DROP
6.1.0380 2DUP
6.1.0400 2OVER
6.1.0430 2SWAP

*/

CODE_ASM(lit)			! aux for "literal" - CORE 6.1.1780
	mov.l	TOS, @-PSP
	jmp	@NEXT
	 mov.l	@IP+, TOS


CODE_ASM(branch)		! "branch"
	jmp	@NEXT
	 mov.l	@IP+, IP


CODE_ASM(question_branch)	! "?branch"
	tst	TOS, TOS
	bf/s	1f		! branch is taken when TOS is false(!)
	 mov.l	@PSP+, TOS

	jmp	@NEXT
	 mov.l	@IP+, IP	! take branch
1:
	jmp	@NEXT
	 add	#4, IP		! skip branch address


CODE_ASM(false)			! "false" - CORE-EXT 6.2.1485
	mov.l	TOS, @-PSP
	jmp	@NEXT
	 mov	#0, TOS


CODE_ASM(true)			! "true" - CORE-EXT 6.2.2298
	mov.l	TOS, @-PSP
	jmp	@NEXT
	 mov	#-1, TOS


CODE_ASM(one)			! "1" XXX
	mov.l	TOS, @-PSP
	jmp	@NEXT
	 mov	#1, TOS


CODE_ASM(two)			! "2" XXX
	mov.l	TOS, @-PSP
	jmp	@NEXT
	 mov	#2, TOS


CODE_ASM(abs)			! "abs" - CORE 6.1.0690
	cmp/pz	TOS
	bt	1f
	neg	TOS, TOS
1:	jmp	@NEXT
	 nop

CODE_ASM(negate)		! "negate" - CORE 6.1.1910
	jmp	@NEXT
	 neg	TOS, TOS


CODE_ASM(plus)			! "+" - CORE 6.1.0120
	mov.l	@PSP+, r0
	jmp	@NEXT
	 add	r0, TOS


CODE_ASM(minus)			! "-" - CORE 6.1.0160
	mov.l	@PSP+, r0
	sub	TOS, r0
	jmp	@NEXT
	 mov	r0, TOS


CODE_ASM(star)			! "*" - CORE 6.1.0090
	mov.l	@PSP+, r0
	mul.l	r0, TOS
	jmp	@NEXT
	 sts	macl, TOS


CODE_ASM(slash)		    	! "/" - CORE 6.1.0230
	!! sdivsi3 tweaked to return result in TOS
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


CODE_ASM(one_plus)		! "1+" - CORE 6.1.0290
	jmp	@NEXT
	 add	#1, TOS


CODE_ASM(one_minus)		! "1-" - CORE 6.1.0300
	jmp	@NEXT
	 add	#-1, TOS


CODE_ASM(two_star)		! "2*" - CORE 6.1.0320
	jmp	@NEXT
	 shal	TOS


CODE_ASM(two_slash)		! "2/" - CORE 6.1.0330
	jmp	@NEXT
	 shar	TOS


CODE_ASM(zero_less)		! "0<" - CORE 6.1.0250
	mov	#0, r0
	cmp/gt	TOS, r0
	movt	TOS
	jmp	@NEXT
	 neg	TOS, TOS


CODE_ASM(zero_greater)		! "0>" - CORE-EXT 6.2.0280
	cmp/pl	TOS
	movt	TOS
	jmp	@NEXT
	 neg	TOS, TOS


CODE_ASM(zero_equals)		! "0=" - CORE 6.1.0270
	tst	TOS, TOS
	movt	TOS
	jmp	@NEXT
	 neg	TOS, TOS


CODE_ASM(zero_not_equals)	! "0<>" - CORE-EXT 6.2.0260
	tst	TOS, TOS
	movt	TOS
	neg	TOS, TOS
	jmp	@NEXT
	 not	TOS, TOS


CODE_ASM(less_than)		! "<" - CORE 6.1.0480
	mov.l	@PSP+, r0
	cmp/gt	r0, TOS
	movt	TOS
	jmp	@NEXT
	 neg	TOS, TOS


CODE_ASM(greater_than)		! ">" - CORE 6.1.0540
	mov.l	@PSP+, r0
	cmp/gt	TOS, r0
	movt	TOS
	jmp	@NEXT
	 neg	TOS, TOS


CODE_ASM(equals)		! "=" - CORE 6.1.0530
	mov.l	@PSP+, r0
	cmp/eq	TOS, r0
	movt	TOS
	jmp	@NEXT
	 neg	TOS, TOS


CODE_ASM(not_equals)		! "<>" - CORE-EXT 6.2.0500
	mov.l	@PSP+, r0
	cmp/eq	TOS, r0
	movt	TOS
	neg	TOS, TOS
	jmp	@NEXT
	 not	TOS, TOS


CODE_ASM(u_less_than)		! "u<" - CORE 6.1.2340
	mov.l	@PSP+, r0
	cmp/hi	r0, TOS
	movt	TOS
	jmp	@NEXT
	 neg	TOS, TOS


CODE_ASM(u_greater_than)	! "u>" - CORE-EXT 6.2.2350
	mov.l	@PSP+, r0
	cmp/hi	TOS, r0
	movt	TOS
	jmp	@NEXT
	 neg	TOS, TOS


CODE_ASM(invert)		! "invert" - CORE 6.1.1720
	jmp	@NEXT
	 not	TOS, TOS


CODE_ASM(and)			! "and" - CORE 6.1.0720
	mov.l	@PSP+, r0
	jmp	@NEXT
	 and	r0, TOS
	

CODE_ASM(or)			! "or" - CORE 6.1.1980
	mov.l	@PSP+, r0
	jmp	@NEXT
	 or	r0, TOS
	

CODE_ASM(xor)			! "xor" - CORE 6.1.2490
	mov.l	@PSP+, r0
	jmp	@NEXT
	 xor	r0, TOS
	

!CODE_ASM(lshift)		! "lshift" - CORE 6.1.1805
!CODE_ASM(rshift)		! "rshift" - CORE 6.1.2162
!CODE_ASM(max)			! "max" - CORE 6.1.1870
!CODE_ASM(min)			! "min" - CORE 6.1.1880


!!! ====================================================================

CODE_4TH(one_plus_two)
	.long	lit, 9
	.long	lit, 3
	.long	slash
	EXIT_4TH


CODE_4TH(plus_three)
	.long	one_plus_two
	.long	plus
	.long	false
	.long	question_branch, 1f
	.long	lit, 1000
	.long	star
1:	
	.long	lit, 100
	.long	lit, 10
	.long	not_equals
	EXIT_4TH


	.align	2
ALTENTRY(entry_point)		! call me via NEXT !!!
	.long	plus_three
	.long	return_to_c
	.long	0xdeadbeef
