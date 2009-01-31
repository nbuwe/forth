/* $Id$ */

#include <machine/asm.h>

#define TOS	r8		/* top of parameter stack lives in a register */
#define PSP	r9		/* parameter stack pointer */
#define RSP	r10		/* return stack pointer */
#define IP	r11		/* instruction pointer */
#define NEXT	r13		/* "next" address to avoid memory loads */

#define SPILL_TOS	mov.l	TOS, @-PSP
#define FILL_TOS	mov.l	@PSP+, TOS


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
	SPILL_TOS
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

#define CODE_ASM(label)		  \
	.align	2		; \
  label:			; \
	.long	asm_does	; \
  label/**/_code:


#define CODE_4TH(label)		  \
	.align	2		; \
  label:			; \
	.long	call_code	; \
  label/**/_body:


#define CODE_VAR(label)		  \
	.align	2		; \
  label:			; \
	.long	next_code	; \
  label/**/_var:


#define CODE_CONST(label)	  \
	.align	2		; \
  label:			; \
	.long	const_does	; \
  label/**/_const:


#define EXIT_4TH		  \
	.long	exit_4th	; \
	.long	0xdeadbeef	/* sentinel */


/*
 * Address of this code will be written into the code field
 * This code will be directly called by the interpreter
 * PFA of the defined word will be in TOS
 */
#define DOES_ASM(label)		  \
	.align	2		; \
  label:			; \


/*
 * For words with DOES> part (that contains forth code by definition)
 * the address of the following asm code will be compiled into the
 * code field of the defined word.
 *
 * This code does an equivalent of jumping to "CALL" with 1f in TOS,
 * but inlined and since we jump to "NEXT" directly we can avoid
 * messing with TOS at all.
 */
#define DOES_4TH(label)		  \
  DOES_ASM(label)		; \
	mova	1f, r0		; \
	mov.l	IP, @-RSP	; \
	jmp	@NEXT		; \
	 mov	r0, IP		; \
  1:	/* threaded code follows */



#define NAME_FIELD(name, flags)   \
	.align	2, 0		; \
	.byte	flags | (2f-1f)	; \
1:	.ascii	name		; \
2:	.align	2, 0


#define DEFWORD(name, flags, defcode, label)	  \
	NAME_FIELD(name, flags)			; \
	.long	0 /* XXX: link field */		; \
	defcode(label)

#define WORD(name, label)	DEFWORD(name, 0, CODE_4TH, label)
#define IMMWORD(name, label)	DEFWORD(name, 0x80, CODE_4TH, label)
#define ASMWORD(name, label)	DEFWORD(name, 0, CODE_ASM, label)
#define CONSTANT(name, label)	DEFWORD(name, 0, CODE_CONST, label)
#define VARIABLE(name, label)	DEFWORD(name, 0, CODE_VAR, label)

!!! ====================================================================

	.data			! XXX: need writable+executable

	.align	2
	.global	heap
heap:
	.long	0xdeadbeef	! dummy


CODE_ASM(return_to_c)
	mov.l	@RSP+, r0
	jmp	@r0
	 nop


	!! This should eventually become the DOES> part of ":"
	.align	2
call_code:
	mov.l	IP, @-RSP	! save caller's IP
	bra	.L_next_code_no_spill
	 mov	TOS, IP		! set my IP (consume PFA from TOS)
	!! don't refill TOS here, just skip the spill in next_code
	!! and let next_code set vacant TOS to next word PFA


!!! "exit" - CORE 6.1.1380
ASMWORD("exit", exit_4th)
	mov.l	@RSP+, IP	! pop return address
	!! FALLTHROUGH to "NEXT"
next_code:
	SPILL_TOS
  .L_next_code_no_spill:	! -- entry point for call_code to jump to
	mov.l	@IP+, r0	! r0 = CFA of the next word to execute
				!  and advance IP
	mov.l	@r0, r1		! r1 = *CFA (acctual code)
	mov	#4, TOS		!
	jmp	@r1		! execute word's code
	 add	r0, TOS		! TOS = PFA (for code to consume)


!!! ====================================================================


	!! This should eventually become the DOES> part of CODE
DOES_ASM(asm_does)		! code field of asm words points here
	jmp	@TOS		! consume TOS
	 FILL_TOS


	!! This should eventually become the DOES> part of "constant"
DOES_ASM(const_does)
	jmp	@NEXT
	 mov.l	@TOS, TOS


#if 0
	!! Example only - this is how the DOES> part of "constant"
	!! would look like if it was defined as forth code
DOES_4TH(const_does_4th_example)
	.long	fetch
	EXIT_4TH
#endif


/**
 * Stack manipualtion
 */


!!! "drop" - CORE 6.1.1260
ASMWORD("drop", drop)
	jmp	@NEXT
	 FILL_TOS


!!! "dup - CORE 6.1.1290
ASMWORD("dup", dup)
	jmp	@NEXT
	 SPILL_TOS


!!! "?dup" - CORE 6.1.0630
ASMWORD("?dup", question_dup)
	tst	TOS, TOS
	bt	1f
	jmp	@NEXT
	 SPILL_TOS		! dup if non-zero
1:	jmp	@NEXT
	 nop			! do nothing if zero


!!! "over" - CORE 6.1.1990
ASMWORD("over", over)
	mov.l	@PSP, r0
	SPILL_TOS
	jmp 	@NEXT
	 mov	r0, TOS


!!! "swap" - CORE 6.1.2260
ASMWORD("swap", swap)
	mov.l	@PSP, r0
	mov.l	TOS, @PSP
	jmp 	@NEXT
	 mov	r0, TOS


!!! "rot" - CORE 6.1.2160
ASMWORD("rot", rot)
	mov.l	@PSP+, r0
	mov.l	@PSP+, r1
	mov.l	r0, @-PSP
	mov.l	TOS, @-PSP
	jmp	@NEXT
	 mov	r1, TOS


/*

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



!!! "@" - CORE 6.1.0650
ASMWORD("@", fetch)
	jmp	@NEXT
	 mov.l	@TOS, TOS


!!! "!" - CORE 6.1.0010
ASMWORD("!", store)
	mov.l	@PSP+, r0	! address
	mov.l	TOS, @r0	! store value
	jmp	@NEXT
	 FILL_TOS


!!! "+!" - CORE 6.1.0130
ASMWORD("+!", plus_store)
	mov.l	@PSP+, r0	! address
	mov.l	@r0, r1		! fetch current value
	add	r1, TOS		! add
	mov.l	TOS, @r0	! store sum
	jmp	@NEXT
	 FILL_TOS


!!! "1+!" - nonstandard
ASMWORD("1+!", one_plus_store)
	mov.l	@TOS, r1	! fetch current value
	add	#1, r1		! increment
	mov.l	r1, @TOS	! store sum
	jmp	@NEXT
	 FILL_TOS


!!! "lit" - auxiliary for "literal" (CORE 6.1.1780)
ASMWORD("lit", lit)
	SPILL_TOS
	jmp	@NEXT
	 mov.l	@IP+, TOS


!!! "branch" - auxiliary for "else" &co
ASMWORD("branch", branch)
	jmp	@NEXT
	 mov.l	@IP+, IP


!!! "?branch" - auxiliary for "if" &co
ASMWORD("?branch", question_branch)
	tst	TOS, TOS
	bf/s	1f		! branch is taken when TOS is false(!)
	 FILL_TOS
	jmp	@NEXT
	 mov.l	@IP+, IP	! take branch
1:	jmp	@NEXT
	 add	#4, IP		! skip branch address


!!! "false" - CORE-EXT 6.2.1485
ASMWORD("false", false)
	SPILL_TOS
	jmp	@NEXT
	 mov	#0, TOS


!!! "true" - CORE-EXT 6.2.2298
ASMWORD("true", true)
	SPILL_TOS
	jmp	@NEXT
	 mov	#-1, TOS


!!! "1" - nonstandard
ASMWORD("1", one)
	SPILL_TOS
	jmp	@NEXT
	 mov	#1, TOS


!!! "2" - nonstandard
ASMWORD("2", two)
	SPILL_TOS
	jmp	@NEXT
	 mov	#2, TOS


!!! "abs" - CORE 6.1.0690
ASMWORD("abs", abs)
	cmp/pz	TOS
	bt	1f
	neg	TOS, TOS
1:	jmp	@NEXT
	 nop


!!! "negate" - CORE 6.1.1910
ASMWORD("negate", negate)
	jmp	@NEXT
	 neg	TOS, TOS


!!! "+" - CORE 6.1.0120
ASMWORD("+", plus)
	mov.l	@PSP+, r0
	jmp	@NEXT
	 add	r0, TOS


!!! "-" - CORE 6.1.0160
ASMWORD("-", minus)
	mov.l	@PSP+, r0
	sub	TOS, r0
	jmp	@NEXT
	 mov	r0, TOS


!!! "*" - CORE 6.1.0090
ASMWORD("*", star)
	mov.l	@PSP+, r0
	mul.l	r0, TOS
	jmp	@NEXT
	 sts	macl, TOS


!!! "/" - CORE 6.1.0230
!!!	sdivsi3 tweaked to return result in TOS
ASMWORD("/", slash)
	tst	TOS, TOS
	mov	TOS, r1
	bt/s	.L_divide_by_zero
	 FILL_TOS

	mov	#0, r2
	div0s	r2, TOS
	subc	r3, r3
	subc	r2, TOS
	div0s	r1, r3
	.rept /* DIVSTEP */ 32 /* times { */
	rotcl	TOS
	div1	r1, r3
	.endr /* } */
	rotcl	TOS
	jmp	@NEXT
	 addc	r2, TOS

  .L_divide_by_zero:
	jmp	@NEXT
	 mov	#0, TOS


!!! "1+" - CORE 6.1.0290
ASMWORD("1+", one_plus)
	jmp	@NEXT
	 add	#1, TOS


!!! "1-" - CORE 6.1.0300
ASMWORD("1-", one_minus)
	jmp	@NEXT
	 add	#-1, TOS


!!! "2*" - CORE 6.1.0320
ASMWORD("2*", two_star)
	jmp	@NEXT
	 shal	TOS


!!! "2/" - CORE 6.1.0330
ASMWORD("2/", two_slash)
	jmp	@NEXT
	 shar	TOS


!!! "0<" - CORE 6.1.0250
ASMWORD("0<", zero_less)
	mov	#0, r0
	cmp/gt	TOS, r0
	movt	TOS
	jmp	@NEXT
	 neg	TOS, TOS


!!! "0>" - CORE-EXT 6.2.0280
ASMWORD("0>", zero_greater)
	cmp/pl	TOS
	movt	TOS
	jmp	@NEXT
	 neg	TOS, TOS


!!! "0=" - CORE 6.1.0270
ASMWORD("0=", zero_equals)
	tst	TOS, TOS
	movt	TOS
	jmp	@NEXT
	 neg	TOS, TOS


!!! "0<>" - CORE-EXT 6.2.0260
ASMWORD("0<>", zero_not_equals)
	tst	TOS, TOS
	movt	TOS
	neg	TOS, TOS
	jmp	@NEXT
	 not	TOS, TOS


!!! "<" - CORE 6.1.0480
ASMWORD("<", less_than)
	mov.l	@PSP+, r0
	cmp/gt	r0, TOS
	movt	TOS
	jmp	@NEXT
	 neg	TOS, TOS


!!! ">" - CORE 6.1.0540
ASMWORD(">", greater_than)
	mov.l	@PSP+, r0
	cmp/gt	TOS, r0
	movt	TOS
	jmp	@NEXT
	 neg	TOS, TOS


!!! "=" - CORE 6.1.0530
ASMWORD("=", equals)
	mov.l	@PSP+, r0
	cmp/eq	TOS, r0
	movt	TOS
	jmp	@NEXT
	 neg	TOS, TOS


!!! "<>" - CORE-EXT 6.2.0500
ASMWORD("<>", not_equals)
	mov.l	@PSP+, r0
	cmp/eq	TOS, r0
	movt	TOS
	neg	TOS, TOS
	jmp	@NEXT
	 not	TOS, TOS


!!! "u<" - CORE 6.1.2340
ASMWORD("u<", u_less_than)
	mov.l	@PSP+, r0
	cmp/hi	r0, TOS
	movt	TOS
	jmp	@NEXT
	 neg	TOS, TOS


!!! "u>" - CORE-EXT 6.2.2350
ASMWORD("u>", u_greater_than)
	mov.l	@PSP+, r0
	cmp/hi	TOS, r0
	movt	TOS
	jmp	@NEXT
	 neg	TOS, TOS


!!! "invert" - CORE 6.1.1720
ASMWORD("invert", invert)
	jmp	@NEXT
	 not	TOS, TOS


!!! "and" - CORE 6.1.0720
ASMWORD("and", and)
	mov.l	@PSP+, r0
	jmp	@NEXT
	 and	r0, TOS


!!! "or" - CORE 6.1.1980
ASMWORD("or", or)
	mov.l	@PSP+, r0
	jmp	@NEXT
	 or	r0, TOS


!!! "xor" - CORE 6.1.2490
ASMWORD("xor", xor)
	mov.l	@PSP+, r0
	jmp	@NEXT
	 xor	r0, TOS
	

!CODE_ASM(lshift)		! "lshift" - CORE 6.1.1805
!CODE_ASM(rshift)		! "rshift" - CORE 6.1.2162
!CODE_ASM(max)			! "max" - CORE 6.1.1870
!CODE_ASM(min)			! "min" - CORE 6.1.1880


!!! ====================================================================


!!! "bl" - CORE 6.1.0770
CONSTANT("bl", bl) .long 0x20


!!! "cr" - CORE 6.1.0990
WORD("cr", cr)
	.long	lit, '\n, emit
	EXIT_4TH


!!! "emit" - CORE 6.1.1320
ASMWORD("emit", emit)
	mov.l	1f, r0
	mov	TOS, r4
	jsr	@r0
	 FILL_TOS
	jmp	@NEXT
	 nop
	.align	2
1:	.long	_C_LABEL(emit_impl)


!!! "space" - CORE 6.1.2220
WORD("space", space)
	.long	bl, emit
	EXIT_4TH


!!! "spaces" - CORE 6.1.2230
!WORD("spaces", space)


!!! "type" - CORE 6.1.2310
ASMWORD("type", type)
	mov.l	1f, r0
	mov	TOS, r5
	mov.l	@PSP+, r4
	jsr	@r0
	 FILL_TOS
	jmp	@NEXT
	 nop
	.align	2
1:	.long	_C_LABEL(type_impl)


!!! ====================================================================

CONSTANT("c1", c1) .long 0x00c0ffee
VARIABLE("v1", v1) .long 0x0000beef

WORD("three", three)
	.long	lit, 11, lit, 3, slash
	EXIT_4TH


WORD("test-word", test_word)
	.long	three, plus
	.long	lit, 100, lit, 10, not_equals
	.long	question_branch, 1f ! if
	.long	   lit, 1000, star
1:	!       then
	.long	v1
	.long	v1, fetch
	.long	v1, c1, store
	.long	v1, one_plus_store
	.long	v1, fetch
	EXIT_4TH

99:	.ascii	"Hello, World!"
100:

	.align	2
ALTENTRY(entry_point)		! call me via NEXT !!!
	.long	test_word
	.long	lit, 99b, lit, (100b-99b), type, cr
	.long	return_to_c
	.long	0xdeadbeef
