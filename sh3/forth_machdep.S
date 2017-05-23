/*
 * Copyright (c) 2009-2016 Valery Ushakov
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
#include <forth.h>

/*
 * Should we provide just the bare minimum of asm words to test MI
 * forth implementations and simplify porting, or more asm words to
 * get a bit more speed once initial porting is done?
 */
#define MORE_ASM_WORDS 1

#define TOS	r8	/* top of parameter stack lives in a register */
#define PSP	r9	/* parameter stack pointer */
#define RSP	r10	/* return stack pointer */
#define IP	r11	/* instruction pointer */
#define NEXT	r13	/* "next" address to avoid memory loads */

#define SPILL_TOS	mov.l	TOS, @-PSP
#define FILL_TOS	mov.l	@PSP+, TOS


!!! ====================================================================
/**
 *
 * Forth stacks and buffers in the BSS section.
 *
 */

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

terminal_input_buffer:
	.space	4096
	.type	terminal_input_buffer, @object
	.size	terminal_input_buffer, 4096 ! XXX, compute


!!! ====================================================================
/**
 *
 * Trampoline code callable from C that starts Forth.
 *
 */
	.text
ENTRY(start_forth)
	!! prologue
	mov.l	r8, @-r15
	mov.l	r9, @-r15
	mov.l	r10, @-r15
	mov.l	r11, @-r15
	mov.l	r12, @-r15
	mov.l	r13, @-r15
	mov.l	r14, @-r15
	sts.l	pr, @-r15
	mov	r15, r14
	!! end prologue

	!! address of "next" lives in a dedicated global register
	mov.l	.L_next, NEXT

	!! these will be reset by ABORT
	mov	#0, TOS
	mov	#0, PSP
	mov	#0, RSP

	!! ABORT to forth
	mov.l	.L_abort_body, IP
	mov.l	@IP+, r0		! CFA of (abort), the first word
	add	#4, r0			! PFA - the actual code
	jmp	@r0
	 nop

.L_BYE_RETURNS_HERE:
	SPILL_TOS
	mov	PSP, r0		! return forth stack pointer

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

	.p2align 2
.L_next:	.long	next_code
.L_abort_body:	.long	abort_body


!!! ====================================================================

/*
 * Address of this code will be written into the code field
 * This code will be directly called by the interpreter
 * PFA of the defined word will be in TOS
 */
#define DOES_ASM(label)		  \
	.p2align 2		; \
	.type	label@function	; \
  label:			;


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

/*
 * Forth code to compile the above.  This will be part of DOES>
 *
 *   01 c7	mova	.+8, r0
 *   b6 2a	mov.l	IP, @-RSP
 *   2b 4d	jmp	@NEXT
 *   03 6b	 mov	r0, IP
 */
#define COMPILE_DOES_CODE		  \
	.long	lit, 0x2ab6c701, _comma	; \
	.long	lit, 0x6b034d2b, _comma

/*
 * Generate an instance for DOES> thunk to decouple MD asm from MI
 * forth code that can just copy the thunk when compiling DOES>
 */
CONSTANT("does-thunk", does_thunk)
	.long	0f
0:	.long	9f - does_thunk_code
	DOES_4TH(does_thunk_code)
9:	.size	does_thunk_code, .-does_thunk_code



!!! ====================================================================
/**
 *
 *				FORTH
 *
 */

	.data			! XXX: need writable+executable

	.p2align 2
	.global	heap
heap:

/*
 * Fake sentinel word that starts the vocabulary.
 */
	.p2align 2, 0
	!! Name Field
.LASTNFA = .		! init .LASTNFA for subsequent DEFWORDs
	.byte	0x80	! empty name, immediate (just in case?)
	!! Link Field
	.p2align 2, 0
	.long	0	! terminate the LFA-linked list
	!! Code Field
	.long	0	! XXX: use some sentinel code?  just crash for now
	!! Parameter Field
	.long	0	! there's no parameters, but provide a stub


!!! Asm part of ABORT that resets parameter and return stacks
!!! Must be the first word called by ABORT.
ASMWORD("(abort)", abort_parens)
	mov.l	rp0_const, RSP	! reset return stack
	mov.l	sp0_const, PSP	! reset parameter stack
	jmp	@NEXT
	 mov	#0, TOS		! clear TOS just in case
	ASMEND(abort_parens)


!!! Asm part of QUIT that resets return stack.
!!! Must be the first word called by QUIT.
ASMWORD("(quit)", quit_parens)
	mov.l	rp0_const, RSP	! reset return stack
	jmp	@NEXT
	 nop
	ASMEND(quit_parens)


!!! Where the stacks are.  (ABORT) and (QUIT) just above use these
!!! directly in assembler as literals.
CONSTANT("sp0", sp0)	.long	stack_bottom
CONSTANT("rp0", rp0)	.long	rstack_bottom


!!! Current stack pointer.
!!!     ( -- sp )
!!!
!!! XXX: Both SP@ and SP0 don't try to hide the fact that we keep TOS
!!! in a register.  This has implications in MI forth code, e.g. in .S
!!! or DEPTH.
!!!
!!! The cell just above SP0 is not part of stack - it's a place to
!!! spill invalid TOS register when we push the first value onto the
!!! stack.  The TOS is in a register, so to spill it into memory, DUP
!!! it.
ASMWORD("sp@", sp_fetch)
	mov	PSP, r0
	SPILL_TOS
	jmp	@NEXT
	 mov	r0, TOS
	ASMEND(sp_fetch)


!!! SP!
ASMWORD("sp!", sp_store)
	jmp	@NEXT
	 mov	TOS, PSP
	ASMEND(sp_fetch)


!!! RP@
ASMWORD("rp@", rp_fetch)
	SPILL_TOS
	jmp	@NEXT
	 mov	RSP, TOS
	ASMEND(rp_fetch)


!!! RP!
ASMWORD("rp!", rp_store)
	mov	TOS, RSP
	jmp	@NEXT
	 FILL_TOS
	ASMEND(rp_fetch)



!!! Asm part of BYE that returns control to the C code that started
!!! the Forth system.  The stacks are not reset and can be inspected.
ASMWORD("(bye)", bye_parens)
	mov.l	.L_return, r0
	jmp	@r0
	 nop

	.p2align 2
.L_return:
	.long	.L_BYE_RETURNS_HERE
	ASMEND(bye_parens)


!!! nop to use as a gdb breakpoint
ASMWORD("breakpoint", breakpoint)
	jmp	@NEXT
	 nop
	ASMEND(breakpoint)


/**
 * Threaded code interpreter.
 *
 * Executing a word means calling it's code with it's body at the top
 * of the parameter stack.
 *
 * The real core of the interpreter is "next_code" that interprets
 * endless stream of words.  Some of those words are forth definitions
 * that begin with "call_code" in their code field and end with EXIT.
 * That diverts execution by saving/restoring IP before proceeding to
 * "next_code".
 */

!!! EXECUTE - CORE 6.1.1370
!!!     ( i*x xt -- j*x )
!!!
!!! With indirect threaded code the execution token (xt) is the
!!! address of the code field (CFA).  See also next_code.
ASMWORD("execute", execute)
	mov.l	@TOS+, r0	! fetch code address and advance TOS to PFA
	jmp	@r0		! call word's code on word's body
	 nop
	ASMEND(execute)


!!! Code field of asm words points here.
!!! This should eventually become the DOES> part of CODE
DOES_ASM(asm_does)
	jmp	@TOS		! PFA: word's body is its asm code
	 FILL_TOS		! PFA is consumed
	.size asm_does, .-asm_does


!!! Code field of C words points here.  The body contains the C
!!! funtion address.  Call it as PSP = (*PFA)(PSP);
DOES_ASM(c_does)
	mov.l	@TOS, TOS	! function pointer from word's body
	jsr	@TOS
	 mov	PSP, r4		! PSP as argument
	mov	r0, PSP		! function may adjust PSP to push/pop cells
	jmp	@NEXT
	 FILL_TOS
	.size c_does, .-c_does


!!! Code field of colon definitions (i.e. forth words)  points here.
!!! This should eventually become the DOES> part of ":"
DOES_ASM(call_code)
	mov.l	IP, @-RSP	! save caller's IP
	bra	.L_next_code_no_spill
	 mov	TOS, IP		! set my IP (consume PFA from TOS)
	!! don't refill TOS here, just skip the spill in next_code
	!! and let next_code set vacant TOS to next word PFA
	.size call_code, .-call_code


!!! EXIT - CORE 6.1.1380
!!!     ( -- ) ( R: nest-sys -- )
ASMWORD("exit", exit_4th)
	mov.l	@RSP+, IP	! pop return address
	!! FALLTHROUGH to "NEXT"
next_code:
	SPILL_TOS
  .L_next_code_no_spill:	! -- entry point for call_code to jump to
	mov.l	@IP+, TOS	! TOS = CFA of the next word to execute
				!  and advance IP
	!! this is the code of EXECUTE
	mov.l	@TOS+, r0	! fetch code address and advance TOS to PFA
	jmp	@r0		! execute word's code
	 nop
	ASMEND(exit_4th)


!!! tail-call - note that the difference from EXIT is the order of
!!! CFA := @IP and IP := @RSP+
!!!
!!! This makes it possible to add a wrapper around words that examine
!!! the cell after them, like LIT, BRANCH, etc.  Just make the tail
!!! call to such word at the end of the wrapper and it will work.
ASMWORD("(goto)", goto_parens)
	SPILL_TOS
	mov.l	@IP, TOS	! TOS = CFA of the next word to execute
	mov.l	@RSP+, IP	! NB: this is what makes it a tail call
	!! this is the code of EXECUTE
	mov.l	@TOS+, r0	! fetch code address and advance TOS to PFA
	jmp	@r0		! execute word's code
	 nop
	ASMEND(goto_parens)



/**
 * Stack manipualtion
 */

!!! DROP - CORE 6.1.1260
!!!     ( x -- )
ASMWORD("drop", drop)
	jmp	@NEXT
	 FILL_TOS
	ASMEND(drop)


!!! DUP - CORE 6.1.1290
!!!     ( x -- x x )
ASMWORD("dup", dup)
	jmp	@NEXT
	 SPILL_TOS
	ASMEND(dup)


#ifdef MORE_ASM_WORDS
!!! ?DUP - CORE 6.1.0630
!!!     ( x -- 0 | x x )
ASMWORD("?dup", question_dup)
	tst	TOS, TOS
	bt	1f
	jmp	@NEXT
	 SPILL_TOS		! dup if non-zero
1:	jmp	@NEXT
	 nop			! do nothing if zero
	ASMEND(question_dup)
#endif


!!! SWAP - CORE 6.1.2260
!!!     ( x1 x2 -- x2 x1 )
ASMWORD("swap", swap)
	mov.l	@PSP, r0
	mov.l	TOS, @PSP
	jmp 	@NEXT
	 mov	r0, TOS
	ASMEND(swap)


!!! OVER - CORE 6.1.1990
!!!     ( x1 x2 -- x1 x2 x1 )
ASMWORD("over", over)
	mov.l	@PSP, r0
	SPILL_TOS
	jmp 	@NEXT
	 mov	r0, TOS
	ASMEND(over)


#ifdef MORE_ASM_WORDS
!!! NIP - CORE EXT 6.2.1930
!!!     ( x1 x2 -- x2 )
ASMWORD("nip", nip)
	jmp	@NEXT
	 add	#4, PSP
	ASMEND(nip)
#endif


#ifdef MORE_ASM_WORDS
!!! TUCK - CORE EXT 6.2.2300
!!!     ( x1 x2 -- x2 x1 x2 )
ASMWORD("tuck", tuck)
	mov.l	@PSP, r0	! fetch x1
	mov.l	TOS, @PSP	! store x2
	jmp	@NEXT
	 mov.l	r0, @-PSP	! push x1
	ASMEND(tuck)
#endif


#ifdef MORE_ASM_WORDS
!!! ROT - CORE 6.1.2160
!!!     ( x1 x2 x3 -- x2 x3 x1 )
ASMWORD("rot", rot)
	mov.l	@PSP, r2
	mov.l	@(4, PSP), r1
	mov.l	TOS, @PSP	! x3
	mov.l	r2, @(4, PSP)
	jmp	@NEXT
	 mov	r1, TOS
	ASMEND(rot)
#endif


#ifdef MORE_ASM_WORDS
!!! -ROT - non-standard, inverse of ROT
!!!     ( x1 x2 x3 -- x3 x1 x2 )
ASMWORD("-rot", minus_rot)
	mov.l	@PSP, r2
	mov.l	@(4, PSP), r1
	mov.l	r1, @PSP
	mov.l	TOS, @(4, PSP)	! x3
	jmp	@NEXT
	 mov	r2, TOS
	ASMEND(minus_rot)
#endif


!!! PICK - CORE EXT 6.2.2030
!!!     ( xu ... x1 x0 u -- xu ... x1 x0 xu )
ASMWORD("pick", pick)
	mov	PSP, r0
	shll2	TOS
	jmp	@NEXT
	 mov.l	@(r0, TOS), TOS
	ASMEND(pick)


!!! ROLL - CORE EXT 6.2.2150
!!!     ( xu xu-1 ... x0 u -- xu-1 ... x0 xu )
ASMWORD("roll", roll)
#  define DST r2
#  define SRC r3
	mov	TOS, DST
	shll2	DST
	add	PSP, DST
	mov	DST, SRC

	mov.l	@DST, r0

1:
	add	#-4, SRC
	mov.l	@SRC, r1
	mov.l	r1, @DST
	add	#-4, DST

	dt	TOS
	bf	1b

	add	#4, PSP

	jmp	@NEXT
	 mov	r0, TOS
	ASMEND(roll)


#ifdef MORE_ASM_WORDS
!!! 2DROP - CORE 6.1.0370
!!!     ( x1 x2 -- )
ASMWORD("2drop", two_drop)
	add	#4, PSP
	jmp	@NEXT
	 FILL_TOS
	ASMEND(two_drop)
#endif


#ifdef MORE_ASM_WORDS
!!! 2DUP - CORE 6.1.0380
!!!     ( x1 x2 -- x1 x2 x1 x2 )
ASMWORD("2dup", two_dup)
	mov.l	@PSP, r0
	SPILL_TOS
	jmp	@NEXT
	 mov.l	r0, @-PSP
	ASMEND(two_dup)
#endif


#ifdef MORE_ASM_WORDS
!!! 2SWAP - CORE 6.1.0430
!!!     ( x1 x2 x3 x4 -- x3 x4 x1 x2 )
ASMWORD("2swap", two_swap)
	mov.l	@PSP, r1
	mov.l	@(4, PSP), r2
	mov.l	@(8, PSP), r3
	mov.l	r1, @(8, PSP)
	mov.l	TOS, @(4, PSP)
	mov.l	r3, @PSP
	jmp	@NEXT
	 mov	r2, TOS
	ASMEND(two_swap)
#endif


#ifdef MORE_ASM_WORDS
!!! 2OVER - CORE 6.1.0400
!!!     ( x1 x2 x3 x4 -- x1 x2 x3 x4 x1 x2 )
ASMWORD("2over", two_over)
	mov.l	@(4, PSP), r0
	mov.l	@(8, PSP), r1
	SPILL_TOS
	mov.l	r1, @-PSP
	jmp	@NEXT
	 mov	r0, TOS
	ASMEND(two_over)
#endif


!!! >R - CORE 6.1.0580
!!!     ( x -- ) ( R: -- x )
ASMWORD(">r", to_r)
	mov.l	TOS, @-RSP
	jmp	@NEXT
	 FILL_TOS
	ASMEND(to_r)


!!! R> - CORE 6.1.2060
!!!     ( -- x ) ( R: x -- )
ASMWORD("r>", r_from)
	SPILL_TOS
	jmp	@NEXT
	 mov.l	@RSP+, TOS
	ASMEND(r_from)


!!! R@ - CORE 6.1.2070
!!!     ( -- x ) ( R: x -- x )
ASMWORD("r@", r_fetch)
	SPILL_TOS
	jmp	@NEXT
	 mov.l	@RSP, TOS
	ASMEND(r_fetch)


!!! 2>R CORE EXT 6.2.0340
!!!     ( x1 x2 -- ) ( R: -- x1 x2 )
ASMWORD("2>r", two_to_r)
	mov.l	@PSP+, r0
	mov.l	r0, @-RSP
	mov.l	TOS, @-RSP
	jmp	@NEXT
	 FILL_TOS
	ASMEND(two_to_r)


!!! 2R> - CORE EXT 6.2.0410
!!!     ( -- x1 x2 ) ( R: x1 x2 -- )
ASMWORD("2r>", two_r_from)
	SPILL_TOS
	mov.l	@RSP+, TOS
	mov.l	@RSP+, r0
	jmp	@NEXT
	 mov.l	r0, @-PSP
	ASMEND(two_r_from)


!!! 2R@ - CORE EXT 6.2.0415
!!!     ( -- x1 x2 ) ( R: x1 x2 -- x1 x2 )
ASMWORD("2r@", two_r_fetch)
	SPILL_TOS
	mov.l	@RSP, TOS
	mov.l	@(4, RSP), r0
	jmp	@NEXT
	 mov.l	r0, @-PSP
	ASMEND(two_r_fetch)



/**
 * Comparison
 */

!!! 0= - CORE 6.1.0270
!!!     ( x -- flag )
ASMWORD("0=", zero_equals)
	tst	TOS, TOS
	movt	TOS
	jmp	@NEXT
	 neg	TOS, TOS
	ASMEND(zero_equals)


#ifdef MORE_ASM_WORDS
!!! 0<> - CORE-EXT 6.2.0260
!!!     ( x -- flag )
ASMWORD("0<>", zero_not_equals)
	tst	TOS, TOS
	movt	TOS
	neg	TOS, TOS
	jmp	@NEXT
	 not	TOS, TOS
	ASMEND(zero_not_equals)
#endif


!!! 0> - CORE-EXT 6.2.0280
!!!     ( x -- flag )
ASMWORD("0>", zero_greater)
	cmp/pl	TOS
	movt	TOS
	jmp	@NEXT
	 neg	TOS, TOS
	ASMEND(zero_greater)


!!! 0< - CORE 6.1.0250
!!!     ( x -- flag )
ASMWORD("0<", zero_less)
	mov	#0, r0
	cmp/gt	TOS, r0
	movt	TOS
	jmp	@NEXT
	 neg	TOS, TOS
	ASMEND(zero_less)


!!! = - CORE 6.1.0530
!!!     ( x1 x2 -- flag )
ASMWORD("=", equals)
	mov.l	@PSP+, r0
	cmp/eq	TOS, r0
	movt	TOS
	jmp	@NEXT
	 neg	TOS, TOS
	ASMEND(equals)


!!! <> - CORE-EXT 6.2.0500
!!!     ( x1 x2 -- flag )
ASMWORD("<>", not_equals)
	mov.l	@PSP+, r0
	cmp/eq	TOS, r0
	movt	TOS
	neg	TOS, TOS
	jmp	@NEXT
	 not	TOS, TOS
	ASMEND(not_equals)


!!! < - CORE 6.1.0480
!!!     ( n1 n2 -- flag )
ASMWORD("<", less_than)
	mov.l	@PSP+, r0
	cmp/gt	r0, TOS
	movt	TOS
	jmp	@NEXT
	 neg	TOS, TOS
	ASMEND(less_than)


!!! > - CORE 6.1.0540
!!!     ( n1 n2 -- flag )
ASMWORD(">", greater_than)
	mov.l	@PSP+, r0
	cmp/gt	TOS, r0
	movt	TOS
	jmp	@NEXT
	 neg	TOS, TOS
	ASMEND(greater_than)


!!! U< - CORE 6.1.2340
!!!     ( u1 u2 -- flag )
ASMWORD("u<", u_less_than)
	mov.l	@PSP+, r0
	cmp/hi	r0, TOS
	movt	TOS
	jmp	@NEXT
	 neg	TOS, TOS
	ASMEND(u_less_than)


!!! U> - CORE-EXT 6.2.2350
!!!     ( u1 u2 -- flag )
ASMWORD("u>", u_greater_than)
	mov.l	@PSP+, r0
	cmp/hi	TOS, r0
	movt	TOS
	jmp	@NEXT
	 neg	TOS, TOS
	ASMEND(u_greater_than)



/**
 * Arithmetic and bit-wise operations
 */

#ifdef MORE_ASM_WORDS
!!! FALSE - CORE-EXT 6.2.1485
!!!     ( -- false )
ASMWORD("false", false)
	SPILL_TOS
	jmp	@NEXT
	 mov	#0, TOS
	ASMEND(false)
#endif


#ifdef MORE_ASM_WORDS
!!! TRUE - CORE-EXT 6.2.2298
!!!     ( -- true )
ASMWORD("true", true)
	SPILL_TOS
	jmp	@NEXT
	 mov	#-1, TOS
	ASMEND(true)
#endif


!!! NEGATE - CORE 6.1.1910
!!!     ( n1 -- n2 )
ASMWORD("negate", negate)
	jmp	@NEXT
	 neg	TOS, TOS
	ASMEND(negate)


#ifdef MORE_ASM_WORDS
!!! ABS - CORE 6.1.0690
!!!     ( n -- u )
ASMWORD("abs", abs)
	cmp/pz	TOS
	bt	1f
	neg	TOS, TOS
1:	jmp	@NEXT
	 nop
	ASMEND(abs)
#endif


!!! + - CORE 6.1.0120
!!!     ( n1|u1 n2|u2 -- n3|u3 )
ASMWORD("+", plus)
	mov.l	@PSP+, r0
	jmp	@NEXT
	 add	r0, TOS
	ASMEND(plus)


!!! +? - nonstandard: add with overflow check
!!!     used by LOOP to efficiently detect loop terminataion
!!!     ( n1|u1 n2|u2 -- n3|u3 flag )
ASMWORD("+?", plus_question)
	mov.l	@PSP+, r0
	addv	r0, TOS
	SPILL_TOS
	movt	TOS
	jmp	@NEXT
	 neg	TOS, TOS
	ASMEND(plus_question)


!!! - - CORE 6.1.0160
!!!     ( n1|u1 n2|u2 -- n3|u3 )
ASMWORD("-", minus)
	mov.l	@PSP+, r0
	sub	TOS, r0
	jmp	@NEXT
	 mov	r0, TOS
	ASMEND(minus)


#ifdef MORE_ASM_WORDS
!!! 1+ - CORE 6.1.0290
!!!     ( n1|u1 -- n2|u2 )
ASMWORD("1+", one_plus)
	jmp	@NEXT
	 add	#1, TOS
	ASMEND(one_plus)
#endif


#ifdef MORE_ASM_WORDS
!!! 1- - CORE 6.1.0300
!!!     ( n1|u1 -- n2|u2 )
ASMWORD("1-", one_minus)
	jmp	@NEXT
	 add	#-1, TOS
	ASMEND(one_minus)
#endif


!!! * - CORE 6.1.0090
!!!     ( n1|u1 n2|u2 -- n3|u3 )
ASMWORD("*", star)
	mov.l	@PSP+, r0
	mul.l	r0, TOS
	jmp	@NEXT
	 sts	macl, TOS
	ASMEND(star)


!!! / - CORE 6.1.0230
!!!     NB: on SH3 division is symmetric
!!!     ( n1 n2 -- n3 )
ASMWORD("/", slash)
	tst	TOS, TOS
	mov	TOS, r1		! r1 = divisor
	bt/s	.L_divide_by_zero
	 FILL_TOS		! TOS = dividend

	mov	#0, r2		! this is a fast way to set ...
	div0s	r2, TOS		! ... T := sign(dividend)
	subc	r3, r3		! sign-extend dividend into 64 bit {TOS, r3}
	subc	r2, TOS		! if dividend < 0 convert to 1-complement

	div0s	r1, r3		! setup division {TOS, r3} / r1
	.rept /* DIVSTEP */ 32 /* times { */
	rotcl	TOS		! feed bits of TOS into division via T
	div1	r1, r3
	.endr /* } */
	rotcl	TOS		! quotient in 1-complement
	jmp	@NEXT
	 addc	r2, TOS		! if quotient < 0 convert to 2-complement

  .L_divide_by_zero:
	jmp	@NEXT
	 mov	#0, TOS
	ASMEND(slash)


!!! U/ - nonstandard unsigned division
!!!     ( u1 u2 -- u3 )
ASMWORD("u/", u_slash)
	tst	TOS, TOS
	mov	TOS, r1		! r1 = divisor
	bt/s	.L_u_divide_by_zero
	 FILL_TOS		! TOS = dividend

	mov	#0, r0
	div0u
	.rept /* DIVSTEP */ 32 /* times { */
	rotcl	TOS		! feed bits of dividend into division via T
	div1	r1, r0
	.endr /* } */
	jmp	@NEXT
	 rotcl	TOS

  .L_u_divide_by_zero:
	jmp	@NEXT
	 mov	#0, TOS
	ASMEND(u_slash)


!!! 2* - CORE 6.1.0320
!!!     ( x1 -- x2 )
ASMWORD("2*", two_star)
	jmp	@NEXT
	 shal	TOS
	ASMEND(two_star)


!!! 2/ - CORE 6.1.0330
!!!     ( x1 -- x2 )
ASMWORD("2/", two_slash)
	jmp	@NEXT
	 shar	TOS
	ASMEND(two_slash)


!!! INVERT - CORE 6.1.1720
!!!     ( x1 -- x2 )
ASMWORD("invert", invert)
	jmp	@NEXT
	 not	TOS, TOS
	ASMEND(invert)


!!! AND - CORE 6.1.0720
!!!     ( x1 x2 -- x3 )
ASMWORD("and", and)
	mov.l	@PSP+, r0
	jmp	@NEXT
	 and	r0, TOS
	ASMEND(and)


!!! OR - CORE 6.1.1980
!!!     ( x1 x2 -- x3 )
ASMWORD("or", or)
	mov.l	@PSP+, r0
	jmp	@NEXT
	 or	r0, TOS
	ASMEND(or)


!!! XOR - CORE 6.1.2490
!!!     ( x1 x2 -- x3 )
ASMWORD("xor", xor)
	mov.l	@PSP+, r0
	jmp	@NEXT
	 xor	r0, TOS
	ASMEND(xor)


!! LSHIFT - CORE 6.1.1805
!!!     ( x1 u -- x2 )
ASMWORD("lshift", lshift)
	extu.b	TOS, r1		! shift amount (ensure positive)
	mov.l	@PSP+, TOS	! the number to shift
	jmp	@NEXT
	 shld	r1, TOS
	ASMEND(lshift)


!! RSHIFT - CORE 6.1.2162
!!!     ( x1 u -- x2 )
ASMWORD("rshift", rshift)
	extu.b	TOS, r1		! shift amount (ensure positive)
	mov.l	@PSP+, TOS	! the number to shift
	neg	r1, r1		! negative means shift right
	jmp	@NEXT
	 shld	r1, TOS
	ASMEND(rshift)



/**
 * Address alignment
 */

#if 0 && defined(MORE_ASM_WORDS)
!!! ALIGNED - CORE 6.1.0706
!!!     ( addr -- a-addr )
ASMWORD("aligned", aligned)
	add	#3, TOS
	mov	#-4, r0
	jmp	@NEXT
	 and	r0, TOS
	ASMEND(aligned)
#endif


#if 0 && defined(MORE_ASM_WORDS)
!!! CELL+ - CORE 6.1.0880
!!!     ( a-addr1 -- a-addr2 )
ASMWORD("cell+", cell_plus)
	jmp	@NEXT
	 add	#4, TOS
	ASMEND(cell_plus)
#endif


#if 0 && defined(MORE_ASM_WORDS)
!!! CELL- - nonstandard
!!!     ( a-addr1 -- a-addr2 )
ASMWORD("cell-", cell_minus)
	jmp	@NEXT
	 add	#-4, TOS
	ASMEND(cell_minus)
#endif


#if 0 && defined(MORE_ASM_WORDS)
!!! CELLS - CORE 6.1.0890
!!!     ( n1 -- n2 )
ASMWORD("cells", cells)
	jmp	@NEXT
	 shll2	TOS
	ASMEND(cells)
#endif


#if 0 && defined(MORE_ASM_WORDS)
!!! CHAR+ - CORE 6.1.0897
!!!     ( c-addr1 -- c-addr2 )
ASMWORD("char+", char_plus)
	jmp	@NEXT
	 add	#1, TOS
	ASMEND(char_plus)
#endif


#if 0 && defined(MORE_ASM_WORDS)
!!! CHARS - CORE 6.1.0898
!!!     ( n1 -- n2 )
ASMWORD("chars", chars)
	jmp	@NEXT
	 nop
	ASMEND(chars)
#endif



/**
 * Fetch and store
 */

!!! @ - CORE 6.1.0650
!!!     ( a-addr -- x )
ASMWORD("@", fetch)
	jmp	@NEXT
	 mov.l	@TOS, TOS
	ASMEND(fetch)


!!! ! - CORE 6.1.0010
!!!     ( x a-addr -- )
ASMWORD("!", store)
	mov.l	@PSP+, r0	! value
	mov.l	r0, @TOS	! store value
	jmp	@NEXT
	 FILL_TOS
	ASMEND(store)


#ifdef MORE_ASM_WORDS
!!! +! - CORE 6.1.0130
!!!     ( n|u a-addr -- )
ASMWORD("+!", plus_store)
	mov.l	@PSP+, r0	! value increment
	mov.l	@TOS, r1	! fetch current value
	add	r1, r0
	mov.l	r0, @TOS	! store new value
	jmp	@NEXT
	 FILL_TOS
	ASMEND(plus_store)
#endif


#ifdef MORE_ASM_WORDS
!!! 1+! - nonstandard
!!!     ( a-addr -- )
ASMWORD("1+!", one_plus_store)
	mov.l	@TOS, r1	! fetch current value
	add	#1, r1		! increment
	mov.l	r1, @TOS	! store sum
	jmp	@NEXT
	 FILL_TOS
	ASMEND(one_plus_store)
#endif


!!! C@ - CORE 6.1.0870
!!!     ( c-addr -- char )
ASMWORD("c@", c_fetch)
	mov.b	@TOS, TOS	! loaded byte is sign-extended
	jmp	@NEXT
	 extu.b	TOS, TOS	! we want it zero-extended
	ASMEND(c_fetch)


!!! C! - CORE 6.1.0850
!!!     ( char c-addr -- )
ASMWORD("c!", c_store)
	mov.l	@PSP+, r0	! value
	mov.b	r0, @TOS	! store value
	jmp	@NEXT
	 FILL_TOS
	ASMEND(c_store)


#ifdef MORE_ASM_WORDS
!!! 2@ - CORE 6.1.0350
!!!     ( a-addr -- x1 x2 )
ASMWORD("2@", two_fetch)
	mov.l	@(4, TOS), r0
	mov.l	@TOS, TOS	! lower address goes to the top
	jmp	@NEXT
	 mov.l	r0, @-PSP
	ASMEND(two_fetch)
#endif


#ifdef MORE_ASM_WORDS
!!! 2! - CORE 6.1.0310
!!!     ( x1 x2 a-addr -- )
ASMWORD("2!", two_store)
	mov.l	@PSP, r0	! top -> lower address
	mov.l	@(4, PSP), r1
	mov.l	r0, @TOS
	mov.l	r1, @(4, TOS)
	mov.l	@(8, PSP), TOS	! fill tos
	jmp	@NEXT
	 add	#12, PSP
	ASMEND(two_store)
#endif



/**
 * Strings
 */

#if 0 && defined(MORE_ASM_WORDS)
!!! COUNT - CORE 6.1.0980
!!!     ( c-addr1 -- c-addr2 u )
ASMWORD("count", count)
	mov.b	@TOS+, r0	! fetch count (sign-extended), advance ptr
	SPILL_TOS		! string contents address
	jmp	@NEXT
	 extu.b	r0, TOS		! zero-extend the count
	ASMEND(count)
#endif


!!! CMOVE - STRING 17.6.1.0910
!!!     ( src dst len -- )
!!! NB: char-by-char left-to-right copy is required by the standard
ASMWORD("cmove", cmove)
	!!	TOS		! len
	mov.l	@PSP, r1	! dst
	mov.l	@(4, PSP), r0	! src

	tst	TOS, TOS	! len == 0?
	bt	9f

1:	mov.b	@r0+, r2
	mov.b	r2, @r1
	dt	TOS
	bf/s	1b
	 add	#1, r1

9:	add	#8, PSP
	jmp	@NEXT
	 FILL_TOS
	ASMEND(cmove)


!!! CMOVE> - STRING 17.6.1.0920
!!!     ( src dst len -- )
!!! NB: char-by-char right-to-left copy is required by the standard
ASMWORD("cmove>", cmove_up)
	!!	TOS		! len
	mov.l	@PSP, r1	! dst
	mov.l	@(4, PSP), r0	! src

	tst	TOS, TOS	! len == 0?
	bt	9f

	add	TOS, r0
	add	TOS, r1

1:	add	#-1, r0
	mov.b	@r0, r2
	mov.b	r2, @-r1
	dt	TOS
	bf	1b

9:	add	#8, PSP
	jmp	@NEXT
	 FILL_TOS
	ASMEND(cmove_up)


!!! COMPARE - STRING 17.6.1.0935
!!!     ( c-addr1 u1 c-addr2 u2 -- n )
ASMWORD("compare", compare)
	!!	       TOS	! u2
	mov.l	@PSP+, r7	! ca2
	mov.l	@PSP+, r6	! u1
	mov.l	@PSP+, r5	! ca1

	tst	r6, r6		! u1 == 0?
.Loop:
	bf.s	.L_u1_pos
	 tst	TOS, TOS	! u2 == 0?

	movt	TOS		!  0 if u2 != 0; 1 if u2 == 0
	jmp	@NEXT
	 add	#-1, TOS	! -1 if u2 != 0; 0 if u2 == 0

.L_u1_pos:
	bf.s	.L_u1u2_pos
	 nop
	jmp	@NEXT
	 mov	#1, TOS

.L_u1u2_pos:
	mov.b	@r5+, r0
	add	#-1, r6

	mov.b	@r7+, r1
	add	#-1, TOS

	extu.b	r0, r0
	extu.b	r1, r1

	cmp/eq	r0, r1
	bt.s	.Loop
	 tst	r6, r6

	cmp/hi	r1, r0		! r0 > r1?
	bt	1f

	jmp	@NEXT
	 mov	#-1, TOS	! *ca1 < *ca2

1:	jmp	@NEXT
	 mov	#1, TOS		! *ca1 > *ca2
	ASMEND(compare)


!!! FILL - CORE 6.1.1540
!!!     ( c-addr u char -- )
ASMWORD("fill", fill)
	!! cheat, use memset(3)
	mov.l	1f, r0
	mov.l	@(4, PSP), r4	! dst
	mov	TOS, r5		! char
	mov.l	@PSP, r6	! len
	add	#8, PSP
	jsr	@r0
	 FILL_TOS
	jmp	@NEXT
	 nop
	.p2align 2
1:	.long	memset
	ASMEND(fill)


!!! MOVE - CORE 6.1.1900
!!!     ( src dst len -- )
ASMWORD("move", move)
	!! cheat, use memmove(3)
	mov.l	1f, r0
	mov.l	@PSP, r4	! dst
	mov.l	@(4, PSP), r5	! src
	mov	TOS, r6		! len
	add	#8, PSP
	jsr	@r0
	 FILL_TOS
	jmp	@NEXT
	 nop
	.p2align 2
1:	.long	memmove
	ASMEND(move)


!!! ====================================================================
!!! Double-cell numbers

!!! 3.1.4.1 Double-cell integers
!!! 
!!!   On the stack, the cell containing the most significant part of a
!!!   double-cell integer shall be above the cell containing the least
!!!   significant part.

!!! E.3.1 Big endian vs. little endian
!!! 
!!!   When a cell pair is moved from the stack to memory with 2!, the
!!!   cell that was on top of the stack is placed at the lower memory
!!!   address.


!!! S>D - CORE 6.1.2170
!!!     ( n -- d )
ASMWORD("s>d", s2d)
	SPILL_TOS		! least significant cell is below
	shll	TOS		! shift sign into the T bit
	jmp	@NEXT
	 subc	TOS, TOS	! most significant cell = 0 - T
	ASMEND(s2d)


!!! DNEGATE - DOUBLE 8.6.1.1230
!!!     ( d1 -- d2 )
ASMWORD("dnegate", dnegate)
	mov.l	@PSP, r0	! lower cell
	clrt
	negc	r0, r0
	negc	TOS, TOS
	jmp	@NEXT
	 mov.l	r0, @PSP


!!! D+ - DOUBLE 8.6.1.1040 
!!!     ( d1|ud1 d2|ud2 -- d3|ud3 )
ASMWORD("d+", d_plus)
	mov.l	@PSP+, r2	! lo(d2)
	mov.l	@PSP+, r1	! hi(d1)
	mov.l	@PSP+, r0	! lo(d1)
	clrt
	addc	r0, r2		! lo
	addc	r1, TOS		! hi
	jmp	@NEXT
	 mov.l	r2, @-PSP
	ASMEND(d_plus)


!!! M* - CORE 6.1.1810
!!!     ( n1 n2 -- d ) signed mixed multiplication (32x32 -> 64)
ASMWORD("m*", m_star)
	mov.l	@PSP+, r0
	dmuls.l	r0, TOS
	sts	macl, r0
	mov.l	r0, @-PSP
	jmp	@NEXT
	 sts	mach, TOS
	ASMEND(m_star)


!!! UM* - CORE 6.1.2360
!!!     ( n1 n2 -- d ) unsigned mixed multiplication (32x32 -> 64)
ASMWORD("um*", um_star)
	mov.l	@PSP+, r0
	dmulu.l	r0, TOS
	sts	macl, r0
	mov.l	r0, @-PSP
	jmp	@NEXT
	 sts	mach, TOS
	ASMEND(um_star)



/**
 * Control structure helpers
 */

!!! LIT - auxiliary for LITERAL (CORE 6.1.1780)
!!!   Forth version may be used, but then CELL+ must be in asm.
!!!   See forth definition.
ASMWORD("lit", lit)
	SPILL_TOS
	jmp	@NEXT
	 mov.l	@IP+, TOS
	ASMEND(lit)


#ifdef MORE_ASM_WORDS
!!! 2LIT - auxiliary for 2LITERAL (DOUBLE 8.6.1.0390)
ASMWORD("2lit", two_lit)
	SPILL_TOS
	mov.l	@IP+, TOS	! lower address goes to the top
	mov.l	@IP+, r0
	jmp	@NEXT
	 mov.l	r0, @-PSP
	ASMEND(two_lit)
#endif


!!! BRANCH - auxiliary for ELSE &co
ASMWORD("branch", branch)
	jmp	@NEXT
	 mov.l	@IP, IP
	ASMEND(branch)


!!! ?BRANCH - auxiliary for IF &co
ASMWORD("?branch", question_branch)
	tst	TOS, TOS
	bf/s	1f		! branch is taken when TOS is false(!)
	 FILL_TOS
	jmp	@NEXT
	 mov.l	@IP, IP		! take branch
1:	jmp	@NEXT
	 add	#4, IP		! skip branch address
	ASMEND(question_branch)


#ifdef MORE_ASM_WORDS
!!! I - CORE 6.1.1680
ASMWORD("i", i_4th)
	SPILL_TOS
	mov.l	@RSP, TOS
	mov.l	@(4, RSP), r0
	jmp	@NEXT
	 sub	r0, TOS
	ASMEND(i_4th)
#endif


#ifdef MORE_ASM_WORDS
!!! J - 6.1.1730
ASMWORD("j", j_4th)
	SPILL_TOS
	mov.l	@(12, RSP), TOS
	mov.l	@(16, RSP), r0
	jmp	@NEXT
	 sub	r0, TOS
	ASMEND(j_4th)
#endif


/* Machine-independent words in threaded code. */
#include "forth.S"

/* Random test code. */
#include "test.S"

	.data
!!! value for CURRENT
.LATEST = .LASTNFA

	.size	heap, . - heap

	.section ".bss"
.L_DATA_SPACE: /* begins HERE */
.FENCE = .
	.space	8192
