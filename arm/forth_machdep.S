/*
 * Copyright (c) 2009-2020 Valery Ushakov
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

	.arch armv5

#define TOS	r4	/* top of parameter stack lives in a register */
#define PSP	r5	/* parameter stack pointer */
#define RSP	r6	/* return stack pointer */
#define IP	r7	/* instruction pointer */
#define W	r8	/* "work" register */
#define NEXT	r10	/* "next" address to avoid memory loads */

#define SPILL_TOS	str     TOS, [PSP, #-4]!
#define FILL_TOS	ldr	TOS, [PSP], #4


/// ====================================================================
/**
 *
 * Trampoline code callable from C that starts Forth.
 *
 */
	.text
ENTRY(start_forth)
	mov	ip, sp
	push	{r4, r5, r6, r7, r8, r9, r10, fp, ip, lr}
	@@ end prologue

	@@ address of "next" lives in a dedicated global register
	ldr	NEXT, .L_next

	@@ these will be reset by ABORT
	mov	TOS, #0
	mov	PSP, #0
	mov	RSP, #0

	@@ ABORT to forth
	ldr	IP, .L_abort_body
	mov	pc, NEXT

.L_BYE_RETURNS_HERE:
	SPILL_TOS		@ for the C code to see in memory
	mov	r0, PSP		@ return forth stack pointer

	@@ epilogue
	ldm	sp, {r4, r5, r6, r7, r8, r9, r10, fp, sp, pc}

.L_next:	.long	next_code
.L_abort_body:	.long	abort_body



/// ====================================================================
/**
 *
 *				FORTH
 *
 */

/* stacks and other static buffers */
#include "forth-bss.S"

	.section ".forth", "awx", %progbits	@ XXX: need writable+executable

#define CODE(label)		  \
	.p2align 2		; \
	.type	label%function	; \
  label:			;

	.p2align 2
	.global	heap
heap:

#define CURRENT forth
ROOT_VOCABULARY("forth", forth)


/**
 * Threaded code interpreter.
 * NB: W is used to pass PFA.
 */

/// Code field of colon definitions (i.e. forth words) points here.
CODE(call_code)
	str	IP, [RSP, #-4]!	@ save caller's IP
	mov	IP, W		@ next_code passed callee's PFA in W
	b	next_code


/// EXIT - CORE 6.1.1380
///     ( -- ) ( R: nest-sys -- )
ASMWORD("exit", exit_4th)
	ldr	IP, [RSP], #4	@ back to caller's IP
	@@ FALLTHROUGH to "NEXT"
next_code:
	ldr	W, [IP], #4	@ W = CFA of the next word to execute
				@  and advance IP
	ldr	r0, [W], #4	@ fetch code address and advance W to PFA
	mov	pc, r0		@ execute word's code
	ASMEND(exit_4th)


/// tail-call - note that the difference from EXIT is the order of
/// CFA := @IP and IP := @RSP+
///
/// This makes it possible to add a wrapper around words that examine
/// the cell after them, like LIT, BRANCH, etc.  Just make the tail
/// call to such word at the end of the wrapper and it will work.
ASMWORD("(goto)", goto_parens)
	ldr	W, [IP]		@ W = CFA of the next word to execute
	ldr	r0, [W], #4	@ fetch code address and advance W to PFA
	ldr	IP, [RSP], #4	@ pretend the call was made by our caller
	mov	pc, r0		@ execute word's code
	ASMEND(goto_parens)


/// This thunk is compiled by DOES> in the defining word before the
/// threaded code for the DOES> part.  Defined word gets thunk's
/// address as its code.
#define DOES_4TH(label)		  \
  CODE(label)			; \
	/* XXX: TODO */		  \
  1:	/* threaded code follows */


/// EXECUTE - CORE 6.1.1370
///     ( i*x xt -- j*x )
ASMWORD("execute", execute)
	ldr	r0, [TOS] 	@ fetch word's code address from its CFA
	add	W, TOS, #4	@ advance to word's PFA
	FILL_TOS
	mov	pc, r0		@ execute word's code
	ASMEND(execute)



/**
 * Start-up and termination.
 */

/// Asm part of BYE that returns control to the C code that started
/// the Forth system.  The stacks are not reset and can be inspected.
ASMWORD("(bye)", bye_parens)
	ldr	r0, .L_return
	mov	pc, r0

	.p2align 2
.L_return:
	.long	.L_BYE_RETURNS_HERE
	ASMEND(bye_parens)


/// nop to use as a gdb breakpoint
ASMWORD("breakpoint", breakpoint)
	mov	pc, NEXT
	ASMEND(breakpoint)


/// Asm part of ABORT that resets parameter and return stacks
/// Must be the first word called by ABORT.
ASMWORD("(abort)", abort_parens)
	ldr	PSP, sp0_const	@ reset parameter stack
	ldr	RSP, rp0_const	@ reset return stack
	mov	TOS, #0		@ clear TOS just in case
	mov	pc, NEXT
	ASMEND(abort_parens)


/// Asm part of QUIT that resets return stack.
/// Must be the first word called by QUIT.
ASMWORD("(quit)", quit_parens)
	ldr	RSP, rp0_const	@ reset return stack
	mov	pc, NEXT
	ASMEND(quit_parens)


/// Reset parameter stack
/// OFW word, not in ANSI
ASMWORD("clear", clear)
	ldr	PSP, sp0_const	@ reset parameter stack
	mov	TOS, #0		@ clear TOS just in case
	mov	pc, NEXT
	ASMEND(clear)


/// Where the stacks are.  (ABORT) and (QUIT) just above use these
/// directly in assembler as literals.
CONSTANT("sp0", sp0)	.long	stack_bottom
CONSTANT("rp0", rp0)	.long	rstack_bottom



/**
 * Stack manipualtion
 */


/// DROP - CORE 6.1.1260
///     ( x -- )
ASMWORD("drop", drop)
	FILL_TOS
	mov	pc, NEXT
	ASMEND(drop)


/// DUP - CORE 6.1.1290
///     ( x -- x x )
ASMWORD("dup", dup)
	SPILL_TOS
	mov	pc, NEXT
	ASMEND(dup)


/// ?DUP - CORE 6.1.0630
///     ( x -- 0 | x x )
ASMWORD("?dup", question_dup)
	cmp	TOS, #0
	strne	TOS, [PSP, #-4]!	@ SPILL_TOS
	mov	pc, NEXT
	ASMEND(question_dup)


/// SWAP - CORE 6.1.2260
///     ( x1 x2 -- x2 x1 )
ASMWORD("swap", swap)
	ldr	r0, [PSP]
	str	TOS, [PSP]
	mov	TOS, r0
	mov	pc, NEXT
	ASMEND(swap)


/// OVER - CORE 6.1.1990
///     ( x1 x2 -- x1 x2 x1 )
ASMWORD("over", over)
	ldr	r0, [PSP]
	SPILL_TOS
	mov	TOS, r0
	mov	pc, NEXT
	ASMEND(over)


/// NIP - CORE EXT 6.2.1930
///     ( x1 x2 -- x2 )
ASMWORD("nip", nip)
	add	PSP, PSP, 4
	mov	pc, NEXT
	ASMEND(nip)


/// TUCK - CORE EXT 6.2.2300
///     ( x1 x2 -- x2 x1 x2 )
ASMWORD("tuck", tuck)
	ldr	r0, [PSP]
	str	TOS, [PSP]
	str     r0, [PSP, #-4]!
	mov	pc, NEXT
	ASMEND(tuck)


/// ROT - CORE 6.1.2160
///     ( x1 x2 x3 -- x2 x3 x1 )
ASMWORD("rot", rot)
	ldr	r0, [PSP]	@ x2
	ldr	r1, [PSP, #4]	@ x1
	str	TOS, [PSP]	@ x3
	str	r0, [PSP, #4]
	mov	TOS, r1
	mov	pc, NEXT
	ASMEND(rot)


/// -ROT - non-standard, inverse of ROT
///     ( x1 x2 x3 -- x3 x1 x2 )
ASMWORD("-rot", minus_rot)
	ldr	r0, [PSP]	@ x2
	ldr	r1, [PSP, #4]	@ x1
	str	TOS, [PSP, #4]	@ x3
	str	r1, [PSP]
	mov	TOS, r0
	mov	pc, NEXT
	ASMEND(minus_rot)


/// PICK - CORE EXT 6.2.2030
///     ( xu ... x1 x0 u -- xu ... x1 x0 xu )
ASMWORD("pick", pick)
	ldr     TOS, [PSP, TOS, lsl #2]
	mov	pc, NEXT
	ASMEND(pick)


/// ROLL - CORE EXT 6.2.2150
///     ( xu xu-1 ... x0 u -- xu-1 ... x0 xu )
ASMWORD("roll", roll)
	cmp	TOS, #0
	bne	0f
	FILL_TOS
	mov	pc, NEXT
0:
	ldr	r3, [PSP, TOS, lsl #2] @ x[u]
	add	r2, PSP, TOS, lsl #2   @ its address
1:
	ldr	r0, [r2, #-4]
	str	r0, [r2], #-4
	cmp	r2, PSP
	bne	1b
	add	PSP, PSP, #4
	mov	pc, NEXT
	ASMEND(roll)


/// 2DROP - CORE 6.1.0370
///     ( x1 x2 -- )
ASMWORD("2drop", two_drop)
	ldr	TOS, [PSP, #4]
	add	PSP, PSP, #8
	mov	pc, NEXT
	ASMEND(two_drop)


/// 2DUP - CORE 6.1.0380
///     ( x1 x2 -- x1 x2 x1 x2 )
ASMWORD("2dup", two_dup)
	ldr	r0, [PSP]		@ fetch x1
	str     TOS, [PSP, #-4]		@ store x2
	str	r0, [PSP, #-8]!		@ store x1, advance PSP
	mov	pc, NEXT
	ASMEND(two_dup)


/// 2SWAP - CORE 6.1.0430
///     ( x1 x2 x3 x4 -- x3 x4 x1 x2 )
ASMWORD("2swap", two_swap)
	ldm	PSP, {r0, r1, r2}
	str	TOS, [PSP, #4]		@ store x4
	str	r0, [PSP, #8]		@ store x3
	str	r2, [PSP]		@ store x1
	mov	TOS, r1			@ x2
	mov	pc, NEXT
	ASMEND(two_swap)


/// 2NIP - nonstandard
///     ( x1 x2 x3 x4 -- x3 x4 )
ASMWORD("2nip", two_nip)
	ldr	r0, [PSP]
	str	r0, [PSP, #8]!
	mov	pc, NEXT
	ASMEND(two_nip)


/// 2OVER - CORE 6.1.0400
///     ( x1 x2 x3 x4 -- x1 x2 x3 x4 x1 x2 )
ASMWORD("2over", two_over)
	ldr	r0, [PSP, #8]		@ fetch x1
	ldr	r1, [PSP, #4]		@ fetch x2
	str	TOS, [PSP, #-4]		@ store x4
	str	r0, [PSP, #-8]!		@ store x1
	mov	TOS, r1			@ x2
	mov	pc, NEXT
	ASMEND(two_over)


/// SP@ - Current stack pointer.
///     ( -- sp )
///
/// XXX: Both SP@ and SP0 don't try to hide the fact that we keep TOS
/// in a register.  This has implications in MI forth code, e.g. in .S
/// or DEPTH.
///
/// The cell just after SP0 is not part of stack - it's a place to
/// spill invalid TOS register when we push the first value onto the
/// stack.  The TOS is in a register, so to spill it into memory, DUP
/// it.
ASMWORD("sp@", sp_fetch)
	mov	r0, PSP		@ before the spill
	SPILL_TOS
	mov	TOS, r0
	mov	pc, NEXT
	ASMEND(sp_fetch)


/// SP!
ASMWORD("sp!", sp_store)
	mov	PSP, TOS
	mov	pc, NEXT
	ASMEND(sp_fetch)


/// RP@
ASMWORD("rp@", rp_fetch)
	SPILL_TOS
	mov	TOS, RSP
	mov	pc, NEXT
	ASMEND(rp_fetch)


/// RP!
ASMWORD("rp!", rp_store)
	mov	RSP, TOS
	FILL_TOS
	mov	pc, NEXT
	ASMEND(rp_fetch)


/// >R - CORE 6.1.0580
///     ( x -- ) ( R: -- x )
ASMWORD(">r", to_r)
	str	TOS, [RSP, #-4]!
	FILL_TOS
	mov	pc, NEXT
	ASMEND(to_r)


/// R> - CORE 6.1.2060
///     ( -- x ) ( R: x -- )
ASMWORD("r>", r_from)
	SPILL_TOS
	ldr	TOS, [RSP], #4
	mov	pc, NEXT
	ASMEND(r_from)


/// R@ - CORE 6.1.2070
///     ( -- x ) ( R: x -- x )
ASMWORD("r@", r_fetch)
	SPILL_TOS
	ldr	TOS, [RSP]
	mov	pc, NEXT
	ASMEND(r_fetch)


/// 2>R CORE EXT 6.2.0340
///     ( x1 x2 -- ) ( R: -- x1 x2 )
ASMWORD("2>r", two_to_r)
	ldm	PSP!, {r1, r2}
	mov	r0, TOS
	stmdb	RSP!, {r0, r1}
	mov	TOS, r2
	mov	pc, NEXT
	ASMEND(two_to_r)


/// 2R> - CORE EXT 6.2.0410
///     ( -- x1 x2 ) ( R: x1 x2 -- )
ASMWORD("2r>", two_r_from)
	ldm	RSP!, {r0, r1}
	str	TOS, [PSP, #-4]
	str	r1, [PSP, #-8]!
	mov	TOS, r0
	mov	pc, NEXT
	ASMEND(two_r_from)


/// 2R@ - CORE EXT 6.2.0415
///     ( -- x1 x2 ) ( R: x1 x2 -- x1 x2 )
ASMWORD("2r@", two_r_fetch)
	ldr	r0, [RSP, #4]
	SPILL_TOS
	ldr	TOS, [RSP]
	str	r0, [PSP, #-4]!
	mov	pc, NEXT
	ASMEND(two_r_fetch)



/**
 * Comparison
 */


/// 0= - CORE 6.1.0270
///	( x -- flag )
ASMWORD("0=", zero_equals)
	clz	TOS, TOS
	lsr	TOS, TOS, #5
	rsb	TOS, TOS, #0
	mov	pc, NEXT
	ASMEND(zero_equals)


/// 0<> - CORE-EXT 6.2.0260
///	( x -- flag )
ASMWORD("0<>", zero_not_equals)
	adds	TOS, TOS, #0
	movne	TOS, #1
	rsb	TOS, TOS, #0
	mov	pc, NEXT
	ASMEND(zero_not_equals)


/// 0> - CORE-EXT 6.2.0280
///	( x -- flag )
ASMWORD("0>", zero_greater)
	cmp	TOS, #0
	movle	TOS, #0
	movgt	TOS, #1
	rsb	TOS, TOS, #0
	mov	pc, NEXT
	ASMEND(zero_greater)


/// 0< - CORE 6.1.0250
///	( x -- flag )
ASMWORD("0<", zero_less)
	asr	TOS, TOS, #31
	mov	pc, NEXT
	ASMEND(zero_less)


/// = - CORE 6.1.0530
///     ( x1 x2 -- flag )
ASMWORD("=", equals)
	ldr	r0, [PSP], #4
	cmp	TOS, r0
	movne	TOS, #0
	mvneq	TOS, #0
	mov	pc, NEXT
	ASMEND(equals)


/// <> - CORE-EXT 6.2.0500
///     ( x1 x2 -- flag )
ASMWORD("<>", not_equals)
	ldr	r0, [PSP], #4
	subs	TOS, r0, TOS
	mvnne	TOS, #0
	mov	pc, NEXT
	ASMEND(not_equals)


/// < - CORE 6.1.0480
///     ( n1 n2 -- flag )
ASMWORD("<", less_than)
	ldr	r0, [PSP], #4
	cmp	r0, TOS
	movge	TOS, #0
	mvnlt	TOS, #0
	mov	pc, NEXT
	ASMEND(less_than)


/// > - CORE 6.1.0540
///     ( n1 n2 -- flag )
ASMWORD(">", greater_than)
	ldr	r0, [PSP], #4
	cmp	r0, TOS
	movle	TOS, #0
	mvngt	TOS, #0
	mov	pc, NEXT
	ASMEND(greater_than)


/// U< - CORE 6.1.2340
///     ( u1 u2 -- flag )
ASMWORD("u<", u_less_than)
	ldr	r0, [PSP], #4
	cmp	r0, TOS
	movcs	TOS, #0
	mvncc	TOS, #0
	mov	pc, NEXT
	ASMEND(u_less_than)


/// U> - CORE-EXT 6.2.2350
///     ( u1 u2 -- flag )
ASMWORD("u>", u_greater_than)
	ldr	r0, [PSP], #4
	cmp	r0, TOS
	movls	TOS, #0
	mvnhi	TOS, #0
	mov	pc, NEXT
	ASMEND(u_greater_than)



/**
 * Arithmetic and bit-wise operations
 */


/// FALSE - CORE-EXT 6.2.1485
///     ( -- false )
ASMWORD("false", false)
	SPILL_TOS
	mov	TOS, #0
	mov	pc, NEXT
	ASMEND(false)


/// TRUE - CORE-EXT 6.2.2298
///     ( -- true )
ASMWORD("true", true)
	SPILL_TOS
	mvn	TOS, #0
	mov	pc, NEXT
	ASMEND(true)


/// NEGATE - CORE 6.1.1910
///     ( n1 -- n2 )
ASMWORD("negate", negate)
	rsb	TOS, TOS, #0
	mov	pc, NEXT
	ASMEND(negate)


/// ABS - CORE 6.1.0690
///     ( n -- u )
ASMWORD("abs", abs)
	cmp	TOS, #0
	rsblt	TOS, TOS, #0
	mov	pc, NEXT
	ASMEND(abs)


/// + - CORE 6.1.0120
///	( n1|u1 n2|u2 -- n3|u3 )
ASMWORD("+", plus)
	ldr	r0, [PSP], #4
	add	TOS, TOS, r0
	mov	pc, NEXT
	ASMEND(plus)


/// +? - nonstandard: add with overflow check
///	used by LOOP to efficiently detect loop terminataion
///	( n1|u1 n2|u2 -- n3|u3 flag )
ASMWORD("+?", plus_question)
	ldr	r0, [PSP]
	adds	r0, TOS, r0
	mvnvs	TOS, #0
	movvc	TOS, #0
	str	r0, [PSP]
	mov	pc, NEXT
	ASMEND(plus_question)


/// - - CORE 6.1.0160
///	( n1|u1 n2|u2 -- n3|u3 )
ASMWORD("-", minus)
	ldr	r0, [PSP], #4
	sub	TOS, r0, TOS
	mov	pc, NEXT
	ASMEND(minus)


/// 1+ - CORE 6.1.0290
///     ( n1|u1 -- n2|u2 )
ASMWORD("1+", one_plus)
	add	TOS, TOS, #1
	mov	pc, NEXT
	ASMEND(one_plus)


/// 1- - CORE 6.1.0300
///     ( n1|u1 -- n2|u2 )
ASMWORD("1-", one_minus)
	sub	TOS, TOS, #1
	mov	pc, NEXT
	ASMEND(one_minus)


/// * - CORE 6.1.0090
///     ( n1|u1 n2|u2 -- n3|u3 )
ASMWORD("*", star)
	ldr	r0, [PSP], #4
	mul	TOS, r0, TOS
	mov	pc, NEXT
	ASMEND(star)


/// 2* - CORE 6.1.0320
///     ( x1 -- x2 )
ASMWORD("2*", two_star)
	lsl	TOS, TOS, #1
	mov	pc, NEXT
	ASMEND(two_star)


/// 2/ - CORE 6.1.0330
///     ( x1 -- x2 )
ASMWORD("2/", two_slash)
	add	TOS, TOS, TOS, lsr #31
	asr	TOS, TOS, #1
	mov	pc, NEXT
	ASMEND(two_slash)


/// INVERT - CORE 6.1.1720
///     ( x1 -- x2 )
ASMWORD("invert", invert)
	mvn	TOS, TOS
	mov	pc, NEXT
	ASMEND(invert)


/// AND - CORE 6.1.0720
///     ( x1 x2 -- x3 )
ASMWORD("and", and)
	ldr	r0, [PSP], #4
	and	TOS, r0, TOS
	mov	pc, NEXT
	ASMEND(and)


/// OR - CORE 6.1.1980
///     ( x1 x2 -- x3 )
ASMWORD("or", or)
	ldr	r0, [PSP], #4
	orr	TOS, r0, TOS
	mov	pc, NEXT
	ASMEND(or)


/// XOR - CORE 6.1.2490
///     ( x1 x2 -- x3 )
ASMWORD("xor", xor)
	ldr	r0, [PSP], #4
	eor	TOS, r0, TOS
	mov	pc, NEXT
	ASMEND(xor)


/// LSHIFT - CORE 6.1.1805
///     ( x1 u -- x2 )
ASMWORD("lshift", lshift)
	ldr	r0, [PSP], #4
	lsl	TOS, r0, TOS
	mov	pc, NEXT
	ASMEND(lshift)


/// RSHIFT - CORE 6.1.2162
///     ( x1 u -- x2 )
ASMWORD("rshift", rshift)
	ldr	r0, [PSP], #4
	lsr	TOS, r0, TOS
	mov	pc, NEXT
	ASMEND(rshift)



/**
 * Fetch and store
 */

/// @ - CORE 6.1.0650
///     ( a-addr -- x )
ASMWORD("@", fetch)
	ldr	TOS, [TOS]
	mov	pc, NEXT
	ASMEND(fetch)


/// ! - CORE 6.1.0010
///     ( x a-addr -- )
ASMWORD("!", store)
	ldr	r0, [PSP], #4
	str	r0, [TOS]
	FILL_TOS
	mov	pc, NEXT
	ASMEND(fetch)


/// C@ - CORE 6.1.0870
///     ( c-addr -- char )
ASMWORD("c@", c_fetch)
	ldrb	TOS, [TOS]
	mov	pc, NEXT
	ASMEND(c_fetch)


/// C! - CORE 6.1.0850
///     ( char c-addr -- )
ASMWORD("c!", c_store)
	ldr	r0, [PSP], #4
	strb	r0, [TOS]
	FILL_TOS
	mov	pc, NEXT
	ASMEND(c_store)


/// 2@ - CORE 6.1.0350
///     ( a-addr -- x1 x2 )
ASMWORD("2@", two_fetch)
	ldr	r0, [TOS, #4]		@ upper address contains LSW
	ldr	TOS, [TOS]		@ lower address contains MSW
	str	r0, [PSP, #-4]!		@ LSW goes to the upper address
	mov	pc, NEXT
	ASMEND(two_fetch)


/// 2! - CORE 6.1.0310
///     ( x1 x2 a-addr -- )
ASMWORD("2!", two_store)
	ldr	r0, [PSP]		@ lower address contains MSW
	ldr	r1, [PSP, #4]		@ upper address contains LSW
	str	r0, [TOS]		@ MSW goes to the lower address
	str	r1, [TOS, #4]		@ LSW goes to the upper address
	ldr	TOS, [PSP, #8]		@ FILL_TOS
	add	PSP, PSP, #12
	mov	pc, NEXT
	ASMEND(two_store)



/**
 * Strings
 */

/// FILL - CORE 6.1.1540
///     ( c-addr u char -- )
ASMWORD("fill", fill)
	// cheat, use memset(3)
	ldr	r0, [PSP, #4]	@ dst
	mov	r1, TOS		@ char
	ldrb	r2, [PSP]	@ len
	ldr	TOS, [PSP, #8]	@ FILL_TOS
	add	PSP, PSP, #12
	bl	memset
	mov	pc, NEXT
	ASMEND(fill)


/// MOVE - CORE 6.1.1900
///     ( src dst len -- )
ASMWORD("move", move)
	// cheat, use memmove(3)
	ldr	r0, [PSP]	@ dst
	ldr	r1, [PSP, #4]	@ src
	mov	r2, TOS		@ len
	ldr	TOS, [PSP, #8]	@ FILL_TOS
	add	PSP, PSP, #12
	bl	memmove
	mov	pc, NEXT
	ASMEND(move)



/*
 * Double-cell numbers
 */

/// S>D - CORE 6.1.2170
///     ( n -- d )
ASMWORD("s>d", s2d)
	SPILL_TOS		@ LSW goes to the upper address
	asr	TOS, TOS, #31	@ sign extend
	mov	pc, NEXT
	ASMEND(s2d)


/// D< - DOUBLE 8.6.1.1110
///     ( d1 d2 -- flag )
ASMWORD("d<", d_less_than)
	ldm	PSP!, {r0, r1, r2}	@ D2lo D1hi D1lo
	cmp	r2, r0
	sbcs	TOS, r1, TOS
	mvnlt	TOS, #0
	movge	TOS, #0
	mov	pc, NEXT
	ASMEND(d_less_than)


/// DNEGATE - DOUBLE 8.6.1.1230
///     ( d1 -- d2 )
ASMWORD("dnegate", dnegate)
	ldr	r0, [PSP]
	rsbs	r0, r0, #0
	rsc	TOS, TOS, #0
	str	r0, [PSP]
	mov	pc, NEXT
	ASMEND(dnegate)


/// DABS - DOUBLE 8.6.1.1160
///     ( d1 -- d2 )
ASMWORD("dabs", dabs)
	ldr	r0, [PSP]		@ LSW
	asr	r2, TOS, #31		@ sign
	eor	r0, r0, r2		@ LSW
	subs	r0, r0, r2
	eor	TOS, TOS, r2		@ MSW
	sbc	TOS, TOS, r2
	str	r0, [PSP]
	mov	pc, NEXT
	ASMEND(dabs)


/// D+ - DOUBLE 8.6.1.1040 
///     ( d1|ud1 d2|ud2 -- d3|ud3 )
ASMWORD("d+", d_plus)
	ldm	PSP!, {r0, r1, r2}	@ D2lo D1hi D1lo
	adds	r0, r0, r2		@ D2lo += D1lo
	adc	TOS, TOS, r1		@ D2hi += D1hi + carry
	str	r0, [PSP, #-4]!
	mov	pc, NEXT
	ASMEND(d_plus)


/// D- - DOUBLE 8.6.1.1050
///     ( d1|ud1 d2|ud2 -- d3|ud3 )
ASMWORD("d-", d_minus)
	ldm	PSP!, {r0, r1, r2}	@ D2lo D1hi D1lo
	rsbs	r0, r0, r2		@ r0  = D1lo - D2lo
	rsc	TOS, r1, TOS		@ TOS = D1hi - D2hi with carry
	str	r0, [PSP, #-4]!
	mov	pc, NEXT
	ASMEND(d_minus)


/// D2* - DOUBLE 8.6.1.1090
///     ( xd1 -- xd2 )
ASMWORD("d2*", d_two_star)
	ldr	r0, [PSP]
	adds	r0, r0, r0
	adc	TOS, TOS, TOS
	str	r0, [PSP]
	mov	pc, NEXT
	ASMEND(d_two_star)


/// D2/ - DOUBLE 8.6.1.1100
///     ( xd1 -- xd2 )
ASMWORD("d2/", d_two_slash)
	ldr	r0, [PSP]
	lsr	r0, r0, #1
	orr	r0, r0, TOS, lsl #31
	asr	TOS, TOS, #1
	str	r0, [PSP]
	mov	pc, NEXT
	ASMEND(d_two_slash)


/// M* - CORE 6.1.1810
///     ( n1 n2 -- d ) signed mixed multiplication (32x32 -> 64)
ASMWORD("m*", m_star)
	ldr	r0, [PSP]
	smull	r1, TOS, r0, TOS
	str	r1, [PSP]
	mov	pc, NEXT
	ASMEND(m_star)


/// UM* - CORE 6.1.2360
///     ( n1 n2 -- d ) unsigned mixed multiplication (32x32 -> 64)
ASMWORD("um*", um_star)
	ldr	r0, [PSP]
	umull	r1, TOS, r0, TOS
	str	r1, [PSP]
	mov	pc, NEXT
	ASMEND(um_star)



/**
 * Control structure helpers
 */

/// LIT - auxiliary for LITERAL (CORE 6.1.1780)
///   Forth version may be used, but then CELL+ must be in asm.
///   See forth definition.
ASMWORD("lit", lit)
	SPILL_TOS
	ldr	TOS, [IP], #4	@ get the operand and skip over it
	mov	pc, NEXT
	ASMEND(lit)


/// 2LIT - auxiliary for 2LITERAL (DOUBLE 8.6.1.0390)
ASMWORD("2lit", two_lit)
	ldr	r0, [IP, #4]	@ upper address contains LSW
	SPILL_TOS
	ldr	TOS, [IP]	@ lower address contains MSW
	add	IP, IP, #8	@ skip the value in the code stream
	str     r0, [PSP, #-4]!	@ LSW goes to the upper address
	mov	pc, NEXT
	ASMEND(two_lit)


/// BRANCH - auxiliary for ELSE &co
ASMWORD("branch", branch)
	ldr	IP, [IP]
	mov	pc, NEXT
	ASMEND(branch)


/// ?BRANCH - auxiliary for IF &co
ASMWORD("?branch", question_branch)
	cmp	TOS, #0
	FILL_TOS
	ldreq	IP, [IP]	@ NB: taken when TOS is false
	addne	IP, IP, #4	@ not taken, skip over target address
	mov	pc, NEXT
	ASMEND(question_branch)


/// I - CORE 6.1.1680
///	see (DO) in forth.fth for details
ASMWORD("i", i_4th)
	SPILL_TOS
	ldm	RSP, {r0, r1}	@ adjusted current, adjustment
	sub	TOS, r0, r1	@ normalize current
	mov	pc, NEXT
	ASMEND(i_4th)


/// J - CORE 6.1.1730
///	like I, but one loop level deeper (3 cells)
ASMWORD("j", j_4th)
	SPILL_TOS
	ldr	TOS, [RSP, #12]		@ adjusted current
	ldr	r1, [RSP, #16]		@ adjustment
	sub	TOS, TOS, r1		@ normalize current
	mov	pc, NEXT
	ASMEND(j_4th)



/* Random test code. */
#define TESTING
#include "test.S"

	.section ".forth"
	.size	heap, . - heap

	.section ".bss"
.L_DATA_SPACE: /* begins HERE */
.FENCE = .
	.space	4096 * 8
