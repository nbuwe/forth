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


/// This thunk is compiled by DOES> in the defining word before the
/// threaded code for the DOES> part.  Defined word gets thunk's
/// address as its code.
#define DOES_4TH(label)		  \
  CODE(label)			; \
	/* XXX: TODO */		  \
  1:	/* threaded code follows */


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


/// Asm part of ABORT that resets parameter and return stacks
/// Must be the first word called by ABORT.
ASMWORD("(abort)", abort_parens)
	ldr	PSP, sp0_const	@ reset parameter stack
	ldr	RSP, rp0_const	@ reset return stack
	mov	TOS, #0		@ clear TOS just in case
	mov	pc, NEXT
	ASMEND(abort_parens)


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



/**
 * Arithmetic and bit-wise operations
 */


/// + - CORE 6.1.0120
///     ( n1|u1 n2|u2 -- n3|u3 )
ASMWORD("+", plus)
	ldr	r0, [PSP], #4
	add	TOS, TOS, r0
	mov	pc, NEXT
	ASMEND(plus)


/// - - CORE 6.1.0160
///     ( n1|u1 n2|u2 -- n3|u3 )
ASMWORD("-", minus)
	ldr	r0, [PSP], #4
	sub	TOS, r0, TOS
	mov	pc, NEXT
	ASMEND(minus)



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


/* Random test code. */
#define TESTING
#include "test.S"

	.section ".forth"
	.size	heap, . - heap

	.section ".bss"
.L_DATA_SPACE: /* begins HERE */
.FENCE = .
	.space	4096 * 8
