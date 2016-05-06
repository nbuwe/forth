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


#define TOS	r27	/* top of parameter stack lives in a register */
#define PSP	r28	/* parameter stack pointer */
#define RSP	r29	/* return stack pointer */
#define IP	r30	/* instruction pointer */
#define NEXT	r31	/* "next" address to avoid memory loads */


### ====================================================================
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
	.space	4096		# hole
rstack_limit:
	.space	4096
rstack_bottom:

terminal_input_buffer:
	.space	4096
	.type	terminal_input_buffer, @object
	.size	terminal_input_buffer, 4096 # XXX, compute


### ====================================================================
/**
 *
 * Trampoline code callable from C that starts Forth.
 *
 */
	.text
ENTRY(start_forth)
	mflr	%r0
	stw	%r0, 4(%sp)		# save LR in caller's frame

	stwu	%sp, -128(%sp)

	## 0(%sp) - back chain
	## 4(%sp) - callee's LR save slot
	## 8(%sp) to 36(%sp) - 8 words of parameter list area

	## 40(%sp) to 48(%sp) - pad to 16 byte alignment/local variable space

	mfcr	%r0
	stw	%r0,   52(%sp)

	stw	%r14,  56(%sp)
	stw	%r15,  60(%sp)
	stw	%r16,  64(%sp)
	stw	%r17,  68(%sp)
	stw	%r18,  72(%sp)
	stw	%r19,  76(%sp)
	stw	%r20,  80(%sp)
	stw	%r21,  84(%sp)
	stw	%r22,  88(%sp)
	stw	%r23,  92(%sp)
	stw	%r24,  96(%sp)
	stw	%r25, 100(%sp)
	stw	%r26, 104(%sp)
	stw	%r27, 108(%sp)
	stw	%r28, 112(%sp)
	stw	%r29, 116(%sp)
	stw	%r30, 120(%sp)
	stw	%r31, 124(%sp)
	## end prologue

	lis	%NEXT, next_code@ha
	la	%NEXT, next_code@l(%NEXT)

	## these will be reset by ABORT
	li	%TOS, 0
	li	%PSP, 0
	li	%RSP, 0

	## ABORT to forth
	lis	%IP, abort_4th_body@ha
	la	%IP, (abort_4th_body - 4)@l(%IP)

	lwzu	%r0, 4(%IP)	# CFA of (ABORT), the first word of ABORT
	addi	%r0, %r0, 4	# PFA - the actual code
	mtctr	%r0
	bctr

.L_BYE_RETURNS_HERE:
#if 0
	SPILL_TOS
	mr	%r3, %PSP	# return forth stack pointer
#else
	lis	%r3, stack_bottom@ha
	la	%r3, (stack_bottom - 4)@l(%r3)
#endif

	## epilogue
	lwz	%r0,   52(%sp)
	mtcr	%r0

	lwz	%r14,  56(%sp)
	lwz	%r15,  60(%sp)
	lwz	%r16,  64(%sp)
	lwz	%r17,  68(%sp)
	lwz	%r18,  72(%sp)
	lwz	%r19,  76(%sp)
	lwz	%r20,  80(%sp)
	lwz	%r21,  84(%sp)
	lwz	%r22,  88(%sp)
	lwz	%r23,  92(%sp)
	lwz	%r24,  96(%sp)
	lwz	%r25, 100(%sp)
	lwz	%r26, 104(%sp)
	lwz	%r27, 108(%sp)
	lwz	%r28, 112(%sp)
	lwz	%r29, 116(%sp)
	lwz	%r30, 120(%sp)
	lwz	%r31, 124(%sp)

	addi	%sp, %sp, 128

	lwz	%r0, 4(%sp)		# restore LR saved in caller's frame
	mtlr	%r0
	blr


### ====================================================================
/**
 *
 *				FORTH
 *
 */

	.data			# XXX: need writable+executable

	.p2align 2
	.global	heap
heap:

/*
 * Fake sentinel word that starts the vocabulary.
 */
	.p2align 2, 0
	## Name Field
.LASTNFA = .		# init .LASTNFA for subsequent DEFWORDs
	.byte	0x80	# empty name, immediate (just in case?)
	## Link Field
	.p2align 2, 0
	.long	0	# terminate the LFA-linked list
	## Code Field
	.long	0	# XXX: use some sentinel code?  just crash for now
	## Parameter Field
	.long	0	# there's no parameters, but provide a stub


### Asm part of ABORT that resets parameter and return stacks
### Must be the first word called by ABORT.
ASMWORD("(abort)", abort_parens)
	lis	%RSP, rstack_bottom@ha		# reset return stack
	la	%RSP, rstack_bottom@l(%RSP)
	lis	%PSP, stack_bottom@ha		# reset parameter stack
	la	%PSP, stack_bottom@l(%PSP)
	li	%TOS, 0				# clear TOS just in case
	blr
	ASMEND(abort_parens)



### XXX: the real one is in MI forth.S, but including that will xref a
### lot of words, so provide a local copy for now

### ABORT - CORE 6.1.0670
WORD("abort", abort_4th)
	.long	abort_parens	# reset return stack, must be first
	EXIT_4TH
