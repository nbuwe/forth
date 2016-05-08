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


#define TMP	r14	/* %r0 is too magic and literal %r14 is too opaque */
#define TOS	r27	/* top of parameter stack lives in a register */
#define PSP	r28	/* parameter stack pointer */
#define RSP	r29	/* return stack pointer */
#define IP	r30	/* instruction pointer */

#define SPILL_TOS	stwu	%TOS, -4(%PSP)
#define FILL_TOS	lwz	%TOS, 0(%PSP); \
			addi	%PSP, %PSP, 4

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

	## LR is fixed with next_code so that we can get always there
        ## with just "blr".  BYE knows its return address statically
        ## (see below), so we don't need LR to record it.
	lis	%TMP, next_code@ha
	la	%TMP, next_code@l(%TMP)
	mtlr	%TMP

	## these will be reset by ABORT
	li	%TOS, 0
	li	%PSP, 0
	li	%RSP, 0

	## ABORT to forth
	lis	%IP, abort_4th_body@ha
	la	%IP, abort_4th_body@l(%IP)

	lwz	%TMP, 0(%IP)	# CFA of (ABORT), the first word of ABORT
	addi	%IP, %IP, 4
	addi	%TMP, %TMP, 4	# PFA - the actual asm code of (ABORT)
	mtctr	%TMP
	bctr

.L_BYE_RETURNS_HERE:
	SPILL_TOS
	mr	%r3, %PSP	# return forth stack pointer

	## epilogue
	lwz	%TMP,   52(%sp)
	mtcr	%TMP

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

/*
 * Address of this code will be written into the code field
 * This code will be directly called by the interpreter
 * PFA of the defined word will be in TOS
 */
#define DOES_ASM(label)		  \
	.p2align 2		; \
	.type	label@function	; \
  label:			;



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


### Asm part of BYE that returns control to the C code that started
### the Forth system.  The stacks are not reset and can be inspected.
ASMWORD("(bye)", bye_parens)
	lis	%TMP, .L_BYE_RETURNS_HERE@ha
	la	%TMP, .L_BYE_RETURNS_HERE@l(%TMP)
	mtctr	%TMP
	bctr
	ASMEND(bye_parens)


### nop to use as a gdb breakpoint
ASMWORD("breakpoint", breakpoint)
	blr
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

### EXECUTE - CORE 6.1.1370
###     ( i*x xt -- j*x )
###
### With indirect threaded code the execution token (xt) is the
### address of the code field (CFA).  See also next_code.
ASMWORD("execute", execute)
	lwz	%TMP, 0(%TOS)	# fetch code address
	addi	%TOS, %TOS, 4	# advance TOS to PFA
	mtctr	%TMP
	bctr			# call word's code on word's body
	ASMEND(execute)


### Code field of asm words points here.
### This should eventually become the DOES> part of CODE
DOES_ASM(asm_does)
	mtctr	%TOS		# PFA: word's body is its asm code
	FILL_TOS		# PFA is consumed
	bctr
	.size asm_does, .-asm_does


### Code field of colon definitions (i.e. forth words)  points here.
### This should eventually become the DOES> part of ":"
DOES_ASM(call_code)
	stwu	%IP, -4(%RSP)	# save caller's IP
	mr	%IP, %TOS	# set my IP (consume PFA from TOS)
	## don't refill TOS here, just skip the spill in next_code
	## and let next_code set vacant TOS to next word PFA
	b	.L_next_code_no_spill
	.size call_code, .-call_code


### EXIT - CORE 6.1.1380
###     ( -- ) ( R: nest-sys -- )
ASMWORD("exit", exit_4th)
	lwz	%IP, 0(%RSP)	# pop return address
	addi	%RSP, %RSP, 4
	## FALLTHROUGH to "NEXT"
next_code:
	SPILL_TOS
  .L_next_code_no_spill:	# -- entry point for call_code to jump to
	lwz	%TOS, 0(%IP)	# CFA of the next word to execute
	addi	%IP, %IP, 4	#  and advance IP
	## this is the code of EXECUTE
	lwz	%TMP, 0(%TOS)	# fetch code address
	addi	%TOS, %TOS, 4	# advance TOS to PFA
	mtctr	%TMP
	bctr			# call word's code on word's body



/**
 * Stack manipualtion
 */

### DROP - CORE 6.1.1260
###     ( x -- )
ASMWORD("drop", drop)
	FILL_TOS
	blr
	ASMEND(drop)


### DUP - CORE 6.1.1290
###     ( x -- x x )
ASMWORD("dup", dup)
	SPILL_TOS
	blr
	ASMEND(dup)


### ?DUP - CORE 6.1.0630
###     ( x -- 0 | x x )
ASMWORD("?dup", question_dup)
	cmpi	%cr0, %TOS, 0
	beq	%cr0, 0f
	SPILL_TOS
0:	blr
	ASMEND(question_dup)


### SWAP - CORE 6.1.2260
###     ( x1 x2 -- x2 x1 )
ASMWORD("swap", swap)
	lwz	%TMP, 0(%PSP)
	stw	%TOS, 0(%PSP)
	mr	%TOS, %TMP
	blr
	ASMEND(swap)


### OVER - CORE 6.1.1990
###     ( x1 x2 -- x1 x2 x1 )
ASMWORD("over", over)
	lwz	%TMP, 0(%PSP)
	SPILL_TOS
	mr	%TOS, %TMP
	blr
	ASMEND(over)


### NIP - CORE EXT 6.2.1930
###     ( x1 x2 -- x2 )
ASMWORD("nip", nip)
	addi	%PSP, %PSP, 4
	blr
	ASMEND(nip)


### TUCK - CORE EXT 6.2.2300
###     ( x1 x2 -- x2 x1 x2 )
ASMWORD("tuck", tuck)
	lwz	%TMP, 0(%PSP)
	stw	%TOS, 0(%PSP)
	stwu	%TMP, -4(%PSP)
	blr
	ASMEND(tuck)


### ROT - CORE 6.1.2160
###     ( x1 x2 x3 -- x2 x3 x1 )
ASMWORD("rot", rot)
	lwz	%TMP, 0(%PSP)	# x2
	lwz	%r0, 4(%PSP)	# x1
	stw	%TMP, 4(%PSP)
	stw	%TOS, 0(%PSP)
	mr	%TOS, %r0
	blr
	ASMEND(rot)


### -ROT - non-standard, inverse of ROT
###     ( x1 x2 x3 -- x3 x1 x2 )
ASMWORD("-rot", minus_rot)
	lwz	%TMP, 0(%PSP)	# x2
	lwz	%r0, 4(%PSP)	# x1
	stw	%TOS, 4(%PSP)
	stw	%r0, 0(%PSP)
	mr	%TOS, %TMP
	blr
	ASMEND(minus_rot)


### PICK - CORE EXT 6.2.2030
###     ( xu ... x1 x0 u -- xu ... x1 x0 xu )
ASMWORD("pick", pick)
	slwi	%TOS, %TOS, 2
	lwzx	%TOS, %PSP, %TOS
	blr
	ASMEND(pick)


### ROLL - CORE EXT 6.2.2150
###     ( xu xu-1 ... x0 u -- xu-1 ... x0 xu )
ASMWORD("roll", roll)
	cmpi	%cr0, %TOS, 0
	bne+	%cr0, 0f
	FILL_TOS	#  if u == 0, just drop it
	blr
0:
	mtctr	%TOS
	slwi	%TMP, %TOS, 2
	add	%TMP, %PSP, %TMP	# TMP = &xu;
	lwz	%TOS, 0(%TMP)		# TOS = xu;
	addi	%PSP, %PSP, 4
0:
	lwzu	%r0, -4(%TMP)
	stw	%r0, 4(%TMP)
	bdnz	0b
	blr
	ASMEND(roll)




/**
 * Arithmetic and bit-wise operations
 */

### + - CORE 6.1.0120
###     ( n1|u1 n2|u2 -- n3|u3 )
ASMWORD("+", plus)
	lwz	%TMP, 0(%PSP)
	addi	%PSP, %PSP, 4
	add	%TOS, %TOS, %TMP
	blr
	ASMEND(plus)



/**
 * Control structure helpers
 */

### LIT - auxiliary for LITERAL (CORE 6.1.1780)
ASMWORD("lit", lit)
	SPILL_TOS
	lwz	%TOS, 0(%IP)
	addi	%IP, %IP, 4
	blr
	ASMEND(lit)



##############################################################################
### XXX: the real one is in MI forth.S, but including that will xref a
### lot of words, so use a local copy and use it for testing.

### ABORT - CORE 6.1.0670
WORD("abort", abort_4th)
	.long	abort_parens	# reset return stack, must be first
	##  ... test code here ...
	.long	bye_parens
	#EXIT_4TH
