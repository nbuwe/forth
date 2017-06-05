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
#define TOS	r26	/* top of parameter stack lives in a register */
#define PSP	r27	/* parameter stack pointer */
#define RSP	r28	/* return stack pointer */
#define IP	r29	/* instruction pointer */
#define DOES	r30	/* address of does_code */
#define NEXT	r31	/* address of next_code */

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

	## LR is fixed with next_code so that we can always get there
        ## with just "blr".  BYE knows its return address statically
        ## (see below), so we don't need LR to record it.
	lis	%NEXT, next_code@ha
	la	%NEXT, next_code@l(%NEXT)
	mtlr	%NEXT

	lis	%DOES, does_code@ha
	la	%DOES, does_code@l(%DOES)

	## these will be reset by ABORT
	li	%TOS, 0
	li	%PSP, 0
	li	%RSP, 0

	## ABORT to forth
	lis	%IP, abort_body@ha
	la	%IP, abort_body@l(%IP)

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
/**
 *
 *				FORTH
 *
 */

	.data			# XXX: need writable+executable

#define CODE(label)		  \
	.p2align 2		; \
	.type	label@function	; \
  label:			;

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


/**
 * Threaded code interpreter.
 *
 * Executing a word means calling its code with the address of its
 * body at the top of the parameter stack.
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
CODE(asm_does)
	mtctr	%TOS		# PFA: word's body is its asm code
	FILL_TOS		# PFA is consumed
	bctr
	.size asm_does, .-asm_does


### Code field of C words points here.  The body contains the C
### funtion address.  Call it as PSP = (*PFA)(PSP);
CODE(c_does)
	lwz	%TOS, 0(%TOS)	# function pointer from word's body
	mr	%r3, %PSP	# pass PSP as argument
	mtctr	%TOS
	bctrl
	mr	%PSP, %r3	# return value is the new %PSP
	mtlr	%NEXT		# restore next_code
	FILL_TOS
	blr
	.size c_does, .-c_does


### Code field of colon definitions (i.e. forth words)  points here.
CODE(call_code)
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


### tail-call - note that the difference from EXIT is the order of
### CFA := @IP and IP := @RSP+
###
### This makes it possible to add a wrapper around words that examine
### the cell after them, like LIT, BRANCH, etc.  Just make the tail
### call to such word at the end of the wrapper and it will work.
ASMWORD("(goto)", goto_parens)
	SPILL_TOS
	lwz	%TOS, 0(%IP)	# CFA of the next word to execute

	lwz	%IP, 0(%RSP)	# pop return address
	addi	%RSP, %RSP, 4

	## this is the code of EXECUTE
	lwz	%TMP, 0(%TOS)	# fetch code address
	addi	%TOS, %TOS, 4	# advance TOS to PFA
	mtctr	%TMP
	bctr			# call word's code on word's body
	ASMEND(goto_parens)


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
  CODE(label)			; \
	mtctr	%DOES		; \
	bctrl			; \
	/* threaded code follows */


## Generate an instance for DOES> thunk to decouple MD asm from MI
## forth code.  DOES> can just copy this instance.
CONSTANT("does-thunk", does_thunk)
	.long	0f
0:	.long	9f - does_thunk_code
	DOES_4TH(does_thunk_code)
9:	.size	does_thunk_code, .-does_thunk_code


### Trampoline for the does-thunk.  Its address is in %DOES.
CODE(does_code)
	stwu	%IP, -4(%RSP)
	mflr	%IP		# threaded code after DOES>
	mtlr	%NEXT		# restore LR to next_code
	b	next_code
	.size	does_code, .-does_code



/**
 * Start-up and termination.
 */

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


### Asm part of ABORT that resets parameter and return stacks
### Must be the first word called by ABORT.
ASMWORD("(abort)", abort_parens)
	lis	%PSP, stack_bottom@ha		# reset parameter stack
	la	%PSP, stack_bottom@l(%PSP)
	lis	%RSP, rstack_bottom@ha		# reset return stack
	la	%RSP, rstack_bottom@l(%RSP)
	li	%TOS, 0				# clear TOS just in case
	blr
	ASMEND(abort_parens)


### Asm part of QUIT that resets return stack.
### Must be the first word called by QUIT.
ASMWORD("(quit)", quit_parens)
	lis	%RSP, rstack_bottom@ha		# reset return stack
	la	%RSP, rstack_bottom@l(%RSP)
	blr
	ASMEND(quit_parens)


### Where the stacks are.
CONSTANT("sp0", sp0)	.long	stack_bottom
CONSTANT("rp0", rp0)	.long	rstack_bottom



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


### 2DROP - CORE 6.1.0370
###     ( x1 x2 -- )
ASMWORD("2drop", two_drop)
	addi	%PSP, %PSP, 4
	FILL_TOS
	blr
	ASMEND(two_drop)


### 2DUP - CORE 6.1.0380
###     ( x1 x2 -- x1 x2 x1 x2 )
ASMWORD("2dup", two_dup)
	lwz	%TMP, 0(%PSP)		# fetch x1
	SPILL_TOS			# push x2
	stwu	%TMP, -4(%PSP)		# push x1
	blr
	ASMEND(two_dup)


### 2SWAP - CORE 6.1.0430
###     ( x1 x2 x3 x4 -- x3 x4 x1 x2 )
ASMWORD("2swap", two_swap)
	lwz	%TMP, 0(%PSP)		# fetch x3
	lwz	%r15, 4(%PSP)		# fetch x2
	lwz	%r16, 8(%PSP)		# fetch x1
	stw	%TMP, 8(%PSP)		# store x3
	stw	%TOS, 4(%PSP)		# store x4
	stw	%r16, 0(%PSP)		# store x1
	mr	%TOS, %r15		# x2
	blr
	ASMEND(two_swap)


### 2OVER - CORE 6.1.0400
###     ( x1 x2 x3 x4 -- x1 x2 x3 x4 x1 x2 )
ASMWORD("2over", two_over)
	lwz	%r15, 4(%PSP)		# fetch x2
	lwz	%r16, 8(%PSP)		# fetch x1
	SPILL_TOS			# push x4
	stwu	%r16, -4(%PSP)		# push x1
	mr	%TOS, %r15		# x2
	blr
	ASMEND(two_over)



### Current stack pointer.
###     ( -- sp )
###
### XXX: Both SP@ and SP0 don't try to hide the fact that we keep TOS
### in a register.  This has implications in MI forth code, e.g. in .S
### or DEPTH.
###
### The cell just after SP0 is not part of stack - it's a place to
### spill invalid TOS register when we push the first value onto the
### stack.  The TOS is in a register, so to spill it into memory, DUP
### it.
ASMWORD("sp@", sp_fetch)
	mr	%r0, %PSP	# before the spill
	SPILL_TOS
	mr	%TOS, %r0
	blr
	ASMEND(sp_fetch)


### SP!
ASMWORD("sp!", sp_store)
	mr	%PSP, %TOS
	blr
	ASMEND(sp_fetch)


### RP@
ASMWORD("rp@", rp_fetch)
	SPILL_TOS
	mr	%TOS, %RSP
	blr
	ASMEND(rp_fetch)


### RP!
ASMWORD("rp!", rp_store)
	mr	%RSP, %TOS
	FILL_TOS
	blr
	ASMEND(rp_fetch)


### >R - CORE 6.1.0580
###     ( x -- ) ( R: -- x )
ASMWORD(">r", to_r)
	stwu	%TOS, -4(%RSP)
	FILL_TOS
	blr
	ASMEND(to_r)


### R> - CORE 6.1.2060
###     ( -- x ) ( R: x -- )
ASMWORD("r>", r_from)
	SPILL_TOS
	lwz	%TOS, 0(%RSP)
	addi	%RSP, %RSP, 4
	blr
	ASMEND(r_from)


### R@ - CORE 6.1.2070
###     ( -- x ) ( R: x -- x )
ASMWORD("r@", r_fetch)
	SPILL_TOS
	lwz	%TOS, 0(%RSP)
	blr
	ASMEND(r_fetch)


### 2>R CORE EXT 6.2.0340
###     ( -- x1 x2 ) ( R: x1 x2 -- )
ASMWORD("2>r", two_to_r)
	lwz	%TMP, 0(%PSP)
	addi	%PSP, %PSP, 4
	stw	%TMP, -4(%RSP)
	stwu	%TOS, -8(%RSP)
	FILL_TOS
	blr
	ASMEND(two_to_r)


### 2R> - CORE EXT 6.2.0410
###     ( -- x1 x2 ) ( R: x1 x2 -- )
ASMWORD("2r>", two_r_from)
	SPILL_TOS
	lwz	%TMP, 4(%RSP)
	lwz	%TOS, 0(%RSP)
	addi	%RSP, %RSP, 8
	stwu	%TMP, -4(%PSP)
	blr
	ASMEND(two_r_from)


### 2R@ - CORE EXT 6.2.0415
###     ( -- x1 x2 ) ( R: x1 x2 -- x1 x2 )
ASMWORD("2r@", two_r_fetch)
	SPILL_TOS
	lwz	%TMP, 4(%RSP)
	lwz	%TOS, 0(%RSP)
	stwu	%TMP, -4(%PSP)
	blr
	ASMEND(two_r_fetch)



/**
 * Comparison
 */

### 0= - CORE 6.1.0270
###     ( x -- flag )
ASMWORD("0=", zero_equals)
	cmpi	%cr0, %TOS, 0
	beq-	%cr0, 1f
	li	%TOS, 0
	blr
1:
	li	%TOS, -1
	blr
	ASMEND(zero_equals)


### 0<> - CORE-EXT 6.2.0260
###     ( x -- flag )
ASMWORD("0<>", zero_not_equals)
	cmpi	%cr0, %TOS, 0
	beqlr	%cr0
	li	%TOS, -1
	blr
	ASMEND(zero_not_equals)


### 0> - CORE-EXT 6.2.0280
###     ( x -- flag )
ASMWORD("0>", zero_greater)
	cmpi	%cr0, %TOS, 0
	bgt	%cr0, 1f
	li	%TOS, 0
	blr
1:
	li	%TOS, -1
	blr
	ASMEND(zero_greater)


### 0< - CORE 6.1.0250
###     ( x -- flag )
ASMWORD("0<", zero_less)
	cmpi	%cr0, %TOS, 0
	blt	%cr0, 1f
	li	%TOS, 0
	blr
1:
	li	%TOS, -1
	blr
	ASMEND(zero_less)


### = - CORE 6.1.0530
###     ( x1 x2 -- flag )
ASMWORD("=", equals)
	lwz	%TMP, 0(%PSP)
	addi	%PSP, %PSP, 4
	cmp	%cr0, %TMP, %TOS
	beq	%cr0, 1f
	li	%TOS, 0
	blr
1:
	li	%TOS, -1
	blr
	ASMEND(equals)


### <> - CORE-EXT 6.2.0500
###     ( x1 x2 -- flag )
ASMWORD("<>", not_equals)
	lwz	%TMP, 0(%PSP)
	addi	%PSP, %PSP, 4
	cmp	%cr0, %TMP, %TOS
	bne	%cr0, 1f
	li	%TOS, 0
	blr
1:
	li	%TOS, -1
	blr
	ASMEND(not_equals)


### < - CORE 6.1.0480
###     ( n1 n2 -- flag )
ASMWORD("<", less_than)
	lwz	%TMP, 0(%PSP)
	addi	%PSP, %PSP, 4
	cmp	%cr0, %TMP, %TOS
	blt	%cr0, 1f
	li	%TOS, 0
	blr
1:
	li	%TOS, -1
	blr
	ASMEND(less_than)


### > - CORE 6.1.0540
###     ( n1 n2 -- flag )
ASMWORD(">", greater_than)
	lwz	%TMP, 0(%PSP)
	addi	%PSP, %PSP, 4
	cmp	%cr0, %TMP, %TOS
	bgt	%cr0, 1f
	li	%TOS, 0
	blr
1:
	li	%TOS, -1
	blr
	ASMEND(greater_than)


### U< - CORE 6.1.2340
###     ( u1 u2 -- flag )
ASMWORD("u<", u_less_than)
	lwz	%TMP, 0(%PSP)
	addi	%PSP, %PSP, 4
	cmpl	%cr0, %TMP, %TOS
	blt	%cr0, 1f
	li	%TOS, 0
	blr
1:
	li	%TOS, -1
	blr
	ASMEND(u_less_than)


### U> - CORE-EXT 6.2.2350
###     ( u1 u2 -- flag )
ASMWORD("u>", u_greater_than)
	lwz	%TMP, 0(%PSP)
	addi	%PSP, %PSP, 4
	cmpl	%cr0, %TMP, %TOS
	bgt	%cr0, 1f
	li	%TOS, 0
	blr
1:
	li	%TOS, -1
	blr
	ASMEND(u_greater_than)



/**
 * Arithmetic and bit-wise operations
 */

### FALSE - CORE-EXT 6.2.1485
###     ( -- false )
ASMWORD("false", false)
	SPILL_TOS
	li	%TOS, 0
	blr
	ASMEND(false)


### TRUE - CORE-EXT 6.2.2298
###     ( -- true )
ASMWORD("true", true)
	SPILL_TOS
	li	%TOS, -1
	blr
	ASMEND(true)

### NEGATE - CORE 6.1.1910
###     ( n1 -- n2 )
ASMWORD("negate", negate)
	neg	%TOS, %TOS
	blr
	ASMEND(negate)


### ABS - CORE 6.1.0690
###     ( n -- u )
ASMWORD("abs", abs)
	srawi	%TMP, %TOS, 31	# sign
	xor	%TOS, %TMP, %TOS
	subf	%TOS, %TMP, %TOS
	blr
	ASMEND(abs)


### + - CORE 6.1.0120
###     ( n1|u1 n2|u2 -- n3|u3 )
ASMWORD("+", plus)
	lwz	%TMP, 0(%PSP)
	addi	%PSP, %PSP, 4
	add	%TOS, %TOS, %TMP
	blr
	ASMEND(plus)


### +? - nonstandard: add with overflow check
###     used by LOOP to efficiently detect loop terminataion
###     ( n1|u1 n2|u2 -- n3|u3 flag )
ASMWORD("+?", plus_question)
	lwz	%TMP, 0(%PSP)
	addo	%TOS, %TOS, %TMP
	mcrxr	%cr0			# copy xer[0:3]; XER[ov] -> CR0[gt]
	stw	%TOS, 0(%PSP)
	bgt	%cr0, 1f
	li	%TOS, 0
	blr
1:
	li	%TOS, -1
	blr
	ASMEND(plus_question)


### - - CORE 6.1.0160
###     ( n1|u1 n2|u2 -- n3|u3 )
ASMWORD("-", minus)
	lwz	%TMP, 0(%PSP)
	addi	%PSP, %PSP, 4
	subf	%TOS, %TOS, %TMP
	blr
	ASMEND(minus)


### 1+ - CORE 6.1.0290
###     ( n1|u1 -- n2|u2 )
ASMWORD("1+", one_plus)
	addi	%TOS, %TOS, 1
	blr
	ASMEND(one_plus)


### 1- - CORE 6.1.0300
###     ( n1|u1 -- n2|u2 )
ASMWORD("1-", one_minus)
	addi	%TOS, %TOS, -1
	blr
	ASMEND(one_minus)


### * - CORE 6.1.0090
###     ( n1|u1 n2|u2 -- n3|u3 )
ASMWORD("*", star)
	lwz	%TMP, 0(%PSP)
	addi	%PSP, %PSP, 4
	mullw	%TOS, %TOS, %TMP
	blr
	ASMEND(star)


### / - CORE 6.1.0230
###     NB: symmetric division
###     ( n1 n2 -- n3 )
ASMWORD("/", slash)
	lwz	%TMP, 0(%PSP)
	addi	%PSP, %PSP, 4
	divw	%TOS, %TMP, %TOS
	blr
	ASMEND(slash)


### U/ - nonstandard unsigned division
###     ( u1 u2 -- u3 )
ASMWORD("u/", u_slash)
	lwz	%TMP, 0(%PSP)
	addi	%PSP, %PSP, 4
	divwu	%TOS, %TMP, %TOS
	blr
	ASMEND(u_slash)


### 2* - CORE 6.1.0320
###     ( x1 -- x2 )
ASMWORD("2*", two_star)
	slwi	%TOS, %TOS, 1
	blr
	ASMEND(two_star)


### 2/ - CORE 6.1.0330
###     ( x1 -- x2 )
ASMWORD("2/", two_slash)
	srawi	%TOS, %TOS, 1
	blr
	ASMEND(two_slash)


### INVERT - CORE 6.1.1720
###     ( x1 -- x2 )
ASMWORD("invert", invert)
	not	%TOS, %TOS
	blr
	ASMEND(invert)


### AND - CORE 6.1.0720
###     ( x1 x2 -- x3 )
ASMWORD("and", and)
	lwz	%TMP, 0(%PSP)
	addi	%PSP, %PSP, 4
	and	%TOS, %TOS, %TMP
	blr
	ASMEND(and)


### OR - CORE 6.1.1980
###     ( x1 x2 -- x3 )
ASMWORD("or", or)
	lwz	%TMP, 0(%PSP)
	addi	%PSP, %PSP, 4
	or	%TOS, %TOS, %TMP
	blr
	ASMEND(or)


### XOR - CORE 6.1.2490
###     ( x1 x2 -- x3 )
ASMWORD("xor", xor)
	lwz	%TMP, 0(%PSP)
	addi	%PSP, %PSP, 4
	xor	%TOS, %TOS, %TMP
	blr
	ASMEND(xor)


## LSHIFT - CORE 6.1.1805
###     ( x1 u -- x2 )
ASMWORD("lshift", lshift)
	lwz	%TMP, 0(%PSP)
	addi	%PSP, %PSP, 4
	slw	%TOS, %TMP, %TOS
	blr
	ASMEND(lshift)


## RSHIFT - CORE 6.1.2162
###     ( x1 u -- x2 )
ASMWORD("rshift", rshift)
	lwz	%TMP, 0(%PSP)
	addi	%PSP, %PSP, 4
	srw	%TOS, %TMP, %TOS
	blr
	ASMEND(rshift)



/**
 * Fetch and store
 */

### @ - CORE 6.1.0650
###     ( a-addr -- x )
ASMWORD("@", fetch)
	lwz	%TOS, 0(%TOS)
	blr
	ASMEND(fetch)


### ! - CORE 6.1.0010
###     ( x a-addr -- )
ASMWORD("!", store)
	lwz	%TMP, 0(%PSP)
	addi	%PSP, %PSP, 4
	stw	%TMP, 0(%TOS)
	FILL_TOS
	blr
	ASMEND(fetch)


### C@ - CORE 6.1.0870
###     ( c-addr -- char )
ASMWORD("c@", c_fetch)
	lbz	%TOS, 0(%TOS)
	blr
	ASMEND(c_fetch)


### C! - CORE 6.1.0850
###     ( char c-addr -- )
ASMWORD("c!", c_store)
	lwz	%TMP, 0(%PSP)
	addi	%PSP, %PSP, 4
	stb	%TMP, 0(%TOS)
	FILL_TOS
	blr
	ASMEND(c_store)


### 2@ - CORE 6.1.0350
###     ( a-addr -- x1 x2 )
ASMWORD("2@", two_fetch)
	lwz	%TMP, 4(%TOS)
	lwz	%TOS, 0(%TOS)	# lower address goes to the top
	stwu	%TMP, -4(%PSP)
	blr
	ASMEND(two_fetch)


### 2! - CORE 6.1.0310
###     ( x1 x2 a-addr -- )
ASMWORD("2!", two_store)
	lwz	%TMP, 0(%PSP)
	lwz	%r15, 4(%PSP)
	stw	%TMP, 0(%TOS)	# top -> lower address
	stw	%r15, 4(%TOS)
	lwz	%TOS, 8(%PSP)	# fill tos
	addi	%PSP, %PSP, 12
	blr
	ASMEND(two_store)



/**
 * Strings
 */

### FILL - CORE 6.1.1540
###     ( c-addr u char -- )
ASMWORD("fill", fill)
	## cheat, use memset(3)
	lwz	%r3, 4(%PSP)	# dst
	mr	%r4, %TOS	# char
	lwz	%r5, 0(%PSP)	# len
	lwz	%TOS, 8(%PSP)	# fill tos
	addi	%PSP, %PSP, 12

	lis	%TMP, memset@ha
	la	%TMP, memset@l(%TMP)
	mtctr	%TMP
	bctrl
	mtlr	%NEXT
	blr
	ASMEND(fill)


### MOVE - CORE 6.1.1900
###     ( src dst len -- )
ASMWORD("move", move)
	## cheat, use memmove(3)
	lwz	%r3, 0(%PSP)	# dst
	lwz	%r4, 4(%PSP)	# src
	mr	%r5, %TOS	# len
	lwz	%TOS, 8(%PSP)	# fill tos
	addi	%PSP, %PSP, 12

	lis	%TMP, memmove@ha
	la	%TMP, memmove@l(%TMP)
	mtctr	%TMP
	bctrl
	mtlr	%NEXT
	blr
	ASMEND(move)



/*
 * Double-cell numbers
 */

### S>D - CORE 6.1.2170
###     ( n -- d )
ASMWORD("s>d", s2d)
	SPILL_TOS		# least significant cell is below
	srawi	%TOS, %TOS, 31	# sign extend
	blr
	ASMEND(s2d)


### D< - DOUBLE 8.6.1.1110
###     ( d1 d2 -- flag )
ASMWORD("d<", d_less_than)
#	define	D1lo	r20
#	define	D1hi	r21
#	define	D2lo	r22
	#	D2hi	TOS
	lwz	%D2lo, 0(%PSP)
	lwz	%D1hi, 4(%PSP)
	lwz	%D1lo, 8(%PSP)
	addi	%PSP, %PSP, 12
	cmp	%cr0, %D1hi, %TOS
	li	%TOS, -1
	bltlr	%cr0
	beq	%cr0, .L_dlt_check_lo
	li	%TOS, 0
	blr
.L_dlt_check_lo:
	cmpl	%cr0, %D1lo, %D2lo
	bltlr	%cr0		# TOS is already -1
	li	%TOS, 0
	blr
	ASMEND(d_less_than)


### DNEGATE - DOUBLE 8.6.1.1230
###     ( d1 -- d2 )
ASMWORD("dnegate", dnegate)
	lwz	%TMP, 0(%PSP)
	subfic	%TMP, %TMP, 0
	subfze	%TOS, %TOS
	stw	%TMP, 0(%PSP)
	blr
	ASMEND(dnegate)


### DABS - DOUBLE 8.6.1.1160
###     ( d1 -- d2 )
ASMWORD("dabs", dabs)
	lwz	%TMP, 0(%PSP)		# lo
	srawi	%r16, %TOS, 31		# sign
	srawi	%r15, %TOS, 31		# sign
	xor	%TMP, %r16, %TMP	# lo
	xor	%TOS, %r15, %TOS	# hi
	subfc	%TMP, %r16, %TMP	# lo
	subfe	%TOS, %r15, %TOS	# hi
	stw	%TMP, 0(%PSP)
	blr
	ASMEND(dabs)


### D+ - DOUBLE 8.6.1.1040 
###     ( d1|ud1 d2|ud2 -- d3|ud3 )
ASMWORD("d+", d_plus)
#	define	D1lo	r20
#	define	D1hi	r21
#	define	D2lo	r22
	#	D2hi	TOS
	lwz	%D1lo, 8(%PSP)
	lwz	%D2lo, 0(%PSP)
	lwz	%D1hi, 4(%PSP)
	addc	%D2lo, %D2lo, %D1lo
	adde	%TOS, %TOS, %D1hi
	stw	%D2lo, 8(%PSP)
	addi	%PSP, %PSP, 8
	blr
	ASMEND(d_plus)
#undef	D1lo
#undef	D1hi
#undef	D2lo


### D- - DOUBLE 8.6.1.1050
###     ( d1|ud1 d2|ud2 -- d3|ud3 )
ASMWORD("d-", d_minus)
#	define	D1lo	r20
#	define	D1hi	r21
#	define	D2lo	r22
	#	D2hi	TOS
	lwz	%D1lo, 8(%PSP)
	lwz	%D2lo, 0(%PSP)
	lwz	%D1hi, 4(%PSP)
	subfc	%D2lo, %D2lo, %D1lo
	subfe	%TOS, %TOS, %D1hi
	stw	%D2lo, 8(%PSP)
	addi	%PSP, %PSP, 8
	blr
	ASMEND(d_minus)
#undef	D1lo
#undef	D1hi
#undef	D2lo


### D2* - DOUBLE 8.6.1.1090
###     ( xd1 -- xd2 )
ASMWORD("d2*", d_two_star)
#	define	LO	r20
	lwz	%LO, 0(%PSP)
	srwi	%TMP, %LO, 31	# upper bit of lo
	slwi	%TOS, %TOS, 1
	slwi	%LO, %LO, 1
	or	%TOS, %TOS, %TMP # carry over
	stw	%LO, 0(%PSP)
	blr
	ASMEND(d_two_star)
#undef	LO


### D2/ - DOUBLE 8.6.1.1100
###     ( xd1 -- xd2 )
ASMWORD("d2/", d_two_slash)
#	define	LO	r20
	lwz	%LO, 0(%PSP)
	slwi	%TMP, %TOS, 31	# lower bit of hi
	srwi	%LO, %LO, 1
	srwi	%TOS, %TOS, 1
	or	%LO, %LO, %TMP # carry over
	stw	%LO, 0(%PSP)
	blr
	ASMEND(d_two_slash)
#undef	LO


### M* - CORE 6.1.1810
###     ( n1 n2 -- d ) signed mixed multiplication (32x32 -> 64)
ASMWORD("m*", m_star)
#	define	LO	r20
	lwz	%TMP, 0(%PSP)
	mullw	%LO, %TMP, %TOS
	mulhw	%TOS, %TMP, %TOS
	stw	%LO, 0(%PSP)
	blr
	ASMEND(m_star)
#undef	LO


### UM* - CORE 6.1.2360
###     ( n1 n2 -- d ) unsigned mixed multiplication (32x32 -> 64)
ASMWORD("um*", um_star)
#	define	LO	r20
	lwz	%TMP, 0(%PSP)
	mullw	%LO, %TMP, %TOS
	mulhwu	%TOS, %TMP, %TOS
	stw	%LO, 0(%PSP)
	blr
	ASMEND(m_star)
#undef	LO



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


### 2LIT - auxiliary for 2LITERAL (DOUBLE 8.6.1.0390)
ASMWORD("2lit", two_lit)
	SPILL_TOS
	lwz	%TOS, 0(%IP)	# lower address goes to the top
	lwz	%TMP, 4(%IP)
	stwu	%TMP, -4(%PSP)
	addi	%IP, %IP, 8
	blr
	ASMEND(two_lit)


### BRANCH - auxiliary for ELSE &co
ASMWORD("branch", branch)
	lwz	%IP, 0(%IP)
	blr
	ASMEND(branch)


### ?BRANCH - auxiliary for IF &co
ASMWORD("?branch", question_branch)
	cmpi	%cr0, %TOS, 0
	FILL_TOS
	beq	%cr0, .L_branch_taken	# NB: taken when TOS is false
	addi	%IP, %IP, 4	# not taken, skip over target address
	blr
.L_branch_taken:
	lwz	%IP, 0(%IP)
	blr
	ASMEND(question_branch)


### I - CORE 6.1.1680
ASMWORD("i", i_4th)
	SPILL_TOS
	lwz	%TOS, 0(%RSP)		# adjusted current
	lwz	%TMP, 4(%RSP)		# adjustment
	subf	%TOS, %TMP, %TOS	# normalize current
	blr
	ASMEND(i_4th)


### J - CORE 6.1.1730
ASMWORD("j", j_4th)
	SPILL_TOS
	lwz	%TOS, 12(%RSP)		# adjusted current
	lwz	%TMP, 16(%RSP)		# adjustment
	subf	%TOS, %TMP, %TOS	# normalize current
	blr
	ASMEND(i_4th)


### XXX: nothing to do on ppc, just 2DROP the args
ASMWORD("sync-i$", sync_icache)
	addi	%PSP, %PSP, 4
	FILL_TOS
	blr
	ASMEND(breakpoint)


/* Machine-independent words in threaded code. */
#include "forth.S"

/* Random test code. */
#include "test.S"

	.data
### value for CURRENT
.LATEST = .LASTNFA

	.size	heap, . - heap

	.section ".bss"
.L_DATA_SPACE: /* begins HERE */
.FENCE = .
	.space	4096 * 8
