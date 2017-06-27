/*
 * Copyright (c) 2009-2017 Valery Ushakov
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


#define W	eax	/* "work" register */
#define TMP	edx	/* extra temporary register */
#define TOS	ecx	/* top of parameter stack lives in a register */
#define PSP	edi	/* parameter stack pointer */
#define RSP	esi	/* return stack pointer */
#define IP	ebx	/* instruction pointer */

/*
 * In the kernel %esp is used for interrupts and exceptions, so we
 * can't use it for Forth's PSP.
 */
#define SPILL_TOS	mov %TOS, -4(%PSP);  add $-4, %PSP
#define FILL_TOS	mov (%PSP), %TOS;  add $4, %PSP

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
	push	%ebp
	mov	%esp, %ebp
	push	%edi
	push	%esi
	push	%ebx
	## end prologue

	## these will be reset by ABORT
	xor	%TOS, %TOS
	xor	%PSP, %PSP
	xor	%RSP, %RSP

	## ABORT to forth
	lea	abort_body + 4, %IP
	mov	-4(%IP), %W	# CFA of (ABORT), the first word of ABORT
	add	$4, %W		# PFA - the actual asm code of (ABORT)
	jmp	*%W

.L_BYE_RETURNS_HERE:
	SPILL_TOS
	mov	%PSP, %eax	# return forth stack pointer

	## epilogue
	pop	%ebx
	pop	%esi
	pop	%edi
	leave
	ret



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

#define CURRENT forth
VOCABULARY("forth", forth)


/**
 * Threaded code interpreter.
 * NB: W is used to pass CFA.
 */

### Code field of colon definitions (i.e. forth words)  points here.
CODE(call_code)
	mov	%IP, -4(%RSP)	# save caller's IP
	add	$-4, %RSP
	lea	4(%W), %IP	# set my IP to PFA
	jmp	next_code
	.size call_code, .-call_code


### EXIT - CORE 6.1.1380
###     ( -- ) ( R: nest-sys -- )
ASMWORD("exit", exit_4th)
	mov	(%RSP), %IP	# pop return address
	add	$4, %RSP
	## FALLTHROUGH to "NEXT"
next_code:
	mov	(%IP), %W	# W = CFA of the next word to execute
	add	$4, %IP		#  and advance IP
	jmp	*(%W)		# call word's code on word's body


### tail-call - note that the difference from EXIT is the order of
### CFA := @IP and IP := @RSP+
###
### This makes it possible to add a wrapper around words that examine
### the cell after them, like LIT, BRANCH, etc.  Just make the tail
### call to such word at the end of the wrapper and it will work.
ASMWORD("(goto)", goto_parens)
	mov	(%IP), %W	# W = CFA of the next word to execute
	mov	(%RSP), %IP	# pop return address
	add	$4, %RSP
	jmp	*(%W)		# call word's code on word's body
	ASMEND(goto_parens)


### Code field of C words points here.  The body contains the C
### function address.  Call it as PSP = (*PFA)(PSP);
CODE(c_does)
	mov	4(%W), %W	# function pointer from word's body
	SPILL_TOS		# for the function to see it
	push	%PSP		# pass PSP as argument
	call	*%W
	mov	%eax, %PSP	# return value is the new %PSP
	FILL_TOS
	jmp	next_code
	.size c_does, .-c_does


### Default code field of CREATE'd words: push PFA
### This is equivalent to DOES> ( nothing ) ;
CODE(var_does)
	SPILL_TOS
	lea	4(%W), %TOS	# push PFA
	jmp	next_code
	.size var_does, .-var_does


### This thunk is compiled by DOES> in the defining word before the
### threaded code for the DOES> part.  Defined word gets thunk's
### address as its code.
#define DOES_4TH(label)		  \
  CODE(label)			; \
	call	*91f		; \
  90:	.p2align 2		; \
  91:	.long	does_code	; \
  99:	/* threaded code follows */


## Generate an instance for DOES> thunk to decouple MD asm from MI
## forth code.  DOES> can just copy this instance.
CONSTANT("does-thunk", does_thunk)
	.long	0f
0:	.long	9f - does_thunk_code
	DOES_4TH(does_thunk_code)
9:	.size	does_thunk_code, .-does_thunk_code


### Trampoline for the does-thunk.  Its address is in %DOES.
CODE(does_code)
	SPILL_TOS
	lea	4(%W), %TOS	# push PFA
	mov	%IP, -4(%RSP)	# save caller's IP
	add	$-4, %RSP
	pop	%IP			# inside does_thunk_code
	add	$(99b - 90b), %IP	# threaded code after DOES>
	jmp	next_code
	.size	does_code, .-does_code


### EXECUTE - CORE 6.1.1370
###     ( i*x xt -- j*x )
ASMWORD("execute", execute)
	mov	%TOS, %W	# set W to CFA
	FILL_TOS
	jmp	*(%W)		# call word's code on word's body
	ASMEND(execute)



/**
 * Start-up and termination.
 */

### Asm part of BYE that returns control to the C code that started
### the Forth system.  The stacks are not reset and can be inspected.
ASMWORD("(bye)", bye_parens)
	jmp	.L_BYE_RETURNS_HERE
	ASMEND(bye_parens)


### nop to use as a gdb breakpoint
ASMWORD("breakpoint", breakpoint)
	jmp	next_code
	ASMEND(breakpoint)


### Asm part of ABORT that resets parameter and return stacks
### Must be the first word called by ABORT.
ASMWORD("(abort)", abort_parens)
	lea	stack_bottom, %PSP		# reset parameter stack
	lea	rstack_bottom, %RSP		# reset return stack
	xor	%TOS, %TOS			# clear TOS just in case
	jmp	next_code
	ASMEND(abort_parens)


### Asm part of QUIT that resets return stack.
### Must be the first word called by QUIT.
ASMWORD("(quit)", quit_parens)
	lea	rstack_bottom, %RSP		# reset return stack
	jmp	next_code
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
	jmp	next_code
	ASMEND(drop)


### DUP - CORE 6.1.1290
###     ( x -- x x )
ASMWORD("dup", dup)
	SPILL_TOS
	jmp	next_code
	ASMEND(dup)


### ?DUP - CORE 6.1.0630
###     ( x -- 0 | x x )
ASMWORD("?dup", question_dup)
	test	%TOS, %TOS
	jz	next_code
	SPILL_TOS
	jmp	next_code
	ASMEND(question_dup)


### SWAP - CORE 6.1.2260
###     ( x1 x2 -- x2 x1 )
ASMWORD("swap", swap)
	mov	(%PSP), %W
	mov	%TOS, (%PSP)
	mov	%W, %TOS
	jmp	next_code
	ASMEND(swap)


### OVER - CORE 6.1.1990
###     ( x1 x2 -- x1 x2 x1 )
ASMWORD("over", over)
	mov	(%PSP), %W
	SPILL_TOS
	mov	%W, %TOS
	jmp	next_code
	ASMEND(over)


### NIP - CORE EXT 6.2.1930
###     ( x1 x2 -- x2 )
ASMWORD("nip", nip)
	add	$4, %PSP
	jmp	next_code
	ASMEND(nip)


### TUCK - CORE EXT 6.2.2300
###     ( x1 x2 -- x2 x1 x2 )
ASMWORD("tuck", tuck)
	mov	(%PSP), %W
	mov	%TOS, (%PSP)
	mov	%W, -4(%PSP)
	add	$-4, %PSP
	jmp	next_code
	ASMEND(tuck)


### ROT - CORE 6.1.2160
###     ( x1 x2 x3 -- x2 x3 x1 )
ASMWORD("rot", rot)
	mov	(%PSP), %W	# x2
	mov	4(%PSP), %TMP	# x1
	mov	%W, 4(%PSP)
	mov	%TOS, (%PSP)
	mov	%TMP, %TOS
	jmp	next_code
	ASMEND(rot)


### -ROT - non-standard, inverse of ROT
###     ( x1 x2 x3 -- x3 x1 x2 )
ASMWORD("-rot", minus_rot)
	mov	(%PSP), %W	# x2
	mov	4(%PSP), %TMP	# x1
	mov	%TOS, 4(%PSP)
	mov	%TMP, (%PSP)
	mov	%W, %TOS
	jmp	next_code
	ASMEND(minus_rot)


### PICK - CORE EXT 6.2.2030
###     ( xu ... x1 x0 u -- xu ... x1 x0 xu )
ASMWORD("pick", pick)
	mov	(%PSP, %TOS, 4), %TOS
	jmp	next_code
	ASMEND(pick)


### ROLL - CORE EXT 6.2.2150
###     ( xu xu-1 ... x0 u -- xu-1 ... x0 xu )
ASMWORD("roll", roll)
	test	%TOS, %TOS
	jnz	0f
	FILL_TOS		#  if u == 0, just drop it
	jmp	next_code
0:
	movl	(%PSP, %TOS, 4), %W	# xu
1:
	sub	$1, %TOS		# from u-1 to 0
	movl	(%PSP, %TOS, 4), %TMP	#  fetch xi
	movl	%TMP, 4(%PSP, %TOS, 4)	#  store xi+1
	test	%TOS, %TOS
	jnz	1b
	add	$4, %PSP
	mov	%W, %TOS
	jmp	next_code
	ASMEND(roll)


### 2DROP - CORE 6.1.0370
###     ( x1 x2 -- )
ASMWORD("2drop", two_drop)
	add	$4, %PSP
	FILL_TOS
	jmp	next_code
	ASMEND(two_drop)


### 2DUP - CORE 6.1.0380
###     ( x1 x2 -- x1 x2 x1 x2 )
ASMWORD("2dup", two_dup)
	mov	(%PSP), %W
	mov	%TOS, -4(%PSP)
	mov	%W, -8(%PSP)
	add	$-8, %PSP
	jmp	next_code
	ASMEND(two_dup)


### 2SWAP - CORE 6.1.0430
###     ( x1 x2 x3 x4 -- x3 x4 x1 x2 )
ASMWORD("2swap", two_swap)
	mov	8(%PSP), %W	# swap x1
	mov	(%PSP), %TMP	#  and x3
	mov	%W, (%PSP)
	mov	%TMP, 8(%PSP)
	mov	4(%PSP), %W	# swap x2
	mov	%TOS, 4(%PSP)	#  and x4
	mov	%W, %TOS
	jmp	next_code
	ASMEND(two_swap)


### 2OVER - CORE 6.1.0400
###     ( x1 x2 x3 x4 -- x1 x2 x3 x4 x1 x2 )
ASMWORD("2over", two_over)
	mov	%TOS, -4(%PSP)	# x4
	mov	8(%PSP), %W	# x1
	mov	4(%PSP), %TOS	# x2
	mov	%W, -8(%PSP)
	add	$-8, %PSP
	jmp	next_code
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
	mov	%PSP, %W	# before the spill
	SPILL_TOS
	mov	%W, %TOS
	jmp	next_code
	ASMEND(sp_fetch)


### SP!
ASMWORD("sp!", sp_store)
	mov	%TOS, %PSP
	jmp	next_code
	ASMEND(sp_fetch)


### RP@
ASMWORD("rp@", rp_fetch)
	SPILL_TOS
	mov	%RSP, %TOS
	jmp	next_code
	ASMEND(rp_fetch)


### RP!
ASMWORD("rp!", rp_store)
	mov	%TOS, %RSP
	FILL_TOS
	jmp	next_code
	ASMEND(rp_fetch)


### >R - CORE 6.1.0580
###     ( x -- ) ( R: -- x )
ASMWORD(">r", to_r)
	mov	%TOS, -4(%RSP)
	add	$-4, %RSP
	FILL_TOS
	jmp	next_code
	ASMEND(to_r)


### R> - CORE 6.1.2060
###     ( -- x ) ( R: x -- )
ASMWORD("r>", r_from)
	SPILL_TOS
	mov	(%RSP), %TOS
	add	$4, %RSP
	jmp	next_code
	ASMEND(r_from)


### R@ - CORE 6.1.2070
###     ( -- x ) ( R: x -- x )
ASMWORD("r@", r_fetch)
	SPILL_TOS
	mov	(%RSP), %TOS
	jmp	next_code
	ASMEND(r_fetch)


### 2>R CORE EXT 6.2.0340
###     ( x1 x2 -- ) ( R: -- x1 x2 )
ASMWORD("2>r", two_to_r)
	mov	(%PSP), %W
	mov	%TOS, -8(%RSP)
	mov	%W, -4(%RSP)
	add	$-8, %RSP
	mov	4(%PSP), %TOS
	add	$8, %PSP
	jmp	next_code
	ASMEND(two_to_r)


### 2R> - CORE EXT 6.2.0410
###     ( -- x1 x2 ) ( R: x1 x2 -- )
ASMWORD("2r>", two_r_from)
	mov	4(%RSP), %W
	mov	%TOS, -4(%PSP)
	mov	%W, -8(%PSP)
	mov	(%RSP), %TOS
	add	$8, %RSP
	add	$-8, %PSP
	jmp	next_code
	ASMEND(two_r_from)


### 2R@ - CORE EXT 6.2.0415
###     ( -- x1 x2 ) ( R: x1 x2 -- x1 x2 )
ASMWORD("2r@", two_r_fetch)
	mov	4(%RSP), %W
	mov	%TOS, -4(%PSP)
	mov	%W, -8(%PSP)
	mov	(%RSP), %TOS
	add	$-8, %PSP
	jmp	next_code
	ASMEND(two_r_fetch)



/**
 * Comparison
 */

#define CMP0(cc)				  \
	xor	%W, %W				; \
	test	%TOS, %TOS			; \
	__CONCAT(set,cc) %al /* lsb of %W */	; \
	neg	%W				; \
	mov	%W, %TOS

### 0= - CORE 6.1.0270
###     ( x -- flag )
ASMWORD("0=", zero_equals)
	CMP0(z)
	jmp	next_code
	ASMEND(zero_equals)


### 0<> - CORE-EXT 6.2.0260
###     ( x -- flag )
ASMWORD("0<>", zero_not_equals)
	CMP0(nz)
	jmp	next_code
	ASMEND(zero_not_equals)


### 0> - CORE-EXT 6.2.0280
###     ( x -- flag )
ASMWORD("0>", zero_greater)
	CMP0(g)
	jmp	next_code
	ASMEND(zero_greater)


### 0< - CORE 6.1.0250
###     ( x -- flag )
ASMWORD("0<", zero_less)
	sar	$31, %TOS	# fill with the sign bit
	jmp	next_code
	ASMEND(zero_less)


#define CMP(cc)					  \
	cmp	%TOS, (%PSP)			; \
	__CONCAT(set,cc) %cl /* lsb of %TOS */	; \
	movzbl	%cl, %TOS			; \
	add	$4, %PSP			; \
	neg	%TOS

### = - CORE 6.1.0530
###     ( x1 x2 -- flag )
ASMWORD("=", equals)
	CMP(e)
	jmp	next_code
	ASMEND(equals)


### <> - CORE-EXT 6.2.0500
###     ( x1 x2 -- flag )
ASMWORD("<>", not_equals)
	CMP(ne)
	jmp	next_code
	ASMEND(not_equals)


### < - CORE 6.1.0480
###     ( n1 n2 -- flag )
ASMWORD("<", less_than)
	CMP(l)
	jmp	next_code
	ASMEND(less_than)


### > - CORE 6.1.0540
###     ( n1 n2 -- flag )
ASMWORD(">", greater_than)
	CMP(g)
	jmp	next_code
	ASMEND(greater_than)


### U< - CORE 6.1.2340
###     ( u1 u2 -- flag )
ASMWORD("u<", u_less_than)
	cmp	%TOS, (%PSP)
	sbb	%TOS, %TOS
	add	$4, %PSP
	jmp	next_code
	ASMEND(u_less_than)


### U> - CORE-EXT 6.2.2350
###     ( u1 u2 -- flag )
ASMWORD("u>", u_greater_than)
	cmp	(%PSP), %TOS
	sbb	%TOS, %TOS
	add	$4, %PSP
	jmp	next_code
	ASMEND(u_greater_than)



/**
 * Arithmetic and bit-wise operations
 */

### FALSE - CORE-EXT 6.2.1485
###     ( -- false )
ASMWORD("false", false)
	SPILL_TOS
	xor	%TOS, %TOS
	jmp	next_code
	ASMEND(false)


### TRUE - CORE-EXT 6.2.2298
###     ( -- true )
ASMWORD("true", true)
	SPILL_TOS
	mov	$-1, %TOS
	jmp	next_code
	ASMEND(true)

### NEGATE - CORE 6.1.1910
###     ( n1 -- n2 )
ASMWORD("negate", negate)
	neg	%TOS
	jmp	next_code
	ASMEND(negate)


### ABS - CORE 6.1.0690
###     ( n -- u )
ASMWORD("abs", abs)
	mov	%TOS, %eax		# W
	cltd	# eax to { edx, eax }	# i.e. to { W, TMP }
	xor	%edx, %eax
	sub	%edx, %eax
	mov	%eax, %TOS
	jmp	next_code
	ASMEND(abs)


### + - CORE 6.1.0120
###     ( n1|u1 n2|u2 -- n3|u3 )
ASMWORD("+", plus)
	add	(%PSP), %TOS
	add	$4, %PSP
	jmp	next_code
	ASMEND(plus)


### +? - nonstandard: add with overflow check
###     used by LOOP to efficiently detect loop terminataion
###     ( n1|u1 n2|u2 -- n3|u3 flag )
ASMWORD("+?", plus_question)
	xor	%W, %W
	add	%TOS, (%PSP)
	seto	%al		# lsb of %W
	neg	%W
	mov	%W, %TOS
	jmp	next_code
	ASMEND(plus_question)


### - - CORE 6.1.0160
###     ( n1|u1 n2|u2 -- n3|u3 )
ASMWORD("-", minus)
	mov	%TOS, %W
	mov	(%PSP), %TOS
	add	$4, %PSP
	sub	%W, %TOS
	jmp	next_code
	ASMEND(minus)


### 1+ - CORE 6.1.0290
###     ( n1|u1 -- n2|u2 )
ASMWORD("1+", one_plus)
	inc	%TOS
	jmp	next_code
	ASMEND(one_plus)


### 1- - CORE 6.1.0300
###     ( n1|u1 -- n2|u2 )
ASMWORD("1-", one_minus)
	dec	%TOS
	jmp	next_code
	ASMEND(one_minus)


### * - CORE 6.1.0090
###     ( n1|u1 n2|u2 -- n3|u3 )
ASMWORD("*", star)
	imul	(%PSP), %TOS
	add	$4, %PSP
	jmp	next_code
	ASMEND(star)


### / - CORE 6.1.0230
###     NB: symmetric division
###     ( n1 n2 -- n3 )
ASMWORD("/", slash)
	mov	(%PSP), %eax		# W
	add	$4, %PSP
	cltd	# eax to { edx, eax }	# i.e. to { TMP, W }
	idiv	%TOS			# eax = quotient, edx = remainder
	mov	%eax, %TOS
	jmp	next_code
	ASMEND(slash)


### U/ - nonstandard unsigned division
###     ( u1 u2 -- u3 )
ASMWORD("u/", u_slash)
	mov	(%PSP), %eax		# W
	add	$4, %PSP
	xor	%edx, %edx		# zero-extend to 64-bit
	div	%TOS			# eax = quotient, edx = remainder
	mov	%eax, %TOS
	jmp	next_code
	ASMEND(u_slash)


### 2* - CORE 6.1.0320
###     ( x1 -- x2 )
ASMWORD("2*", two_star)
	add	%TOS, %TOS
	jmp	next_code
	ASMEND(two_star)


### 2/ - CORE 6.1.0330
###     ( x1 -- x2 )
ASMWORD("2/", two_slash)
	sar	%TOS
	jmp	next_code
	ASMEND(two_slash)


### INVERT - CORE 6.1.1720
###     ( x1 -- x2 )
ASMWORD("invert", invert)
	not	%TOS
	jmp	next_code
	ASMEND(invert)


### AND - CORE 6.1.0720
###     ( x1 x2 -- x3 )
ASMWORD("and", and)
	and	(%PSP), %TOS
	add	$4, %PSP
	jmp	next_code
	ASMEND(and)


### OR - CORE 6.1.1980
###     ( x1 x2 -- x3 )
ASMWORD("or", or)
	or	(%PSP), %TOS
	add	$4, %PSP
	jmp	next_code
	ASMEND(or)


### XOR - CORE 6.1.2490
###     ( x1 x2 -- x3 )
ASMWORD("xor", xor)
	xor	(%PSP), %TOS
	add	$4, %PSP
	jmp	next_code
	ASMEND(xor)


## LSHIFT - CORE 6.1.1805
###     ( x1 u -- x2 )
ASMWORD("lshift", lshift)
	mov	(%PSP), %W
	add	$4, %PSP
	shl	%cl, %W		# lsb of TOS
	mov	%W, %TOS
	jmp	next_code
	ASMEND(lshift)


## RSHIFT - CORE 6.1.2162
###     ( x1 u -- x2 )
ASMWORD("rshift", rshift)
	mov	(%PSP), %W
	add	$4, %PSP
	shr	%cl, %W		# lsb of TOS
	mov	%W, %TOS
	jmp	next_code
	ASMEND(rshift)



/**
 * Fetch and store
 */

### @ - CORE 6.1.0650
###     ( a-addr -- x )
ASMWORD("@", fetch)
	mov	(%TOS), %TOS
	jmp	next_code
	ASMEND(fetch)


### ! - CORE 6.1.0010
###     ( x a-addr -- )
ASMWORD("!", store)
	mov	(%PSP), %W
	mov	%W, (%TOS)
	mov	4(%PSP), %TOS
	add	$8, %PSP
	jmp	next_code
	ASMEND(fetch)


### C@ - CORE 6.1.0870
###     ( c-addr -- char )
ASMWORD("c@", c_fetch)
	movzbl	(%TOS), %TOS
	jmp	next_code
	ASMEND(c_fetch)


### C! - CORE 6.1.0850
###     ( char c-addr -- )
ASMWORD("c!", c_store)
	mov	(%PSP), %W
	movb	%al, (%TOS)	# lsb of %W
	mov	4(%PSP), %TOS
	add	$8, %PSP
	jmp	next_code
	ASMEND(c_store)


### 2@ - CORE 6.1.0350
###     ( a-addr -- x1 x2 )
ASMWORD("2@", two_fetch)
	mov	4(%TOS), %W
	mov	(%TOS), %TOS	# lower address goes to the top
	mov	%W, -4(%PSP)
	add	$-4, %PSP
	jmp	next_code
	ASMEND(two_fetch)


### 2! - CORE 6.1.0310
###     ( x1 x2 a-addr -- )
ASMWORD("2!", two_store)
	mov	(%PSP), %W
	mov	4(%PSP), %TMP
	mov	%W, (%TOS)	# top -> lower address
	mov	%TMP, 4(%TOS)
	mov	8(%PSP), %TOS
	add	$12, %PSP
	jmp	next_code
	ASMEND(two_store)



/**
 * Strings
 */

### FILL - CORE 6.1.1540
###     ( c-addr u char -- )
ASMWORD("fill", fill)
	push	%PSP		# save, "stosb" needs %edi
	mov	%TOS, %eax	# W = %al - char
	mov	(%PSP), %ecx	# TOS = %ecx - count
	mov	4(%PSP), %edi	# NB: overwrites %PSP
	rep stosb
	pop	%PSP
	mov	8(%PSP), %TOS
	add	$12, %PSP
	jmp	next_code
	ASMEND(fill)


### MOVE - CORE 6.1.1900
###     ( src dst len -- )
ASMWORD("move", move)
	mov	4(%PSP), %W
	mov	(%PSP), %TMP
	push	%TOS		# len
	push	%W		# src
	push	%TMP		# dst
	call	memmove
	mov	8(%PSP), %TOS
	add	$12, %PSP
	jmp	next_code
	ASMEND(move)



/*
 * Double-cell numbers
 */

### S>D - CORE 6.1.2170
###     ( n -- d )
ASMWORD("s>d", s2d)
	SPILL_TOS		# least significant cell is below
	sar	$31, %TOS	# fill with the sign bit
	jmp	next_code
	ASMEND(s2d)


### D< - DOUBLE 8.6.1.1110
###     ( d1 d2 -- flag )
ASMWORD("d<", d_less_than)
	mov	0(%PSP), %W	# D2lo
	mov	8(%PSP), %TMP	# D1lo
	cmp	%TOS, 4(%PSP)	# D1hi < D2hi?
	jl	2f		# true
	je	1f		# compare the lower cells
0:
	xor	%TOS, %TOS	# false
	add	$12, %PSP
	jmp	next_code
1:
	cmp	%W, %TMP	# D1lo < D2lo?
	jnb	0b		# false
2:
	mov	$-1, %TOS	# true
	add	$12, %PSP
	jmp	next_code
	ASMEND(d_less_than)


### DNEGATE - DOUBLE 8.6.1.1230
###     ( d1 -- d2 )
ASMWORD("dnegate", dnegate)
	negl	(%PSP)
	adc	$0, %TOS
	neg	%TOS
	jmp	next_code
	ASMEND(dnegate)


### DABS - DOUBLE 8.6.1.1160
###     ( d1 -- d2 )
ASMWORD("dabs", dabs)
	test	%TOS, %TOS
	jns	next_code
	negl	(%PSP)
	adc	$0, %TOS
	neg	%TOS	
	jmp	next_code
	ASMEND(dabs)


### D+ - DOUBLE 8.6.1.1040 
###     ( d1|ud1 d2|ud2 -- d3|ud3 )
ASMWORD("d+", d_plus)
	mov	(%PSP), %W	# D2lo
	add	8(%PSP), %W	# D3lo = D1lo + D2lo
	adc	4(%PSP), %TOS	# D3hi = D1hi + (D2hi + carry)
	mov	%W, 8(%PSP)
	add	$8, %PSP
	jmp	next_code
	ASMEND(d_plus)


### D- - DOUBLE 8.6.1.1050
###     ( d1|ud1 d2|ud2 -- d3|ud3 )
ASMWORD("d-", d_minus)
	mov	8(%PSP), %W	# D1lo
	mov	4(%PSP), %TMP	# D1hi
	sub	(%PSP), %W	# D3lo = D1lo - D2lo
	sbb	%TOS, %TMP	# D3hi = D1hi - (D2hi + carry)
	mov	%W, 8(%PSP)
	add	$8, %PSP
	mov	%TMP, %TOS
	jmp	next_code
	ASMEND(d_minus)


### D2* - DOUBLE 8.6.1.1090
###     ( xd1 -- xd2 )
ASMWORD("d2*", d_two_star)
	mov	(%PSP), %W	# lo
	add	%W, %W
	adc	%TOS, %TOS
	mov	%W, (%PSP)
	jmp	next_code
	ASMEND(d_two_star)


### D2/ - DOUBLE 8.6.1.1100
###     ( xd1 -- xd2 )
ASMWORD("d2/", d_two_slash)
	mov	%TOS, %W
	shr	$31, %W		# sign
	xor	%TMP, %TMP
	add	(%PSP), %W	# lo
	adc	%TOS, %TMP	# hi
	shrd	$1, %TMP, %W
	sar	%TMP
	mov	%W, (%PSP)
	mov	%TMP, %TOS
	jmp	next_code
	ASMEND(d_two_slash)


### M* - CORE 6.1.1810
###     ( n1 n2 -- d ) signed mixed multiplication (32x32 -> 64)
ASMWORD("m*", m_star)
	mov	(%PSP), %eax	# W
	imul	%TOS		# result in { %edx, %eax }
	mov	%eax, (%PSP)	# lo
	mov	%edx, %TOS	# hi
	jmp	next_code
	ASMEND(m_star)


### UM* - CORE 6.1.2360
###     ( n1 n2 -- d ) unsigned mixed multiplication (32x32 -> 64)
ASMWORD("um*", um_star)
	mov	(%PSP), %eax	# W
	mul	%TOS		# result in { %edx, %eax }
	mov	%eax, (%PSP)	# lo
	mov	%edx, %TOS	# hi
	jmp	next_code
	ASMEND(m_star)



/**
 * Control structure helpers
 */

### LIT - auxiliary for LITERAL (CORE 6.1.1780)
ASMWORD("lit", lit)
	SPILL_TOS
	mov	(%IP), %TOS
	add	$4, %IP
	jmp	next_code
	ASMEND(lit)


### 2LIT - auxiliary for 2LITERAL (DOUBLE 8.6.1.0390)
ASMWORD("2lit", two_lit)
	mov	%TOS, -4(%PSP)	# spill tos
	mov	(%IP), %TOS	# lower address goes to the top
	mov	4(%IP), %W
	add	$8, %IP
	mov	%W, -8(%PSP)
	add	$-8, %PSP
	jmp	next_code
	ASMEND(two_lit)


### BRANCH - auxiliary for ELSE &co
ASMWORD("branch", branch)
	mov	(%IP), %IP
	jmp	next_code
	ASMEND(branch)


### ?BRANCH - auxiliary for IF &co
ASMWORD("?branch", question_branch)
	mov	%TOS, %W
	FILL_TOS
	test	%W, %W
	jz	.L_branch_taken	# NB: taken when TOS is false
	add	$4, %IP		# not taken, skip over target address
	jmp	next_code
.L_branch_taken:
	mov	(%IP), %IP
	jmp	next_code
	ASMEND(question_branch)


### I - CORE 6.1.1680
ASMWORD("i", i_4th)
	SPILL_TOS
	mov	(%RSP), %TOS		# adjusted current
	mov	4(%RSP), %W		# adjustment
	sub	%W, %TOS		# normalize current
	jmp	next_code
	ASMEND(i_4th)


### J - CORE 6.1.1730
ASMWORD("j", j_4th)
	SPILL_TOS
	mov	12(%RSP), %TOS		# adjusted current
	mov	16(%RSP), %W		# adjustment
	sub	%W, %TOS		# normalize current
	jmp	next_code
	ASMEND(i_4th)


### XXX: nothing to do on ppc, just 2DROP the args
ASMWORD("sync-i$", sync_icache)
	add	$4, %PSP
	FILL_TOS
	jmp	next_code
	ASMEND(breakpoint)


/* Machine-independent words in threaded code. */
#include "forth.S"

/* Random test code. */
#include "test.S"

	.data
	.size	heap, . - heap

	.section ".bss"
.L_DATA_SPACE: /* begins HERE */
.FENCE = .
	.space	4096 * 8
