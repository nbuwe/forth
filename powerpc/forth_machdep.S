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

	# 0(%sp) - back chain
	# 4(%sp) - callee's LR save slot
	# 8(%sp) to 36(%sp) - 8 words of parameter list area

	# 40(%sp) to 48(%sp) - pad to 16 byte alignment/local variable space

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

	lis	%r3, stack_bottom@ha
	la	%r3, stack_bottom@l(%r3)
	addi	%r3, %r3, -4

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
