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

/**
 *
 * Forth stacks and buffers in the BSS section.
 *
 */

	.section ".bss"
	.balign	4096

#define SPACE(_sym, _size)	  \
	.global	_sym		; \
	_SPACE(_sym, _size)

#define _SPACE(_sym, _size)	  \
	.type	_sym, @object	; \
	.size	_sym, (_size)	; \
_sym:	.if	(_size)		; \
	.space	(_size)		; \
	.endif

SPACE(redzone_above, 4096)
SPACE(stack_limit, 4096)
SPACE(stack_bottom, 2048)
SPACE(redzone_between, 2048)
SPACE(rstack_limit, 4096)
SPACE(rstack_bottom, 4096)
SPACE(redzone_below, 0)

SPACE(input_buffer_start, 4096)


#undef SPACE
#undef _SPACE
