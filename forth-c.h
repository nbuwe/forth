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
#ifndef _FORTH_C_H_
#define _FORTH_C_H_

#ifdef _KERNEL
#include <sys/stdint.h>
#include <machine/limits.h>
#else
#include <stdint.h>
#include <limits.h>
#endif

typedef uintptr_t cell_t;
#define CELL_MAX UINTPTR_MAX

/* this probably should be in a dedicated MD header */
#if defined(__arm__)
# define _REG_TOS	_REG_R4
# define _REG_PSP	_REG_R5
# define _REG_RSP	_REG_R6
# define _REG_IP	_REG_R7
#elif defined(__i386__)
# define _REG_TOS	_REG_ECX
# define _REG_PSP	_REG_EDI
# define _REG_RSP	_REG_ESI
# define _REG_IP	_REG_EBX
#elif defined(__powerpc__)
# define _REG_TOS	_REG_R26
# define _REG_PSP	_REG_R27
# define _REG_RSP	_REG_R28
# define _REG_IP	_REG_R29
#elif defined(__sh__)
# define _REG_TOS	_REG_R8
# define _REG_PSP	_REG_R9
# define _REG_RSP	_REG_R10
# define _REG_IP	_REG_R11
#else
# error Unsupported architecture
#endif


cell_t *emit_impl(cell_t *);
cell_t *type_impl(cell_t *);
cell_t *accept_impl(cell_t *);

cell_t *ms_slash_rem_impl(cell_t *); /* symmetric division */
cell_t *mf_slash_mod_impl(cell_t *); /* floored */
cell_t *mu_slash_mod_impl(cell_t *); /* unsigned */


extern cell_t redzone_above[];
extern cell_t stack_limit[];
extern cell_t stack_bottom[];
extern cell_t redzone_between;
extern cell_t rstack_limit[];
extern cell_t rstack_bottom[];
extern cell_t redzone_below[];

#endif /* _FORTH_C_H_ */
