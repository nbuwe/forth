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

#include "forth-c.h"

#include <sys/types.h>
#include <sys/mman.h>

#include <signal.h>
#include <stdio.h>
#include <string.h>
#include <ucontext.h>


void stack_setup(void);
void trapsegv(int sig, siginfo_t *info, void *ctx);

extern void *start_forth(/* unchecked */);
extern void *forth_eval(const char *s, size_t l);

extern cell_t throw[];
extern cell_t exit_4th_code[];


int
main()
{
    cell_t *psp;
    cell_t *p;
    cell_t *bottom;

    stack_setup();

#if 1
    psp = start_forth();
#else
    /* testing */
    const char expr[] = "2 3 + dup . cr";
    psp = forth_eval(expr, strlen(expr));
#endif

    /* skip placeholder for invalid TOS of an empty stack */
    bottom = stack_bottom - 1;

    if (psp < bottom) {
	printf("-- STACK:\n");
	for (p = psp; p < bottom; ++p) {
	    printf("0x%08x  %d\n", *p, *p);
	}
    }
    else {
	printf("-- STACK: <empty>\n");
    }

    return 0;
}


void
stack_setup(void)
{
    struct sigaction act;

#define REDZONE(beg, end)				\
    do {						\
	mprotect(beg,					\
		 (uintptr_t)(end) - (uintptr_t)(beg),	\
		 PROT_NONE);				\
    } while (0)

    REDZONE(redzone_above, stack_limit);
    REDZONE(stack_bottom, rstack_limit);
    REDZONE(rstack_bottom, redzone_below);

    memset(&act, 0, sizeof(act));
    act.sa_sigaction = trapsegv;
    act.sa_flags = SA_SIGINFO;
    sigaction(SIGSEGV, &act, NULL);
}


void trapsegv(int sig, siginfo_t *si, void *ctx)
{
    static struct xstack {
	cell_t code[2];
	cell_t stack[16];
	cell_t rstack[16];
    } xstack;

    ucontext_t *uc = ctx;
    cell_t exception = 0;

#define BETWEEN(beg, end) \
    ((void *)&(beg) <= si->si_addr && si->si_addr < (void *)&(end))

    if (si->si_code == SEGV_MAPERR) {
	if (BETWEEN(redzone_above, stack_limit))
	    exception = -3;	/* stack overflow */
	else if (BETWEEN(stack_bottom, redzone_between))
	    exception = -4;	/* stack underflow */
	else if (BETWEEN(redzone_between, rstack_limit))
	    exception = -5;	/* return stack overflow */
	else if (BETWEEN(rstack_bottom, redzone_below))
	    exception = -6;	/* return stack underflow */
    }


    if (exception == 0) {   /* re-execute the instruction and crash */
	struct sigaction act;

	fprintf(stderr, "signal %d error %d code %d addr %p\n",
		si->si_signo, si->si_errno, si->si_code, si->si_addr);

	memset(&act, 0, sizeof(act));
	act.sa_handler = SIG_DFL;
	sigaction(SIGSEGV, &act, NULL);
	return;
    }


    xstack.code[0] = (uintptr_t) throw;
    xstack.code[1] = 0;

    /* exception code for THROW */
    uc->uc_mcontext.__gregs[_REG_TOS] = exception;
    uc->uc_mcontext.__gregs[_REG_PSP] =
	(uintptr_t) &xstack.stack[__arraycount(xstack.stack) - 1];

    /* arrange to return to xstack.code[] */
    _UC_MACHINE_SET_PC(uc, (uintptr_t) exit_4th_code);

    xstack.rstack[__arraycount(xstack.rstack) - 1] = 0;
    xstack.rstack[__arraycount(xstack.rstack) - 2] = (uintptr_t) &xstack.code;

    uc->uc_mcontext.__gregs[_REG_RSP] =
	(uintptr_t) &xstack.rstack[__arraycount(xstack.rstack) - 2];
}
