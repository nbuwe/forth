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

#include <string.h>
#include <stdlib.h>
#include <stdio.h>


int
main()
{
    int32_t *psp;
    int32_t *p;
    int32_t *bottom;

    psp = start_forth();

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


/*
 * ( x -- )
 */
cell_t *
emit_impl(cell_t *stack)
{
    char c = (char)stack[0];

    fputc(c, stdout);
    fflush(stdout);

    ++stack;
    return stack;
}


/*
 * ( c-addr u -- )
 */
cell_t *
type_impl(cell_t *stack)
{
    size_t size = stack[0];
    const char *data = (char *)stack[1];

    fwrite(data, size, 1, stdout);
    fflush(stdout);

    stack += 2;
    return stack;
}


/*
 * ( c-addr +n1 -- +n2 )
 */
cell_t *
accept_impl(cell_t *stack)
{
    const size_t buflen = stack[0];
    char *buf = (char *)stack[1];
    int len = 0;
    int c;

    if (buflen == 0) {
	if (feof(stdin) || ferror(stdin)) {
	    len = -1;
	    goto out;
	}
    }

    flockfile(stdin);
    while (len < buflen) {
	c = getc_unlocked(stdin);

	if (c == EOF) {
	    if (len == 0)
		len = -1;
	    break;
	}

	if (c == '\n')
	    break;

	buf[len++] = c;
    }

    /* peek at the next character and drop it if it's a newline */
    if (len == buflen) {
	c = getc_unlocked(stdin);

	if (c != '\n' && c != EOF)
	    ungetc(c, stdin);
    }

    funlockfile(stdin);
  out:

    ++stack;
    stack[0] = len;
    return stack;
}



/* XXX */
#if defined(__sh__)
# define HAVE_DIVMODDI4
# define HAVE_UDIVMODDI4
#elif defined(__powerpc__)
# define HAVE_UDIVMODDI4
#endif


#ifdef HAVE_DIVMODDI4
extern int64_t __divmoddi4(int64_t a, int64_t b, int64_t *rem);
#endif

#ifdef HAVE_UDIVMODDI4
extern uint64_t __udivmoddi4(uint64_t a, uint64_t b, uint64_t *rem);
#endif


/*
 * Symmetric division, aka truncation towards zero.
 * For C99 this is mandated by the standard.
 *
 * ( d1 n1 -- n2 d2 )
 */
cell_t *
ms_slash_rem_impl(cell_t *stack)
{
    int64_t dividend, quotient, remainder;
    int32_t divisor;

    divisor = (int32_t)stack[0];
    dividend = (int64_t)((uint64_t)stack[1] << 32) | stack[2];

#ifdef HAVE_DIVMODDI4
    quotient = __divmoddi4(dividend, divisor, &remainder);
#else
    quotient  = dividend / divisor;
    remainder = dividend % divisor;
#endif

    stack[0] = (uint32_t)(quotient >> 32);
    stack[1] = (uint32_t)quotient;
    stack[2] = (uint32_t)remainder;

    return stack;
}


/*
 * Floored division.
 *
 * ( d1 n1 -- n2 d2 )
 */
cell_t *
mf_slash_mod_impl(cell_t *stack)
{
    int64_t dividend, quotient, remainder;
    int32_t divisor;

    divisor = (int32_t)stack[0];
    dividend = (int64_t)((uint64_t)stack[1] << 32) | stack[2];

    /* this is symmetric division */
#ifdef HAVE_DIVMODDI4
    quotient = __divmoddi4(dividend, divisor, &remainder);
#else
    quotient  = dividend / divisor;
    remainder = dividend % divisor;
#endif

    /* fix rounding if signs are different */
    if (remainder != 0 && ((int32_t)stack[0] ^ (int32_t)stack[1]) < 0) {
	--quotient;
	remainder += divisor;
    }

    stack[0] = (uint32_t)(quotient >> 32);
    stack[1] = (uint32_t)quotient;
    stack[2] = (uint32_t)remainder;

    return stack;
}


cell_t *
mu_slash_mod_impl(cell_t *stack)
{
    uint64_t dividend, quotient, remainder;
    uint32_t divisor;

    divisor = stack[0];
    dividend = ((uint64_t)stack[1] << 32) | stack[2];

#ifdef HAVE_UDIVMODDI4
    quotient = __udivmoddi4(dividend, divisor, &remainder);
#else
    quotient  = dividend / divisor;
    remainder = dividend % divisor;
#endif

    stack[0] = (uint32_t)(quotient >> 32);
    stack[1] = (uint32_t)quotient;
    stack[2] = (uint32_t)remainder;

    return stack;
}
