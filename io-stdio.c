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

#include <stdio.h>


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
