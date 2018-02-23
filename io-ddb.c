/*
 * Copyright (c) 2017 Valery Ushakov
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

#include <sys/types.h>
#include <ddb/ddb.h>

#include "forth-c.h"

/*
 * EMIT ( x -- )
 */
cell_t *
emit_impl(cell_t *stack)
{
    char c = (char)stack[0];

    db_putchar(c);
    db_force_whitespace();

    ++stack;
    return stack;
}


/*
 * TYPE ( c-addr u -- )
 */
cell_t *
type_impl(cell_t *stack)
{
    size_t size = stack[0];
    const char *data = (char *)stack[1];

    db_printf("%*.*s", size, size, data);
    db_force_whitespace();

    stack += 2;
    return stack;
}


/*
 * ACCEPT ( c-addr +n1 -- +n2 )
 */
cell_t *
accept_impl(cell_t *stack)
{
    size_t buflen = stack[0];
    char *buf = (char *)stack[1];
    int len;

    db_output_line = 0;
    len = db_readline(buf, buflen);
    if (buf[len-1] == '\n') { buf[--len] = '\0'; }
    if (buf[len-1] == '\r') { buf[--len] = '\0'; }

    ++stack;
    stack[0] = len;
    return stack;
}
