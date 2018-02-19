/*
 * Copyright (c) 2018 Valery Ushakov
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

#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

static cell_t *doopen(cell_t *, int);


/*
 * OPEN-FILE   ( c-addr u fam -- fileid ior )
 */
cell_t *
open_file_impl(cell_t *stack)
{
    return doopen(stack, 0);
}


/*
 * CREATE-FILE   ( c-addr u fam -- fileid ior )
 */
cell_t *
create_file_impl(cell_t *stack)
{
    return doopen(stack, O_CREAT | O_TRUNC);
}


static cell_t *
doopen(cell_t *stack, int flags)
{
    int mode = stack[0];
    size_t namelen = stack[1];
    const char *name = (void *)stack[2];

    ++stack;
    stack[0] = -1;		/* ior */
    stack[1] = 0;		/* fileid */

    const char *modestr = NULL;
    if ((mode & O_ACCMODE) == O_RDONLY) {
	modestr = "r";
    }
    else if ((mode & O_ACCMODE) == O_WRONLY) {
	modestr = "w";
    }
    else if ((mode & O_ACCMODE) == O_RDWR) {
	if (flags & O_CREAT)
	    modestr = "w+";
	else
	    modestr = "r+";
    }
    else {
	return stack;
    }

    char *namebuf = strndup(name, namelen);
    if (namebuf == NULL)
	return stack;

    /*
     * stdio can't open a file for writing without O_CREAT, so to
     * follow the letter of the standard do open(2) with the right
     * flags manually.
     */
    int fd = open(namebuf, mode | flags, 0666);
    int open_errno = errno;	/* free might clobber it */
    free(namebuf);

    if (fd < 0) {
	stack[0] = open_errno;	/* ior */
	return stack;
    }

    FILE *f = fdopen(fd, modestr);
    if (f == NULL) {
	stack[0] = errno;	/* ior */
	close(fd);
	return stack;
    }

    stack[0] = 0;		/* ior */
    stack[1] = (cell_t)f;	/* fileid */
    return stack;
}


/*
 * CLOSE-FILE   ( fileid -- ior )
 */
cell_t *
close_file_impl(cell_t *stack)
{
    FILE *f = (FILE *)stack[0];

    int status = fclose(f);
    if (status != 0)
	stack[0] = errno;
    else
	stack[0] = 0;
    return stack;
}


/*
 * READ-LINE   ( c-addr u1 fileid -- u2 flag ior )
 *
 * Read the next line from the file specified by fileid into memory at
 * the address c-addr.  At most u1 characters are read.  Up to two
 * implementation-defined line-terminating characters may be read into
 * memory at the end of the line, but are not included in the count
 * u2.  The line buffer provided by c-addr should be at least u1+2
 * characters long.
 *
 * If the operation succeeded, flag is true and ior is zero.  If a
 * line terminator was received before u1 characters were read, then
 * u2 is the number of characters, not including the line terminator,
 * actually read (0 <= u2 <= u1).  When u1 = u2 the line terminator
 * has yet to be reached.
 *
 * If the operation is initiated [at the end of file], flag is false,
 * ior is zero, and u2 is zero.  If ior is non-zero, an exception
 * occurred during the operation and ior is the implementation-defined
 * I/O result code.
 */
cell_t *
read_line_impl(cell_t *stack)
{
    FILE *f = (FILE *)stack[0];
    size_t u1 = stack[1];
    char *buf = (char *)stack[2];

    /*
     * buf has u1+2 bytes, use one of the extra bytes for the '\0'
     */
    size_t buflen = u1;
    if (__predict_true(buflen < CELL_MAX))
	++buflen;

#if CELL_MAX > INT_MAX
    if (__predict_false(buflen > INT_MAX))
	buflen = INT_MAX;
#endif

    char *s = fgets(buf, buflen, f);
    if (s != NULL) {
	stack[0] = 0;		/* ior */
	stack[1] = ~(cell_t)0;	/* flag */

	size_t len;
	char *nlp = memchr(buf, '\n', buflen);
	if (nlp != NULL) {
	    len = nlp - buf;
	}
	else {
	    /* XXX: line with embedded '\0' */
	    len = strlen(buf);
	}
	stack[2] = len;
    }
    else {
	int fget_errno = errno;
	if (ferror(f))
	    stack[0] = fget_errno; /* ior */
	else
	    stack[0] = 0;
	stack[1] = 0;		/* flag */
	stack[2] = 0;		/* u2 */
    }

    return stack;
}
