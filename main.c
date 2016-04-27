/* $Id$ */

#include <string.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>

typedef uintptr_t cell_t;

extern void *start_forth(/* unchecked */);
extern int32_t stack_bottom[];

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


void
emit_impl(int32_t character)
{
    char c = (char)character;
    fputc(c, stdout);
    fflush(stdout);
    return;
}


void
type_impl(const char *data, int32_t size)
{
    fwrite(data, size, 1, stdout);
    fflush(stdout);
    return;
}


uint32_t
accept_impl(char *buf, uint32_t buflen)
{
    char *cbuf;
    char *ret;
    size_t clen;

    cbuf = malloc(buflen + 1); /* +1 for NUL added by fgets(3) */
    if (cbuf == NULL) {
	return 0;
    }

    ret = fgets(cbuf, buflen + 1, stdin);
    if (ret == NULL) {
	free(cbuf);
	return 0;
    }

    clen = strlen(cbuf);

    if (clen > 0 && cbuf[clen - 1] == '\n')
	cbuf[--clen] = '\0';

    memcpy(buf, cbuf, clen);
    free(cbuf);

    return clen;
}


/* XXX */
extern int64_t __divmoddi4(int64_t a, int64_t b, int64_t *rem);
extern uint64_t __udivmoddi4(uint64_t a, uint64_t b, uint64_t *rem);


cell_t *
ms_slash_rem_impl(cell_t *stack)
{
    int64_t dividend, quotient, remainder;
    int32_t divisor;

    divisor = (int32_t)stack[0];
    dividend = (int64_t)((uint64_t)stack[1] << 32) | stack[2];

    quotient = __divmoddi4(dividend, divisor, &remainder);

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

    quotient = __udivmoddi4(dividend, divisor, &remainder);

    stack[0] = (uint32_t)(quotient >> 32);
    stack[1] = (uint32_t)quotient;
    stack[2] = (uint32_t)remainder;

    return stack;
}
