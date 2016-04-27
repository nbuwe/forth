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
    size_t buflen = stack[0];
    char *buf = (char *)stack[1];

    size_t clen = 0;

    char *cbuf;
    char *ret;

    cbuf = malloc(buflen + 1); /* +1 for NUL added by fgets(3) */
    if (cbuf == NULL) {
	goto out;
    }

    ret = fgets(cbuf, buflen + 1, stdin);
    if (ret == NULL) {
	goto out;
    }

    clen = strlen(cbuf);

    if (clen > 0 && cbuf[clen - 1] == '\n')
	cbuf[--clen] = '\0';

    memcpy(buf, cbuf, clen);

  out:
    free(cbuf);

    ++stack;
    stack[0] = clen;
    return stack;
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
