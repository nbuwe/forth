/* $Id$ */

#include <string.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>

extern void *start_forth(/* unchecked */);
extern int32_t stack_bottom[];

int
main()
{
    int32_t *psp;
    int32_t *p;

    psp = start_forth();

    printf("-- STACK:\n");
    for (p = psp; p < stack_bottom; ++p) {
	printf("0x%08x  %d\n", *p, *p);
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
