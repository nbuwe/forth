/* $Id$ */

#include <stdint.h>
#include <stdio.h>

extern void *test(int);
extern int32_t stack_bottom[];

int
main()
{
    int32_t *psp;
    int32_t *p;

    psp = test(10);

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
