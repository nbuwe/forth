/**
 *
 * Test code to try out the Forth system we defined above.
 *
 */

!!! just to have something to use in interactive test
CONSTANT("c", c_XXX)	.long	42
VARIABLE("v", v_XXX)	.long	0

VARIABLE("ten", ten_XXX) .long 10
VARIABLE("sxt", sxt_XXX) .long 16

WORD("dec", dec)
	.long	lit, 10, base, store
	EXIT_4TH

WORD("hex", hex)
	.long	lit, 16, base, store
	EXIT_4TH

#define STRING(fname, aname, str) \
    WORD(fname, aname); .long SLITERAL(str); EXIT_4TH

STRING("test-string", test_string, "6789ABCD")

WORD("numtest", numtest)
	.long	lit, 0, dup, test_string, to_number
	.long	SLITERAL("num: "), type, rot, drop, rot, dot, cr
	.long	SLITERAL("...: "), type, type, cr
	.long	lit, 3, lit, 2, lit, 1, lit, 0, two_swap, two_over
	EXIT_4TH
