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
