/**
 *
 * Test code to try out the Forth system we defined above.
 *
 */

!!! just to have something to use in interactive test
CONSTANT("c", c_XXX)	.long	42
VARIABLE("v", v_XXX)	.long	0


!!! interpret source until it's empty
WORD("interpret", interpret)
1:	!! begin
	.long	parse_word, question_dup ! 0 only on EOF
	.long	question_branch, 9f

	.long	two_dup, search_current
	!! XXX: currently we don't have compilation/immediate etc
	.long	zero_equals, question_branch, 2f

	!! word not found
	.long	type, SLITERAL(" unknown word"), type, cr
	.long	exit_4th	! abort current line

2:	!! word found
	.long	minus_rot, two_drop, execute

	.long	branch, 1b
	!! again
9:
	.long	drop	 ! leftover caddr from unsuccessful parse-word
	EXIT_4TH

!!! refill/interpret until eof - outer repl loop
WORD("repl", repl)
1:	!! begin
	.long	    refill
	.long	question_branch, 9f
	!! while
	.long	    interpret
	.long	    SLITERAL(" ok"), type, cr
	!! repeat
	.long	branch, 1b
9:
	EXIT_4TH


WORD("test-word", test_word)
	.long	repl
	EXIT_4TH
