/**
 *
 * Test code to try out the Forth system we defined above.
 *
 */

WORD("test-word", test_word)
#if 1
	.long	lit, 0x1337babe
	.long	SLITERAL("."), search_current
	.long	question_branch, 1f
	.long	    execute
	.long	branch, 2f
1:	.long	    drop, SLITERAL("not found"), type
2:
	.long	cr
#endif

	.long	lit, 5, lit, 0
	.long	do_parens, /* leave to */ 4f
3:	.long	SLITERAL("i="), type, i_4th, dot, cr
	.long	loop_parens, /* back to */ 3b
4:

	.long	SLITERAL("Hello, World!"), type, cr
	.long	SLITERAL("> "),	type

	.long	refill, question_branch, 9f
	.long	skip_spaces
	.long	source, drop, to_in, fetch, plus, lit, 3, type
9:
	EXIT_4TH
