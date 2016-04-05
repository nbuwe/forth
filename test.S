/**
 *
 * Test code to try out the Forth system we defined above.
 *
 */
WORD("loop-test", loop_test)
	.long	lit, 5, lit, 0
	.long	do_parens, /* leave to */ 4f
3:	.long	SLITERAL("i="), type, i_4th, dot, cr
	.long	i_4th, lit, 3, equals, question_branch, 31f
	.long	    unloop, exit_4th
31:
	.long	loop_parens, /* back to */ 3b
4:
	.long	SLITERAL("loop-test done"), type, cr
	EXIT_4TH

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
	!.long	loop_test

	.long	SLITERAL("> "),	type

	.long	refill, question_branch, 9f
	.long	parse_word
	.long	two_dup, swap, dot, dot
	.long	SLITERAL("and >in is "), type, to_in, fetch, dot
	.long	cr
	.long	question_dup, question_branch, 8f
	.long	    lit, 124, emit
	.long	    type
	.long	    lit, 124, emit, space
	.long	branch, 9f
8:
	.long	    drop
	.long	    SLITERAL("no word"), type, cr
9:
	EXIT_4TH
