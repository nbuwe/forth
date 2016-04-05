/**
 *
 * Test code to try out the Forth system we defined above.
 *
 */

CONSTANT("c1", c1) .long 0x00c0ffee
VARIABLE("v1", v1) .long 0x0000beef

WORD("three", three)
	.long	lit, 11, lit, 3, slash
	EXIT_4TH


WORD("test-word", test_word)
	.long	three, plus
	.long	lit, 100, lit, 10, not_equals
	.long	question_branch, 1f ! if
	.long	   lit, 1000, star
	.long	   lit, 9, mod
1:	!       then
	.long	v1
	.long	v1, fetch
	.long	c1, v1, store
	.long	v1, one_plus_store
	.long	v1, fetch


#if 0
	.long	lit, 0x1337babe
#if 0
	.long	lit, dot, execute
#else
	.long	SLITERAL("."), search_current
	.long	question_branch, 1f
	.long	    execute
	.long	branch, 2f
1:	.long	    SLITERAL("not found"), type
2:
#endif
	.long	cr
#endif /* 0 */
	.long	SLITERAL("Hello, World!"), type, cr
	.long	SLITERAL("> "),	type

	.long	refill, question_branch, 9f
	.long	skip_spaces
	.long	source, drop, to_in, fetch, plus, lit, 3, type
9:
	EXIT_4TH
