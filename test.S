#ifdef TESTING
/**
 * When you port to a new CPU, you want to test your asm words as soon
 * as you write them.  You can do that by including test.S instead of
 * transpiled forth.S in your forth_machdep.S and testing your words
 * here with simple handwritten threaded code sequences.
 */

/* Entry point */
WORD("abort", abort)
	.long	abort_parens	/* setup stacks */
	/*
	 * Your test code here ...
	 */
	.long	lit, 3, lit, 0, do_parens, .Leave
.Loop:	.long	i_4th, loop_parens, .Loop
.Leave:	// ( -- 0 1 2 )
	.long	bye_parens	/* return from forth */

// manually "compiled" from forth.fth
WORD("(do)", do_parens)
	// ( limit first -- R: leave-addr limit current )
	.long	r_from, dup, fetch, to_r, rot
	.long	lit, 0x80000000, swap, minus, to_r
	.long	swap, r_fetch, plus, to_r
	.long	lit, 4, plus, to_r
	EXIT_4TH

WORD("(+loop)", plus_loop_parens)
	// ( increment -- )
	.long	r_from, swap, r_from, plus_question
	.long   question_branch, 0f
	.long	drop, two_r_from, two_drop, lit, 4, plus, to_r
	.long	branch, 9f
0:	.long	to_r, to_r, goto_parens, branch
9:	EXIT_4TH

WORD("(loop)", loop_parens)
	.long	lit, 1, goto_parens, plus_loop_parens
	.long	0

/* For your CONSTANT() definitions */
DOES_4TH(constant_does)
#if 0 // XXX: disable until we implement FETCH
	.long	fetch
#endif
	EXIT_4TH

/*
 * Referenced by FORTH vocabulary definition.  Provide a stub to keep
 * the linker happy.  You won't need it in your handwritten tests.
 */
DOES_4TH(vocabulary_does)
	.long	0

.ifndef LATEST(forth)
LATEST(forth) = LASTNFA(forth)
.endif
#endif /* TESTING */
