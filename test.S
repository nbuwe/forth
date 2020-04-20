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
	.long	true, /* if */ question_branch, 0f
	.long	lit, 0xcafebabe, branch, 1f
0:	// else
	.long	lit, 0xdeadbeef
1:	// then

	.long	false, /* if */ question_branch, 0f
	.long	lit, 0xdeadbeef, branch, 1f
0:	// else
	.long	lit, 0xcafebabe
1:	// then
	// ( -- 0xcafebabe 0xcafebabe )
	.long	bye_parens	/* return from forth */


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
