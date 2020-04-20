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
	.long	callee
	// ( -- 0xcafebabe )
	.long	bye_parens	/* return from forth */

WORD("callee", callee)
	.long	rp_fetch, callee2, lit, 0xdeadbeef
	EXIT_4TH

WORD("callee2", callee2)
	.long rp_store, lit, 0xcafebabe
	EXIT_4TH

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
