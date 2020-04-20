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
	.long	lit, 0x01234567, lit, .Lcell, store
	.long	lit, .Lcell, fetch
	.long	lit, .Lbyte, c_fetch
	.long	lit, 0xa5, lit, .Lcell, c_store
	.long	lit, 0x5a, lit, .Lcell+1, c_store
	.long	lit, 0x5a, lit, .Lcell+2, c_store
	.long	lit, 0xa5, lit, .Lcell+3, c_store
	.long	lit, .Lcell, fetch
	// ( ... -- 0x01234567 255 0xa55aa55a )
	.long	bye_parens	/* return from forth */
.Lcell:	.long	0xdeadbeef
.Lbyte:	.byte	0xff

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
