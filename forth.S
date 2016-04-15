/* $Id$ */

#include <machine/asm.h>

#define TOS	r8	/* top of parameter stack lives in a register */
#define PSP	r9	/* parameter stack pointer */
#define RSP	r10	/* return stack pointer */
#define IP	r11	/* instruction pointer */
#define NEXT	r13	/* "next" address to avoid memory loads */

#define SPILL_TOS	mov.l	TOS, @-PSP
#define FILL_TOS	mov.l	@PSP+, TOS


!!! ====================================================================
/**
 *
 * Forth stacks and buffers in the BSS section.
 *
 */

	.section ".bss"
	.balign	4096

	.global stack_bottom

stack_limit:
	.space	4096
stack_bottom:
	.space	4096		! hole
rstack_limit:
	.space	4096
rstack_bottom:

terminal_input_buffer:
	.space	4096
	.type	terminal_input_buffer, @object
	.size	terminal_input_buffer, 4096 ! XXX, compute

word_buf:	      ! buffer for the counted string returned by WORD
	.space	256


!!! ====================================================================
/**
 *
 * Trampoline code callable from C that starts Forth.
 *
 */
	.text
ENTRY(start_forth)
	!! prologue
	mov.l	r8, @-r15
	mov.l	r9, @-r15
	mov.l	r10, @-r15
	mov.l	r11, @-r15
	mov.l	r12, @-r15
	mov.l	r13, @-r15
	mov.l	r14, @-r15
	sts.l	pr, @-r15
	mov	r15, r14
	!! end prologue

	!! address of "next" lives in a dedicated global register
	mov.l	.L_next, NEXT

	!! these will be reset by QUIT
	mov	#0, TOS
	mov	#0, PSP
	mov	#0, RSP

	!! QUIT to forth
	mov.l	.L_quit_body, IP
	mov.l	@IP+, r0		! CFA of (quit), the first word
	add	#4, r0			! PFA - the actual code

ALTENTRY(test_enter)		! for gdb breakpoing only
	jmp	@r0
	 nop

.L_BYE_RETURNS_HERE:
	SPILL_TOS
	mov	PSP, r0		! return forth stack pointer

	!! epilogue
	mov	r14, r15
	lds.l	@r15+, pr
	mov.l	@r15+, r14
	mov.l	@r15+, r13
	mov.l	@r15+, r12
	mov.l	@r15+, r11
	mov.l	@r15+, r10
	mov.l	@r15+, r9
	rts
	 mov.l	@r15+, r8

	.align	2
.L_next:	.long	next_code
.L_quit_body:	.long	quit_body


!!! ====================================================================
/**
 *
 * Macros for manually defining the core Forth system.
 *
 */


#define NAMED_CELL(label)	  \
	.p2align 2		; \
	.type	label@object	; \
	.size	label, 4	; \
  label:			;


#define DEFCODE_ASM(label)		  \
	/* Code Field */		  \
  NAMED_CELL(label)			; \
	.long	asm_does		; \
	/* Parameter Field */		  \
	.type	label/**/_code@function ; \
  label/**/_code:			;


#define DEFCODE_4TH(label)	  \
	/* Code Field */	  \
  NAMED_CELL(label)		; \
	.long	call_code	; \
	/* Parameter Field */	  \
  label/**/_body:		;


#define DEFCODE_VAR(label)	  \
	/* Code Field */	  \
  NAMED_CELL(label)		; \
	.long	next_code	; \
	/* Parameter Field */	  \
  NAMED_CELL(label/**/_var)


#define DEFCODE_CONST(label)	  \
	/* Code Field */	  \
  NAMED_CELL(label)		; \
	.long	constant_does	; \
	/* Parameter Field */	  \
  NAMED_CELL(label/**/_const)


#define EXIT_4TH		  \
	.long	exit_4th


/*
 * Address of this code will be written into the code field
 * This code will be directly called by the interpreter
 * PFA of the defined word will be in TOS
 */
#define DOES_ASM(label)		  \
	.align	2		; \
	.type	label@function	; \
  label:			;


/*
 * For words with DOES> part (that contains forth code by definition)
 * the address of the following asm code will be compiled into the
 * code field of the defined word.
 *
 * This code does an equivalent of jumping to "CALL" with 1f in TOS,
 * but inlined and since we jump to "NEXT" directly we can avoid
 * messing with TOS at all.
 */
#define DOES_4TH(label)		  \
  DOES_ASM(label)		; \
	mova	1f, r0		; \
	mov.l	IP, @-RSP	; \
	jmp	@NEXT		; \
	 mov	r0, IP		; \
  1:	/* threaded code follows */



/*
 * XXX: unfortunately with traditional cpp we cannot use .L prefix
 * here, since we would also need to pass the label argument around
 * without preceding space, which is very fragile.
 */
#define NFA_LABEL(label) label/**/$nfa

#define NAME_FIELD(name, flags, label)    \
	.align	2, 0			; \
	.local	NFA_LABEL(label)	; \
  NFA_LABEL(label):			  \
	.byte	flags | (2f-1f)		; \
1:	.ascii	name			; \
2:	.align	2, 0


#define DEFWORD(name, flags, defcode, label)	  \
  	NAME_FIELD(name, flags, label)		; \
	/* Link Field */			  \
  NAMED_CELL(label/**/_lnk)			; \
	.long	.LASTNFA			; \
  .LASTNFA = NFA_LABEL(label)			; \
	defcode(label)

#define	IFLAG	0x80		/* immediate */
#define	SFLAG	0x40		/* smudge */

#define WORD(name, label)	DEFWORD(name,     0, DEFCODE_4TH,   label)
#define IMMWORD(name, label)	DEFWORD(name, IFLAG, DEFCODE_4TH,   label)
#define ASMWORD(name, label)	DEFWORD(name,     0, DEFCODE_ASM,   label)
#define CONSTANT(name, label)	DEFWORD(name,     0, DEFCODE_CONST, label)
#define VARIABLE(name, label)	DEFWORD(name,     0, DEFCODE_VAR,   label)


!!! it takes a bit of effort to remember that question_branch is taken
!!! when the condition is false, so provide a more mnemonic and
!!! shorter way to say it
#define OR_ELSE(label)	/* .long */ question_branch, label


#define SLITERAL(str)		  \
     /* .long */ branch, 19f	; \
11:	.ascii	str		; \
12:	.align	2		; \
19:	.long	lit, 11b, lit, (12b - 11b)


#define QUOTESTR(word, str)				  \
	/* .long */ word/**/_quote_parens, (22f - 21f)	; \
21:	.ascii	str					; \
22:	.align	2, 0 /* force new directive with ; */	;

#define SQ(str)		QUOTESTR(s, str)	/* s"     */
#define DOTQ(str)	QUOTESTR(dot, str)	/* ."     */
#define ABORTQ(str)	QUOTESTR(abort, str)	/* abort" */

#define THR14	"Interpreting a compile-only word"
#define THR16	"Attempt to use zero-length string as a name"

/* XXX: for now, implement in terms of abort" */
#define THROW(code) \
	/* .long */ lit, (code), ABORTQ(THR/**/code)

#define OR_THROW(code)				  \
	/* .long */ OR_ELSE(0f), THROW(code)	; \
0:


!!! ====================================================================
/**
 *
 *				FORTH
 *
 */

	.data			! XXX: need writable+executable

	.align	2
	.global	heap
heap:

/*
 * Fake sentinel word that starts the vocabulary.
 */
	.p2align 2, 0
	!! Name Field
.LASTNFA = .		! init .LASTNFA for subsequent DEFWORDs
	.byte	0x80	! empty name, immediate (just in case?)
	!! Link Field 
	.p2align 2, 0
	.long	0	! terminate the LFA-linked list
	!! Code Field
	.long	0	! XXX: use some sentinel code?  just crash for now
	!! Parameter Field
	.long	0	! there's no parameters, but provide a stub


!!! Asm part of QUIT that resets parameter and return stacks.
!!! Must be the first word called by QUIT.
ASMWORD("(quit)", quit_parens)
	mov.l	.L_rsp, RSP	! reset return stack
	mov.l	.L_psp, PSP	! reset parameter stack
	jmp	@NEXT
	 mov	#0, TOS		! clear TOS just in case

	.align 2
.L_psp:		.long	stack_bottom
.L_rsp:		.long	rstack_bottom


!!! Asm part of BYE that returns control to the C code that started
!!! the Forth system.  The stacks are not reset and can be inspected.
ASMWORD("(bye)", bye_parens)
	mov.l	.L_return, r0
	jmp	@r0
	 nop

	.align 2
.L_return:
	.long	.L_BYE_RETURNS_HERE


!!! nop to use as a gdb breakpoint
ASMWORD("breakpoint", breakpoint)
	jmp	@NEXT
	 nop


!!! EXECUTE - CORE 6.1.1370
ASMWORD("execute", execute)
	mov.l	@TOS+, r0	! fetch code address and advance to PFA
	jmp	@r0
	 nop

/*
 * What follows is the very core of the threaded code interpreter.
 *
 * It might make sense to move the following few bits of asm code
 * elsewhere so that it completely fits into a cache line or two
 * without intervening word headers &c.
 */

	!! This should eventually become the DOES> part of ":"
	.align	2
call_code:
	mov.l	IP, @-RSP	! save caller's IP
	bra	.L_next_code_no_spill
	 mov	TOS, IP		! set my IP (consume PFA from TOS)
	!! don't refill TOS here, just skip the spill in next_code
	!! and let next_code set vacant TOS to next word PFA


!!! "exit" - CORE 6.1.1380
ASMWORD("exit", exit_4th)
	mov.l	@RSP+, IP	! pop return address
	!! FALLTHROUGH to "NEXT"
next_code:
	SPILL_TOS
  .L_next_code_no_spill:	! -- entry point for call_code to jump to
	mov.l	@IP+, r0	! r0 = CFA of the next word to execute
				!  and advance IP
	mov.l	@r0, r1		! r1 = *CFA (acctual code)
	mov	#4, TOS		!
	jmp	@r1		! execute word's code
	 add	r0, TOS		! TOS = PFA (for code to consume)


!!! tail-call - note that the difference from EXIT is whether the
!!! order of CFA := @IP and IP := @RSP+
!!!
!!! This makes it possible to add a wrapper around words that examine
!!! the cell after them, like LIT, BRANCH, etc.  Just make the tail
!!! call to such word at the end of the wrapper and it will work.
ASMWORD("(goto)", goto_parens)
	SPILL_TOS
	mov.l	@IP, r0		! r0 = CFA of the next word to execute

	mov.l	@RSP+, IP	! NB: this is what makes it a tail call

	mov.l	@r0, r1		! r1 = *CFA (acctual code)
	mov	#4, TOS		!
	jmp	@r1		! execute word's code
	 add	r0, TOS		! TOS = PFA (for code to consume)


	!! This should eventually become the DOES> part of CODE
DOES_ASM(asm_does)		! code field of asm words points here
	jmp	@TOS		! consume TOS
	 FILL_TOS


/*
 * "does" code of VARIABLE
 *
 * For reference here are both asm and forth versions.
 * See DEFCODE_VAR above, where you can select which one is used.
 */
	!! forth version: ... DOES> ;
DOES_4TH(var_does_4th)
	!! -- nop; leave PFA (variable's address) on the stack
	EXIT_4TH

	!! asm version is basically inlined "nop"
	!!
	!! NB: as you can see, we can remove the indirection and
	!! instead use "next_code" directly as a further optimization!
DOES_ASM(var_does_asm)
	jmp	@NEXT
	 nop


/**
 * Address alignment
 */

!!! ALIGNED - CORE 6.1.0706 
WORD("aligned", aligned)
	.long	lit, 3, plus, lit, -4, and
	EXIT_4TH

!!! CELL+ - CORE 6.1.0880
WORD("cell+", cell_plus)
	.long	lit, 4, plus
	EXIT_4TH

!!! CELL- - nonstandard
WORD("cell-", cell_minus)
	.long	lit, 4, minus
	EXIT_4TH

!!! CELLS - CORE 6.1.0890
WORD("cells", cells)
	.long	two_star, two_star
	EXIT_4TH

!!! CHAR+ - CORE 6.1.0897
WORD("char+", char_plus)
	.long	one_plus
	EXIT_4TH

!!! CHARS - CORE 6.1.0898
WORD("chars", chars)
	!! nop
	EXIT_4TH


/**
 * Definition structure.
 *
 * A word is identified by its CFA (tick &c).  This is implicit source
 * or destination of ">" if omitted.
 */

!!! >BODY - CORE 6.1.0550
WORD(">body", to_body)
	.long	cell_plus
	EXIT_4TH

WORD("body>", body_from)
	.long	cell_minus
	EXIT_4TH

WORD(">link", to_link)
	.long	cell_minus
	EXIT_4TH

WORD("link>", link_from)
	.long	cell_plus
	EXIT_4TH

WORD("name-count", name_count)
	.long	count, lit, 0x1f, and ! upper bits are for flags
	EXIT_4TH

WORD("n>link", n_to_link)
	.long	name_count, plus, aligned
	EXIT_4TH

WORD("name>", name_from)
	.long	n_to_link, link_from
	EXIT_4TH


/**
 * Data-space words
 */

	/*
	 * The value of HERE.  I'm not yet sure we need to keep this
	 * in a register.  We can free r13 for it if we need to.  NEXT
	 * that currently uses r13 can be stored in "pr", in which
	 * case the "jmp @NEXT" becomes just "rts".  We will have to
	 * resotre "pr" after calls to C, but that's not a very large
	 * price to pay.
	 *
	 * For now do this in pure forth to minimize asm code.
	 */
VARIABLE("dp", dp)
	.long	.L_DATA_SPACE

!!! HERE - CORE 6.1.1650
WORD("here", here)
	.long	dp, fetch
	EXIT_4TH

WORD("dp!", dp_store)
	.long	dp, store
	EXIT_4TH

!!! ALIGN - CORE 6.1.0705
WORD("align", align)
	.long	here, aligned, dp_store
	EXIT_4TH

!!! ALLOT - CORE 6.1.0710
WORD("allot", allot)
	.long	here, plus, dp_store
	EXIT_4TH

!!! , - CORE 6.1.0150
WORD(",", comma)
	.long	here, lit, 4, allot, store
	EXIT_4TH

!!! C, - CORE 6.1.0860
WORD("c,", c_comma)
	.long	here, lit, 1, allot, c_store
	EXIT_4TH

!!! PAD - CORE EXT 6.2.2000
!!!     As far as I understand user can use memory below pad, while
!!!     the system uses the area between DP and PAD as hold space.
WORD("pad", pad)
	.long	here, lit, 128, plus
	EXIT_4TH


/**
 * Stack manipualtion
 */


!!! "drop" - CORE 6.1.1260
ASMWORD("drop", drop)
	jmp	@NEXT
	 FILL_TOS


!!! "dup - CORE 6.1.1290
ASMWORD("dup", dup)
	jmp	@NEXT
	 SPILL_TOS


!!! "?dup" - CORE 6.1.0630
ASMWORD("?dup", question_dup)
	tst	TOS, TOS
	bt	1f
	jmp	@NEXT
	 SPILL_TOS		! dup if non-zero
1:	jmp	@NEXT
	 nop			! do nothing if zero


!!! "over" - CORE 6.1.1990
ASMWORD("over", over)
	mov.l	@PSP, r0
	SPILL_TOS
	jmp 	@NEXT
	 mov	r0, TOS


!!! "swap" - CORE 6.1.2260
ASMWORD("swap", swap)
	mov.l	@PSP, r0
	mov.l	TOS, @PSP
	jmp 	@NEXT
	 mov	r0, TOS


!!! "rot" - CORE 6.1.2160
ASMWORD("rot", rot)
	mov.l	@PSP+, r0
	mov.l	@PSP+, r1
	mov.l	r0, @-PSP
	mov.l	TOS, @-PSP
	jmp	@NEXT
	 mov	r1, TOS

!!! -ROT - non-standard
ASMWORD("-rot", minus_rot)
	mov.l	@PSP+, r1
	mov.l	@PSP+, r2
	mov.l	TOS, @-PSP
	mov.l	r2, @-PSP
	jmp	@NEXT
	 mov	r1, TOS


!!! NIP - CORE EXT 6.2.1930
ASMWORD("nip", nip)
	jmp	@NEXT
	 add	#4, PSP

!!! TUCK - CORE EXT 6.2.2300
ASMWORD("tuck", tuck)
	mov.l	@PSP+, r0
	SPILL_TOS
	jmp	@NEXT
	 mov.l	r0, @-PSP

!!! PICK - CORE EXT 6.2.2030
ASMWORD("pick", pick)
	mov	PSP, r0
	shll2	TOS
	jmp	@NEXT
	 mov.l	@(r0, TOS), TOS

!!! ROLL - CORE EXT 6.2.2150
ASMWORD("roll", roll)
#  define DST r2
#  define SRC r3
	mov	TOS, DST
	shll2	DST
	add	PSP, DST
	mov	DST, SRC

	mov.l	@DST, r0

1:
	add	#-4, SRC
	mov.l	@SRC, r1
	mov.l	r1, @DST
	add	#-4, DST

	dt	TOS
	bf	1b

	add	#4, PSP

	jmp	@NEXT
	 mov	r0, TOS

!!! "2drop" - CORE 6.1.0370
ASMWORD("2drop", two_drop)
	add	#4, PSP
	jmp	@NEXT
	 FILL_TOS

!!! "2dup" - CORE 6.1.0380
ASMWORD("2dup", two_dup)
	mov.l	@PSP, r0
	SPILL_TOS
	jmp	@NEXT
	 mov.l	r0, @-PSP


!!! 2SWAP - CORE 6.1.0430
ASMWORD("2swap", two_swap)
	mov.l	@PSP, r1
	mov.l	@(4, PSP), r2
	mov.l	@(8, PSP), r3
	mov.l	r1, @(8, PSP)
	mov.l	TOS, @(4, PSP)
	mov.l	r3, @PSP
	jmp	@NEXT
	 mov	r2, TOS


!!! 2OVER - CORE 6.1.0400
ASMWORD("2over", two_over)
	mov.l	@(4, PSP), r0
	mov.l	@(8, PSP), r1
	SPILL_TOS
	mov.l	r1, @-PSP
	jmp	@NEXT
	 mov	r0, TOS


!!! >R - CORE 6.1.0580
ASMWORD(">r", to_r)
	mov.l	TOS, @-RSP
	jmp	@NEXT
	 FILL_TOS

!!! R> - CORE 6.1.2060
ASMWORD("r>", r_from)
	SPILL_TOS
	jmp	@NEXT
	 mov.l	@RSP+, TOS


!!! R@ - CORE 6.1.2070
ASMWORD("r@", r_fetch)
	SPILL_TOS
	jmp	@NEXT
	 mov.l	@RSP, TOS

!!! 2>R CORE EXT 6.2.0340
ASMWORD("2>r", two_to_r)
	mov.l	@PSP+, r0
	mov.l	r0, @-RSP
	mov.l	TOS, @-RSP
	jmp	@NEXT
	 FILL_TOS

!!! 2R> - CORE EXT 6.2.0410 
ASMWORD("2r>", two_r_from)
	SPILL_TOS
	mov.l	@RSP+, TOS
	mov.l	@RSP+, r0
	jmp	@NEXT
	 mov.l	r0, @-PSP

!!! 2R@ - CORE EXT 6.2.0415
ASMWORD("2r>", two_r_fetch)
	SPILL_TOS
	mov.l	@RSP, TOS
	mov.l	@(4, RSP), r0
	jmp	@NEXT
	 mov.l	r0, @-PSP


!!! "@" - CORE 6.1.0650
ASMWORD("@", fetch)
	jmp	@NEXT
	 mov.l	@TOS, TOS


!!! "!" - CORE 6.1.0010
ASMWORD("!", store)
	mov.l	@PSP+, r0	! value
	mov.l	r0, @TOS	! store value
	jmp	@NEXT
	 FILL_TOS


!!! "+!" - CORE 6.1.0130
ASMWORD("+!", plus_store)
	mov.l	@PSP+, r0	! value increment
	mov.l	@TOS, r1	! fetch current value
	add	r1, r0
	mov.l	r0, @TOS	! store new value
	jmp	@NEXT
	 FILL_TOS


!!! "1+!" - nonstandard
ASMWORD("1+!", one_plus_store)
	mov.l	@TOS, r1	! fetch current value
	add	#1, r1		! increment
	mov.l	r1, @TOS	! store sum
	jmp	@NEXT
	 FILL_TOS


!!! C@ - CORE 6.1.0870
ASMWORD("c@", c_fetch)
	mov.b	@TOS, TOS	! loaded byte is sign-extended
	jmp	@NEXT
	 extu.b	TOS, TOS	! we want it zero-extended


!!! C! - CORE 6.1.0850
ASMWORD("c!", c_store)
	mov.l	@PSP+, r0	! value
	mov.b	r0, @TOS	! store value
	jmp	@NEXT
	 FILL_TOS


!!! COUNT - CORE 6.1.0980
ASMWORD("count", count)
	mov.b	@TOS+, r0	! fetch count (sign-extended), advance ptr
	SPILL_TOS		! string contents address
	jmp	@NEXT
	 extu.b	r0, TOS		! zero-extend the count


!!! "lit" - auxiliary for "literal" (CORE 6.1.1780)
ASMWORD("lit", lit)
	SPILL_TOS
	jmp	@NEXT
	 mov.l	@IP+, TOS


!!! "branch" - auxiliary for "else" &co
ASMWORD("branch", branch)
	jmp	@NEXT
	 mov.l	@IP+, IP


!!! "?branch" - auxiliary for "if" &co
ASMWORD("?branch", question_branch)
	tst	TOS, TOS
	bf/s	1f		! branch is taken when TOS is false(!)
	 FILL_TOS
	jmp	@NEXT
	 mov.l	@IP+, IP	! take branch
1:	jmp	@NEXT
	 add	#4, IP		! skip branch address


!!! Add with overflow check
ASMWORD("+?", plus_question)
	mov.l	@PSP+, r0
	addv	r0, TOS
	SPILL_TOS
	movt	TOS
	jmp	@NEXT
	 neg	TOS, TOS


!!! (do) - auxiliary for "do"
!!!     ( limit first -- R: addr limit current )
WORD("(do)", do_parens)
	.long	r_from			! next cell after compiled "(do)"
	.long	dup, fetch, to_r	! stash end of loop address for "leave"
	.long	rot			! first leave limit --
	!! offset limit so that last + limit causes overflow
	.long	lit, 0x80000000, swap, minus, to_r
	.long	swap			! leave first --
	!! offset first accordingly too
	.long	r_fetch, plus, to_r
	.long	cell_plus, to_r		! skip literal after (do)
	EXIT_4TH


!!! (?do) - auxiliary for "?do";
!!!     like "do", but does nothing if first == limit
WORD("(?do)", question_do_parens)
	.long	two_dup, equals, OR_ELSE(0f)
	.long	    two_drop
	.long	    goto_parens, branch ! skip loop
0:
	.long	goto_parens, do_parens	! setup loop


!!! (+LOOP) - auxiliary for +LOOP with the address of the beginning of
!!!     the loop body compiled as literal after it.
!!! 	( increment -- ) ( R: addr limit current )
WORD("(+loop)", plus_loop_parens)
	.long	r_from, swap		! take (+loop)'s return address

	.long	r_from			! current
	.long	plus_question		! add increment with overflow check
	.long	OR_ELSE(1f)

	!! overflow means we hit the limit, leave the loop
	.long	drop		! current
	.long	r_from, drop	! limit
	.long	r_from, drop	! leave address
	.long	cell_plus, to_r, exit_4th

1:	!! no overflow, update current and continue
	.long	to_r			! new current
	.long	to_r			! return address
	.long	goto_parens, branch	! to beginning of loop


!!! (LOOP) - auxiliary for LOOP.  Does "1 (+LOOP)" via tail call.
WORD("(loop)", loop_parens)
	.long	lit, 1, goto_parens, plus_loop_parens


!!! I - CORE 6.1.1680
WORD("i", i_4th)
	.long	r_from		! return address
	.long	two_r_fetch	! limit and current
	.long	swap, minus	! normalize current - see (do)
	.long	swap, to_r	! restore return address
	EXIT_4TH


!!! LEAVE - CORE 6.1.1760
!!! 	( R: addr limit current )
WORD("leave", leave)
	.long	r_from, drop		! return address
	.long	two_r_from, two_drop	! loop limit and counter
	EXIT_4TH			! to the loop's end


!!! UNLOOP - CORE 6.1.2380
WORD("unloop", unloop)
	.long	r_from			! save return address
	.long	two_r_from, two_drop	! loop limit and counter
	.long	r_from, drop		! leave address
	.long	to_r			! restore return address
	EXIT_4TH


!!! "false" - CORE-EXT 6.2.1485
ASMWORD("false", false)
	SPILL_TOS
	jmp	@NEXT
	 mov	#0, TOS


!!! "true" - CORE-EXT 6.2.2298
ASMWORD("true", true)
	SPILL_TOS
	jmp	@NEXT
	 mov	#-1, TOS


!!! "1" - nonstandard
ASMWORD("1", one)
	SPILL_TOS
	jmp	@NEXT
	 mov	#1, TOS


!!! "2" - nonstandard
ASMWORD("2", two)
	SPILL_TOS
	jmp	@NEXT
	 mov	#2, TOS


!!! "abs" - CORE 6.1.0690
ASMWORD("abs", abs)
	cmp/pz	TOS
	bt	1f
	neg	TOS, TOS
1:	jmp	@NEXT
	 nop


!!! "negate" - CORE 6.1.1910
ASMWORD("negate", negate)
	jmp	@NEXT
	 neg	TOS, TOS


!!! "+" - CORE 6.1.0120
ASMWORD("+", plus)
	mov.l	@PSP+, r0
	jmp	@NEXT
	 add	r0, TOS


!!! "-" - CORE 6.1.0160
ASMWORD("-", minus)
	mov.l	@PSP+, r0
	sub	TOS, r0
	jmp	@NEXT
	 mov	r0, TOS


!!! "*" - CORE 6.1.0090
ASMWORD("*", star)
	mov.l	@PSP+, r0
	mul.l	r0, TOS
	jmp	@NEXT
	 sts	macl, TOS


!!! "/" - CORE 6.1.0230
!!!	sdivsi3 tweaked to return result in TOS
ASMWORD("/", slash)
	tst	TOS, TOS
	mov	TOS, r1		! r1 = divisor
	bt/s	.L_divide_by_zero
	 FILL_TOS		! TOS = dividend

	mov	#0, r2		! this is a fast way to set ...
	div0s	r2, TOS		! ... T := sign(dividend)
	subc	r3, r3		! sign-extend dividend into 64 bit {TOS, r3}
	subc	r2, TOS		! if dividend < 0 convert to 1-complement

	div0s	r1, r3		! setup division {TOS, r3} / r1
	.rept /* DIVSTEP */ 32 /* times { */
	rotcl	TOS		! feed bits of TOS into division via T
	div1	r1, r3
	.endr /* } */
	rotcl	TOS		! quotient in 1-complement
	jmp	@NEXT
	 addc	r2, TOS		! if quotient < 0 convert to 2-complement

  .L_divide_by_zero:
	jmp	@NEXT
	 mov	#0, TOS


!!! "mod" - CORE 6.1.1890
WORD("mod", mod)
	.long two_dup, slash, star, minus
	EXIT_4TH


!!! "/mod" - CORE 6.1.0240
WORD("/mod", slash_mod)
	.long two_dup, slash
	.long tuck, star
	.long lit, 2, roll
	.long swap, minus
	.long swap
	EXIT_4TH

	
!!! U/ - nonstandard unsigned division
!!!	udivsi3 adjusted for register usage
ASMWORD("u/", u_slash)
	tst	TOS, TOS
	mov	TOS, r1		! r1 = divisor
	bt/s	.L_u_divide_by_zero
	 FILL_TOS		! TOS = dividend

	mov	#0, r0
	div0u
	.rept /* DIVSTEP */ 32 /* times { */
	rotcl	TOS		! feed bits of dividend into division via T
	div1	r1, r0
	.endr /* } */
	jmp	@NEXT
	 rotcl	TOS

  .L_u_divide_by_zero:
	jmp	@NEXT
	 mov	#0, TOS


!!! U/MOD - nonstandard unsigned division with remainder
WORD("u/mod", u_slash_mod)
	.long two_dup, u_slash
	.long tuck, star
	.long lit, 2, roll
	.long swap, minus
	.long swap
	EXIT_4TH


!!! "1+" - CORE 6.1.0290
ASMWORD("1+", one_plus)
	jmp	@NEXT
	 add	#1, TOS


!!! "1-" - CORE 6.1.0300
ASMWORD("1-", one_minus)
	jmp	@NEXT
	 add	#-1, TOS


!!! "2*" - CORE 6.1.0320
ASMWORD("2*", two_star)
	jmp	@NEXT
	 shal	TOS


!!! "2/" - CORE 6.1.0330
ASMWORD("2/", two_slash)
	jmp	@NEXT
	 shar	TOS


!!! "0<" - CORE 6.1.0250
ASMWORD("0<", zero_less)
	mov	#0, r0
	cmp/gt	TOS, r0
	movt	TOS
	jmp	@NEXT
	 neg	TOS, TOS


!!! "0>" - CORE-EXT 6.2.0280
ASMWORD("0>", zero_greater)
	cmp/pl	TOS
	movt	TOS
	jmp	@NEXT
	 neg	TOS, TOS


!!! "0=" - CORE 6.1.0270
ASMWORD("0=", zero_equals)
	tst	TOS, TOS
	movt	TOS
	jmp	@NEXT
	 neg	TOS, TOS


!!! "0<>" - CORE-EXT 6.2.0260
ASMWORD("0<>", zero_not_equals)
	tst	TOS, TOS
	movt	TOS
	neg	TOS, TOS
	jmp	@NEXT
	 not	TOS, TOS


!!! "<" - CORE 6.1.0480
ASMWORD("<", less_than)
	mov.l	@PSP+, r0
	cmp/gt	r0, TOS
	movt	TOS
	jmp	@NEXT
	 neg	TOS, TOS


!!! ">" - CORE 6.1.0540
ASMWORD(">", greater_than)
	mov.l	@PSP+, r0
	cmp/gt	TOS, r0
	movt	TOS
	jmp	@NEXT
	 neg	TOS, TOS


!!! "=" - CORE 6.1.0530
ASMWORD("=", equals)
	mov.l	@PSP+, r0
	cmp/eq	TOS, r0
	movt	TOS
	jmp	@NEXT
	 neg	TOS, TOS


!!! "<>" - CORE-EXT 6.2.0500
ASMWORD("<>", not_equals)
	mov.l	@PSP+, r0
	cmp/eq	TOS, r0
	movt	TOS
	neg	TOS, TOS
	jmp	@NEXT
	 not	TOS, TOS


!!! "<=" - nonstandard
WORD("<=", less_than_equals)
	.long	greater_than, zero_equals
	EXIT_4TH


!!! ">=" - nonstandard
WORD(">=", greater_than_equals)
	.long	less_than, zero_equals
	EXIT_4TH


!!! "u<" - CORE 6.1.2340
ASMWORD("u<", u_less_than)
	mov.l	@PSP+, r0
	cmp/hi	r0, TOS
	movt	TOS
	jmp	@NEXT
	 neg	TOS, TOS


!!! "u>" - CORE-EXT 6.2.2350
ASMWORD("u>", u_greater_than)
	mov.l	@PSP+, r0
	cmp/hi	TOS, r0
	movt	TOS
	jmp	@NEXT
	 neg	TOS, TOS


!!! WITHIN - CORE EXT 6.2.2440
!!!     ( test low high -- flag )
!!!     code from the ANS A.6.2.2440
WORD("within", within)
	.long	over, minus	! high - low
	.long	to_r
	.long	minus		! test - low
	.long	r_from
	.long	u_less_than
	EXIT_4TH


!!! "invert" - CORE 6.1.1720
ASMWORD("invert", invert)
	jmp	@NEXT
	 not	TOS, TOS


!!! "and" - CORE 6.1.0720
ASMWORD("and", and)
	mov.l	@PSP+, r0
	jmp	@NEXT
	 and	r0, TOS


!!! "or" - CORE 6.1.1980
ASMWORD("or", or)
	mov.l	@PSP+, r0
	jmp	@NEXT
	 or	r0, TOS


!!! "xor" - CORE 6.1.2490
ASMWORD("xor", xor)
	mov.l	@PSP+, r0
	jmp	@NEXT
	 xor	r0, TOS


!! LSHIFT - CORE 6.1.1805
ASMWORD("lshift", lshift)
	extu.b	TOS, r1		! shift amount (ensure positive)
	mov.l	@PSP+, TOS	! the number to shift
	jmp	@NEXT
	 shld	r1, TOS


!! RSHIFT - CORE 6.1.2162
ASMWORD("rshift", rshift)
	extu.b	TOS, r1		! shift amount (ensure positive)
	mov.l	@PSP+, TOS	! the number to shift
	neg	r1, r1		! negative means shift right
	jmp	@NEXT
	 shld	r1, TOS


!!! MAX - CORE 6.1.1870
ASMWORD("max", max)
	mov.l	@PSP+, r0
	cmp/ge	r0, TOS		! TOS > r0?
	bt	0f

	jmp	@NEXT
	 mov	r0, TOS

0:	jmp	@NEXT
	 nop


!!! MIN - CORE 6.1.1880
ASMWORD("min", min)
	mov.l	@PSP+, r0
	cmp/ge	TOS, r0		! TOS < r0?
	bt	0f

	jmp	@NEXT
	 mov	r0, TOS

0:	jmp	@NEXT
	 nop


!!! ====================================================================
!!! Double-cell numbers

!!! 3.1.4.1 Double-cell integers
!!! 
!!!   On the stack, the cell containing the most significant part of a
!!!   double-cell integer shall be above the cell containing the least
!!!   significant part.

!!! E.3.1 Big endian vs. little endian
!!! 
!!!   When a cell pair is moved from the stack to memory with 2!, the
!!!   cell that was on top of the stack is placed at the lower memory
!!!   address.


!!! "2LIT" - auxiliary for "2LITERAL" DOUBLE 8.6.1.0390
ASMWORD("2lit", two_lit)
	SPILL_TOS
	mov.l	@IP+, TOS	! lower address goes to the top
	mov.l	@IP+, r0
	jmp	@NEXT
	 mov.l	r0, @-PSP


!!! 2@ - CORE 6.1.0350
ASMWORD("2@", two_fetch)
	mov.l	@(4, TOS), r0
	mov.l	@TOS, TOS	! lower address goes to the top
	jmp	@NEXT
	 mov.l	r0, @-PSP


!!! 2! - CORE 6.1.0310
ASMWORD("2!", two_store)
	mov.l	@PSP, r0	! top -> lower address
	mov.l	@(4, PSP), r1
	mov.l	r0, @TOS
	mov.l	r1, @(4, TOS)
	mov.l	@(8, PSP), TOS	! fill tos
	jmp	@NEXT
	 add	#12, PSP


!!! "s>d" - CORE 6.1.2170
ASMWORD("s>d", s2d)
	SPILL_TOS		! least significant cell is below
	shll	TOS		! shift sign into the T bit
	jmp	@NEXT
	 subc	TOS, TOS	! most significant cell = 0 - T


!!! D>S - DOUBLE 8.6.1.1140
WORD("d>s", d2s)
	.long	drop
	EXIT_4TH


!!! DNEGATE - DOUBLE 8.6.1.1230
WORD("dnegate", dnegate)
	mov.l	@PSP, r0	! lower cell
	clrt
	negc	r0, r0
	negc	TOS, TOS
	jmp	@NEXT
	 mov.l	r0, @PSP


!!! D+ - DOUBLE 8.6.1.1040 
ASMWORD("d+", d_plus)
	mov.l	@PSP+, r2	! lo(d2)
	mov.l	@PSP+, r1	! hi(d1)
	mov.l	@PSP+, r0	! lo(d1)
	clrt
	addc	r0, r2		! lo
	addc	r1, TOS		! hi
	jmp	@NEXT
	 mov.l	r2, @-PSP


!!! M* - CORE 6.1.1810
!!!     ( n1 n2 -- d ) signed mixed multiplication (32x32 -> 64)
ASMWORD("m*", m_star)
	mov.l	@PSP+, r0
	dmuls.l	r0, TOS
	sts	macl, r0
	mov.l	r0, @-PSP
	jmp	@NEXT
	 sts	mach, TOS


!!! UM* - CORE 6.1.2360
!!!     ( n1 n2 -- d ) unsigned mixed multiplication (32x32 -> 64)
ASMWORD("um*", um_star)
	mov.l	@PSP+, r0
	dmulu.l	r0, TOS
	sts	macl, r0
	mov.l	r0, @-PSP
	jmp	@NEXT
	 sts	mach, TOS


!!! SM/REM - CORE 6.1.2214
!!!     ( d1 n1 -- n2 n3 ) mixed signed division/remainder (64/32 -> 32)
ASMWORD("sm/rem", sm_slash_rem)
	mov.l	1f, r0
	mov.l	@PSP+, r5	! hi(d1)
	mov.l	@PSP+, r4	! lo(d1)
	mov	TOS, r6		! lo(n1) = n1
	mov	TOS, r7
	shll	r7
	subc	r7, r7		! hi(n1) = sign(n1)
	add	#-12, sp	! 4 for pointer (argument), 8 for value
	mov.l	sp, @sp
	jsr	@r0
	 nop
	mov.l	r0, @-PSP	! lo(div)
	mov.l	@(8, sp), TOS	! lo(rem)
	add	#12, sp
	jmp	@NEXT
	 nop
	.align	2
1:	.long	_C_LABEL(__divmoddi4)


!!! ====================================================================
!!! Strings


!!! CMOVE - STRING 17.6.1.0910
!!!     ( src dst len -- )
!!! NB: char-by-char left-to-right copy is required by the standard
ASMWORD("cmove", cmove)
	!!	TOS		! len
	mov.l	@PSP, r1	! dst
	mov.l	@(4, PSP), r0	! src

	tst	TOS, TOS	! len == 0?
	bt	9f

1:	mov.b	@r0+, r2
	mov.b	r2, @r1
	dt	TOS
	bf/s	1b
	 add	#1, r1

9:	add	#8, PSP
	jmp	@NEXT
	 FILL_TOS


!!! CMOVE> - STRING 17.6.1.0920
!!!     ( src dst len -- )
!!! NB: char-by-char right-to-left copy is required by the standard
ASMWORD("cmove>", cmove_up)
	!!	TOS		! len
	mov.l	@PSP, r1	! dst
	mov.l	@(4, PSP), r0	! src

	tst	TOS, TOS	! len == 0?
	bt	9f

	add	TOS, r0
	add	TOS, r1

1:	add	#-1, r0
	mov.b	@r0, r2
	mov.b	r2, @-r1
	dt	TOS
	bf	1b

9:	add	#8, PSP
	jmp	@NEXT
	 FILL_TOS


!!! COMPARE - STRING 17.6.1.0935 
ASMWORD("compare", compare)
	!!	       TOS	! u2
	mov.l	@PSP+, r7	! ca2
	mov.l	@PSP+, r6	! u1
	mov.l	@PSP+, r5	! ca1

	tst	r6, r6		! u1 == 0?
.Loop:
	bf.s	.L_u1_pos
	 tst	TOS, TOS	! u2 == 0?

	movt	TOS		!  0 if u2 != 0; 1 if u2 == 0
	jmp	@NEXT
	 add	#-1, TOS	! -1 if u2 != 0; 0 if u2 == 0

.L_u1_pos:
	bf.s	.L_u1u2_pos
	 nop
	jmp	@NEXT
	 mov	#1, TOS

.L_u1u2_pos:
	mov.b	@r5+, r0
	add	#-1, r6

	mov.b	@r7+, r1
	add	#-1, TOS

	extu.b	r0, r0
	extu.b	r1, r1

	cmp/eq	r0, r1
	bt.s	.Loop
	 tst	r6, r6

	cmp/hi	r1, r0		! r0 > r1?
	bt	1f

	jmp	@NEXT
	 mov	#-1, TOS	! *ca1 < *ca2

1:	jmp	@NEXT
	 mov	#1, TOS		! *ca1 > *ca2


!!! ====================================================================
!!! I/O - Common

!!! "base" - CORE 6.1.0750
VARIABLE("base", base)
	.long 10


!!! DECIMAL - CORE 6.1.1170
WORD("decimal", decimal)
	.long	lit, 10, base, store
	EXIT_4TH


!!! HEX - CORE EXT 6.2.1660
WORD("hex", hex)
	.long	lit, 16, base, store
	EXIT_4TH


!!! ====================================================================
!!! I/O - Output

!!! "bl" - CORE 6.1.0770
CONSTANT("bl", bl) .long 0x20


!!! "emit" - CORE 6.1.1320
ASMWORD("emit", emit)
	mov.l	1f, r0
	mov	TOS, r4
	jsr	@r0
	 FILL_TOS
	jmp	@NEXT
	 nop
	.align	2
1:	.long	_C_LABEL(emit_impl)


!!! "cr" - CORE 6.1.0990
WORD("cr", cr)
	.long	lit, '\n, emit
	EXIT_4TH


!!! "space" - CORE 6.1.2220
WORD("space", space)
	.long	bl, emit
	EXIT_4TH


!!! "spaces" - CORE 6.1.2230
WORD("spaces", spaces)
	.long	dup, zero_greater, OR_ELSE(9f)
	.long	lit, 0, do_parens, 9f
1:	.long	space
	.long	loop_parens, 1b
9:	EXIT_4TH


!!! "type" - CORE 6.1.2310
ASMWORD("type", type)
	mov.l	1f, r0
	mov	TOS, r5
	mov.l	@PSP+, r4
	jsr	@r0
	 FILL_TOS
	jmp	@NEXT
	 nop
	.align	2
1:	.long	_C_LABEL(type_impl)


VARIABLE("hld", hld)
	.long	0

!!! <# - CORE 6.1.0490
WORD("<#", less_hash)
	.long	pad, hld, store
	EXIT_4TH

!!! HOLD - CORE 6.1.1670
WORD("hold", hold)
	.long	lit, -1, hld, plus_store
	.long	hld, fetch, c_store
	EXIT_4TH


!!! #> - CORE 6.1.0040
WORD("#>", hash_greater)
	.long	drop /* XXX: two_drop */
	.long	hld, fetch, pad, over, minus
	EXIT_4TH

!!! SIGN - CORE 6.1.2210
WORD("sign", sign)
	.long	zero_less, OR_ELSE(1f)
	.long	    lit, 45 /* '-' */, hold
1:	EXIT_4TH

!!! # - CORE 6.1.0030
!!!     XXX: FIXME: single cell!
WORD("#", hash)
	.long	base, fetch, u_slash_mod, swap ! NB: unsigned
	.long	dup, lit, 9, greater_than, OR_ELSE(1f)
	.long		lit, 'a - 10
	.long		branch, 2f
1:		/* else */
	.long		lit, '0
2:		/* then */
	.long	plus, hold
	EXIT_4TH

!!! #S - CORE 6.1.0050
!!!     XXX: FIXME: single cell!
WORD("#s", hash_s)
1:	.long	hash
	.long	dup, zero_equals
	.long	OR_ELSE(1b)
	EXIT_4TH

!!!     XXX: FIXME: single cell!
WORD("(.)", dot_parens)
	.long	dup, abs, less_hash, hash_s, swap, sign, hash_greater
	EXIT_4TH


WORD(".", dot)
	.long	dot_parens, type, space
	EXIT_4TH


!!! ====================================================================
!!! I/O - Input

!!! "accept" - CORE 6.1.0695
ASMWORD("accept", accept_4th)	! XXX: avoid name clash with accept(2)
	mov.l	1f, r0
	mov	TOS, r5
	mov.l	@PSP+, r4
	jsr	@r0
	 FILL_TOS
	SPILL_TOS
	jmp	@NEXT
	 mov	r0, TOS		! push +n2 returned by the C code
	.align	2
1:	.long	_C_LABEL(accept_impl)


!!! TIB - CORE EXT 6.2.2290
CONSTANT("tib", tib)
	.long	terminal_input_buffer


!!! #TIB - CORE EXT 6.2.0060
VARIABLE("#tib", number_tib)
	.long	0


!!! >IN - CORE 6.1.0560
VARIABLE(">in", to_in)
	.long	0

!!! REFILL - CORE EXT 6.2.2125 
WORD("refill", refill)
	.long	tib, lit, 4096, accept_4th
	.long	dup, OR_ELSE(9f)
	.long	number_tib, store
	.long	lit, 0, to_in, store
	.long	true
9:	EXIT_4TH

!!! SOURCE - CORE 6.1.2216
WORD("source", source)
	.long	tib, number_tib, fetch
	EXIT_4TH

WORD("skip-spaces", skip_spaces)
	.long	source, swap
1:
	.long	over			! #tib
	.long	to_in, fetch, swap, less_than
	.long	OR_ELSE(9f)		! exit if >in greater/equal #tib
	.long	dup, to_in, fetch, plus, c_fetch
	.long	bl, equals
	.long	OR_ELSE(9f)		! exit if not a space
	.long	to_in, one_plus_store	! advance >in
	.long	branch, 1b		! repeat
9:
	.long	two_drop
	EXIT_4TH

!!! PARSE - CORE EXT 6.2.2008
!!!     ( delim "ccc<delim>" -- str len )
WORD("parse", parse)
	.long	source, nip, to_in, fetch	! parse area limits
	.long	two_dup, less_than_equals
	.long	OR_ELSE(1f)

	!! parse area is exhausted, return empty word
	.long	two_drop	! limits
	.long	drop		! delim
	.long	source, drop, to_in, fetch, plus ! result address
	.long	lit, 0				 ! result length
	.long	exit_4th
1:
	.long	dup, to_r		! stash away current >in
	.long	lit, 0, minus_rot	! init result length
	.long	do_parens, 8f
2:	!! while >in is exceeding source length

	.long	to_in, one_plus_store ! >in is always advanced
	.long	over		      ! get a copy of delim
	.long	source, drop, i_4th, plus, c_fetch ! current char
	.long	equals, OR_ELSE(7f)	! .. is a delim?
	.long	    leave
7:	.long	one_plus			   ! increment result length
	.long	loop_parens, 2b
8:
	.long	nip			   ! get rid of delim
	.long	source, drop, r_from, plus ! result address (via old >in)
	.long	swap			   ! result length on top

	EXIT_4TH

WORD("parse-word", parse_word)
	.long	skip_spaces, bl, parse
	EXIT_4TH


!!! ( - CORE 6.1.0080
IMMWORD("(", paren)
	.long	lit, '), parse, two_drop
	EXIT_4TH


!!! \ - CORE EXT 6.2.2535
IMMWORD("\\", backslash)
	.long	lit, '\n, parse, two_drop
	EXIT_4TH


!!! CHAR - CORE 6.1.0895
WORD("char", char)
	.long	parse_word, OR_ELSE(0f)
	.long	    c_fetch, exit_4th
0:	.long	drop, lit, 0
	EXIT_4TH


!!! [CHAR] - CORE 6.1.2520
IMMWORD("[char]", char_brackets)
	.long	question_comp
	.long	parse_word, OR_ELSE(0f)
	.long	    c_fetch, branch, 1f
	!!	else
0:	.long	    drop, lit, 0
1:
	.long	    compile, lit, comma
	EXIT_4TH


!!! ( xt "ccc<quote>" -- )
WORD("compile\"", compile_quote)
	.long	lit, '\", parse	! XXX: handle failure
	!! compile: xt <len> <len bytes of string>
	.long	rot, compile_comma	! compile run-time word
	.long	dup, comma	! string length
	.long	here, swap	! copy destination
	.long	dup, allot	! reserve space
	.long	cmove, align	! copy string
	EXIT_4TH


WORD("(s\")", s_quote_parens)
	.long	r_from, dup
	.long	cell_plus, swap			! address of string
	.long	fetch				! length
	.long	two_dup, plus, aligned, to_r	! skip string
	EXIT_4TH


!!! S" - 6.1.2165
IMMWORD("s\"", s_quote)
	.long	lit, s_quote_parens, compile_quote
	EXIT_4TH


WORD("(.\")", dot_quote_parens)
	.long	r_from, dup
	.long	cell_plus, swap			! address of string
	.long	fetch				! length
	.long	two_dup, plus, aligned, to_r	! skip string
	!! the above is a copy of (s")
	.long	type
	EXIT_4TH


!!! ." - 6.1.0190
IMMWORD(".\"", dot_quote)
	.long	lit, dot_quote_parens, compile_quote
	EXIT_4TH


WORD("(abort\")", abort_quote_parens)
	.long	r_from, dup
	.long	cell_plus, swap			! address of string
	.long	fetch				! length
	.long	two_dup, plus, aligned, to_r	! skip string
	!! the above is a copy of (s")
	.long	rot, zero_equals, OR_ELSE(0f)
	.long	    two_drop, exit_4th
0:
	.long	type, cr	! should it call CR?
	.long	quit ! XXX: FIXME: must be ABORT, but see QUIT definition
	EXIT_4TH


!!! ABORT" - CORE 6.1.0680
IMMWORD("abort\"", abort_quote)
	.long	lit, abort_quote_parens, compile_quote
	EXIT_4TH


/*
 * >NUMBER and related words
 */

!!! ( base - '0' upper ) range of (arabic) digits; for "within"
WORD("base-0", base_zero)
	.long	dup, lit, 10, less_than_equals
	.long	OR_ELSE(1f)
	.long	    lit, '0, tuck, plus
	.long	branch, 9f
1:	.long	    drop, lit, '0, lit, '9 + 1
9:	EXIT_4TH

!!! ( base  - 'a' upper) range of alphabetic digits; for within 
WORD("base-a", base_a)
	.long	lit, 10, minus, lit, 'a, tuck, plus
	EXIT_4TH

!!! ( char base - value true | false )
WORD("digit?", is_digit)
	.long	swap, to_r	! stash away the char

	!! a "normal" digit from '0'.. range?
	.long	dup, base_zero
	.long	r_fetch
	.long	minus_rot, within
	.long	OR_ELSE(1f)
	.long	drop, r_from, lit, '0, minus, true
	.long	exit_4th
1:
	!! does base use alphabet digits?
	.long	dup, lit, 10, greater_than
	.long	OR_ELSE(9f)

	!! a digit from 'a'.. range?
	.long	dup, base_a
	.long	r_from, lit, 0x20, or, dup, to_r ! downcased
	.long	minus_rot, within
	.long	OR_ELSE(9f)
	.long	drop, r_from, lit, 'a, minus, lit, 10, plus, true
	.long	exit_4th
9:
	!! not a digit
	.long	drop, r_from, drop, false
	EXIT_4TH

!!! >NUMBER - CORE 6.1.0570
!!! ( ud1 s1 l1 - ud2 s2 l2)
WORD(">number", to_number)
	.long	two_dup, to_r, to_r	! stash away initial string
	.long	over, plus, swap	! pointer loop range
	.long	question_do_parens, 9f
1:
	.long	i_4th, c_fetch, base, fetch, is_digit
	.long	OR_ELSE(2f)

	.long	    swap			! get hi above digit
	.long	    base, fetch, um_star, drop	! lo(hi*base)
		    !! digit lo(hi*base) - is already a double cell sum!
	.long	    rot				! lo
	.long	    base, fetch, um_star	! double cell lo*base
	.long	    d_plus

	.long	branch, 3f
2:
	!! bad character, compute new string position
	.long	i_4th		! current pointer (NB: while we still can)
	.long	unloop		! clean up return stack for exit
	.long	dup			! new position
	.long	r_from, minus		! distance from original position
	.long	r_from, swap, minus	! decrease length by it
	.long	exit_4th
3:
	.long	loop_parens, 1b
9:
	!! loop ended normally, so we used up all the string
	.long	two_r_from, plus, lit, 0 ! 
	EXIT_4TH


!!! addr len - 0 | n 1 | d 2
WORD("number?", is_number)
	.long	base, fetch, to_r ! save base in case of a sigil
	.long	false, to_r	  ! minus sign

	!! if there's just one char, it cannot be a sigil or a minus sign
	.long	dup, lit, 1, greater_than, OR_ELSE(.L_check_number)

	!! Forth 200x #<dec>, $<hex>, %<bin> base sigils
	.long	over, c_fetch

	.long	dup, lit, '# /*'*/, equals, OR_ELSE(1f)
	.long	    lit, 10, base, store, branch, .L_base_sigil
1:
	.long	dup, lit, '$ /*'*/, equals, OR_ELSE(2f)
	.long	    lit, 16, base, store, branch, .L_base_sigil
2:
	.long	dup, lit, '% /*'*/, equals, OR_ELSE(3f)
	.long	    lit, 2, base, store, branch, .L_base_sigil
3:
	!!	else
	.long	    drop, branch, .L_check_sign

.L_base_sigil:
	.long	drop		    ! sigil
	.long	swap, lit, 1, plus  ! ++address
	.long	swap, lit, 1, minus ! --length

	!! if there's just one char, it cannot be a minus sign
	.long	dup, lit, 1, greater_than, OR_ELSE(.L_check_number)

.L_check_sign:
	.long	over, c_fetch, lit, '-, equals
	.long	OR_ELSE(.L_check_number)

	.long	r_from, drop, true, to_r ! note that we have a minus

	.long	swap, lit, 1, plus  ! ++address
	.long	swap, lit, 1, minus ! --length

.L_check_number:
	!! try to convert to number -> ud addr len
	.long	two_lit, 0, 0, two_swap, to_number

	.long	dup, zero_equals, OR_ELSE(.L_check_trailer)

	!! single cell number
	.long	two_drop, drop
	.long	r_from, OR_ELSE(0f)
	.long	    negate
0:
	.long	r_from, base, store
	.long	lit, 1, exit_4th

.L_check_trailer:
	.long	dup, lit, 1, equals, OR_ELSE(.L_failed)

	.long	over, c_fetch, lit, '., equals
	.long	OR_ELSE(.L_failed)

	!! dot at the end means double-cell number
	.long	two_drop
	.long	r_from, OR_ELSE(0f)
	.long	    dnegate
0:
	.long	r_from, base, store
	.long	lit, 2, exit_4th

.L_failed:
	.long	two_drop, two_drop
	.long	r_from, drop
	.long	r_from, base, store
	.long	lit, 0
	EXIT_4TH


!!! XXX: TODO: Vocabularies

VARIABLE("current", current)
	.long	. + 4
	!! this will be part of a vocabulary
	.long .LATEST

WORD("latest", latest)
	.long	current, fetch, fetch
	EXIT_4TH


!!! CREATE - CORE 6.1.1000
WORD("create", create)
	.long	parse_word, question_dup ! 0 only on EOF
	.long	zero_equals, OR_ELSE(0f)
	.long	    SLITERAL("name missing"), type, cr
	.long	    drop, exit_4th
0:
	.long	align, here, to_r	! save NFA of the new word

	!! Name Field
	.long	dup, to_r		! save length
	.long	dup, c_comma		! write name length byte
	.long	here, swap		! destination
	.long	cmove			! copy name
	.long	r_from, allot, align	! move heap pointer past the name

	!! Link Field
	.long	current, fetch, dup, fetch, comma ! pointer to previous
	.long	r_from, swap, store		  ! update current to our NFA

	!! Code Field
	!!
	!! -- by default a word does nothing, so:
	!!
	!! 	!! forth version: ... DOES> ;
	!!  DOES_4TH(does_nothing)
	!! 	!! -- nop; leave PFA pushed by next_code on the stack
	!! 	EXIT_4TH
	!!
	!! 	!! asm version is basically inlined "nop"; as you can
	!! 	!! see, we can remove the indirection and instead use
	!! 	!! "next_code" directly.
	!!     DOES_ASM(does_nothing)
	!! 	jmp	@NEXT
	!! 	 nop
	.long	lit, next_code, comma
	EXIT_4TH


!!! SMUDGE - hide the latest word from search
WORD("smudge", smudge)
	.long	latest, c_fetch, lit, SFLAG, or, latest, c_store
	EXIT_4TH


!!! UNSMUDGE - reverse the latest word to search
WORD("unsmudge", unsmudge)
	.long	latest, c_fetch, lit, ~SFLAG, and, latest, c_store
	EXIT_4TH


!!! IMMEDIATE - CORE .6.1.1710
WORD("immediate", immediate)
	.long	latest, c_fetch, lit, IFLAG, or, latest, c_store
	EXIT_4TH


!!! VARIABLE - CORE 6.1.2410
WORD("variable", variable)
	.long	create, lit, 1, cells, allot
	EXIT_4TH


!!! CONSTANT - CORE 6.1.0950
WORD("constant", constant)
	.long	create, comma
	.long	semicolon_code_parens
	!! code fields of CONSTANT definitions point here
#if 0	/* ... DOES> @ ; */
DOES_4TH(constant_does)
	.long	fetch
	EXIT_4TH
#else	/* just inlined @ */
DOES_ASM(constant_does)
	jmp	@NEXT
	 mov.l	@TOS, TOS
#endif


!!! Traditional Forth's FIND takes counted string from traditional
!!! WORD, but that requires temporary space to hold the counted
!!! string, so ANS Forth suggests using PARSE instead.  The
!!! corresponding word to search the <c-addr, u> string is
!!! SEARCH-WORDLIST.

!!! SEARCH-CURRENT is interim chimera until proper vocabularies are
!!! provided.  For now we only have single wordlist, so.
!!! 
!!!     : search-current get-current search-wordlist ;
!!! 
WORD("search-current", search_current)
	.long	latest
1:	!! begin
	.long	dup, c_fetch, lit, (SFLAG), and, zero_equals ! not smudged?
	.long	OR_ELSE(8f)

	.long	to_r			! stash current NFA
	.long	two_dup			! the word we search for
	.long	r_fetch			! current NFA
	.long	name_count		! count, drop the flags
	.long	compare			! zero if equals
	.long	OR_ELSE(2f)

	!! not the word we are looking for
	.long	    r_from		! get back current NFA
	.long	branch, 9f

2:	!! we've got a match
	.long	    two_drop			! the word we searched for
	.long	    r_from			! get back current NFA
	.long	    dup, name_from, swap	! CFA NFA --
	.long	    c_fetch, lit, IFLAG, and	! nfa -> 1 for immediate,
	.long	    lit, 6, rshift, lit, 1, minus ! ... -1 otherwise
	.long	exit_4th

8:	!! smudged
	!!   placeholder for debugging

9:	!! try previous word in the vocabulary
	.long	n_to_link, fetch
	.long	question_dup, zero_equals, OR_ELSE(1b)

	!! we reached the end of the vocabulary
	.long	two_drop	! the word we searched for
	.long	lit, 0		! indicate failure
	EXIT_4TH


!!! ' - CORE 6.1.0070
WORD("'", tick)
	.long	parse_word, question_dup
	.long	zero_equals, OR_ELSE(0f)
	.long	    SLITERAL("name missing"), type, cr
	.long	    drop, exit_4th
0:
	.long	two_dup, search_current, question_dup
	.long	zero_equals, OR_ELSE(0f)
	.long	    SLITERAL("unknown word "), type, type, cr
	.long	    exit_4th
0:
	.long	drop			! immediate flag
	.long	minus_rot, two_drop	! a copy of name
	EXIT_4TH


!!! ['] - CORE 6.1.2510
IMMWORD("[']", tick_brackets)
	.long	question_comp
	.long	tick, literal
	EXIT_4TH


!!! STATE - CORE 6.1.2250
VARIABLE("state", state)
	.long	0		! interpretation


!!! ?COMP
WORD("?comp", question_comp)
	.long	state, fetch, zero_equals, OR_THROW(14)
	EXIT_4TH


!!! [ - CORE 6.1.2500
IMMWORD("[", left_bracket)
	.long	lit, 0, state, store
	EXIT_4TH


!!! ] - CORE 6.1.2540
WORD("]", right_bracket)
	.long	lit, -1, state, store
	EXIT_4TH


!!! COMPILE, - CORE EXT 6.2.0945
!!!     since this is a naive indirect threaded code implementation we
!!!     can compile the xt/address directly
WORD("compile,", compile_comma)
	.long	comma
	EXIT_4TH


WORD("compile", compile)
	.long	r_from, dup, cell_plus, to_r
	.long	fetch, comma
	EXIT_4TH


!!! LITERAL - CORE 6.1.1780
IMMWORD("literal", literal)
	.long	state, fetch, OR_ELSE(9f)
	.long	    compile, lit, comma
9:	EXIT_4TH


!!! 2LITERAL - DOUBLE 8.6.1.0390
IMMWORD("2literal", two_literal)
	.long	state, fetch, OR_ELSE(9f)
	.long	    compile, two_lit, comma, comma
9:	EXIT_4TH


!!! interpret source until it's empty
WORD("interpret", interpret)
1:	!! begin
	.long	parse_word, question_dup ! 0 only on EOF
	.long	OR_ELSE(9f)

	.long	two_dup, search_current, question_dup, zero_equals
	.long	OR_ELSE(.L_word_found)

	!! word not found, may be a number?
	.long	two_dup, is_number, question_dup, zero_equals
	.long	OR_ELSE(.L_number_found)

	!! not a number either, complain and punt
	.long	SLITERAL("unknown word "), type, type, cr
	.long	left_bracket	! make sure we are interpreting
	.long	exit_4th	! abort current line

.L_number_found:
	.long	lit, 2, equals, OR_ELSE(2f)

		!! double cell number
	.long	    two_swap, two_drop
	.long	    two_literal
0:	.long	    branch, 8f

2:		!! single cell number
	.long	    minus_rot, two_drop
	.long	    literal
0:	.long	    branch, 8f

.L_word_found:
	.long	two_swap, two_drop

	.long	lit, 1, plus, OR_ELSE(3f)

		!! immediate
	.long	    execute
	.long	    branch, 8f

3:		!! not immediate
	.long	    state, fetch, OR_ELSE(0f)
	.long		compile_comma
	.long		branch, 8f

0:	.long		execute

8:
	.long	branch, 1b
	!! again
9:
	.long	drop	 ! leftover caddr from unsuccessful parse-word
	EXIT_4TH


!!! BYE - TOOLS EXT 15.6.2.0830
WORD("bye", bye)
	.long	bye_parens
	.long	0xdeadbeef	! unreachable sentinel


!!! QUIT - CORE 6.1.2050
!!! refill/interpret until eof - outer repl loop
!!! 
!!! XXX: ABORT - CORE 6.1.0670
!!!     FIXME: The difference between QUIT and ABORT is that QUIT does
!!!     NOT reset the parameter stack!  I misread QUIT description, so
!!!     the code below is actually ABORT.
WORD("quit", quit)
	.long	quit_parens	! reset stacks, must be the first
1:	!! begin
	.long	    refill
	.long	OR_ELSE(9f)
	!! while
	.long	    interpret
	.long	    SLITERAL(" ok"), type, cr
	!! repeat
	.long	branch, 1b
9:
	.long	bye
	EXIT_4TH


!!! (;CODE)
WORD("(;code)", semicolon_code_parens)
	.long	r_from, latest, name_from, store
	EXIT_4TH		! to the caller's caller


!!! : - CORE 6.1.0450
WORD(":", colon)
	.long	create, smudge, right_bracket
	.long	semicolon_code_parens
	!! code field of colon definitions will point here
	.local	colon_does
colon_does:
	mov.l	1f, r0
	jmp	@r0
	 nop
	.align 2
1:	.long	call_code


!!! RECURSE - 6.1.2120
IMMWORD("recurse", recurse)
	.long	latest, name_from, comma
	EXIT_4TH


!!! ; - CORE 6.1.0460
IMMWORD(";", semicolon)
	.long	compile, exit_4th	! compile exit
	.long	unsmudge		! reveal the word to search
	.long	left_bracket		! back to interpretation
	EXIT_4TH


!!! IF - CORE 6.1.1700
IMMWORD("if", if)
	.long	question_comp
	.long	compile, question_branch
	.long	here	 ! forward reference past the true branch
	.long	lit, 0, comma
	EXIT_4TH


!!! ELSE - CORE 6.1.1310
IMMWORD("else", else)
	.long	question_comp
	.long	compile, branch
	.long	here		! forward reference past the false branch
	.long	lit, 0, comma
	.long	swap		! address left by "if"
	.long	here, swap, store
	EXIT_4TH


!!! THEN - CORE 6.1.2270
IMMWORD("then", then)
	.long	question_comp
	.long	here, swap, store
	EXIT_4TH


!!! ====================================================================
/*
 * Test code is in a separate file so that random test changes don't
 * pollute commits
 */
#include "test.S"

	.data
!!! value for CURRENT
.LATEST = .LASTNFA

	.size	heap, . - heap

	.section ".bss"
.L_DATA_SPACE: /* begins HERE */
	.space	8192
