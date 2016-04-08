/* $Id$ */

#include <machine/asm.h>

#define TOS	r8		/* top of parameter stack lives in a register */
#define PSP	r9		/* parameter stack pointer */
#define RSP	r10		/* return stack pointer */
#define IP	r11		/* instruction pointer */
#define NEXT	r13		/* "next" address to avoid memory loads */

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
 * Trampoline code callable from C that starts Forth executing a test
 * entry point word.  Eventually it will start real Forth REPL.
 *
 */

	.text
ENTRY(test)
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

	!! stacks
	mov.l	.L_psp, PSP
	mov.l	.L_rsp, RSP

	mov	r4, TOS		! pass our first argument to the test word

	!! to return from forth
	mova	1f, r0
	mov.l	r0, @-RSP	! for return_to_c

	!! to enter forth
	mov.l	.L_entry, IP

ALTENTRY(test_enter)		! for gdb breakpoing only
	jmp	@NEXT
	 nop

	.align	2		! mova target
1:
	SPILL_TOS
	mov	PSP, r0		! return value from the top of forth stack

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
.L_psp:		.long	stack_bottom
.L_rsp:		.long	rstack_bottom
.L_entry: 	.long	entry_point
.L_next:	.long	next_code


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


#if 1
/* see var_does_asm for why we can do this */
#define _VAR_DOES	next_code
#elif 1
#define _VAR_DOES	var_does_asm
#else
#define _VAR_DOES	var_does_4th
#endif
#define DEFCODE_VAR(label)	  \
	/* Code Field */	  \
  NAMED_CELL(label)		; \
	.long	_VAR_DOES	; \
	/* Parameter Field */	  \
  NAMED_CELL(label/**/_var)


#if 1
#define _CONST_DOES	const_does_asm
#else
#define _CONST_DOES	const_does_4th
#endif
#define DEFCODE_CONST(label)	  \
	/* Code Field */	  \
  NAMED_CELL(label)		; \
	.long	_CONST_DOES	; \
	/* Parameter Field */	  \
  NAMED_CELL(label/**/_const)


#define EXIT_4TH		  \
	.long	exit_4th	; \
	.long	0xdeadbeef	/* sentinel */


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

#define WORD(name, label)	DEFWORD(name,    0, DEFCODE_4TH,   label)
#define IMMWORD(name, label)	DEFWORD(name, 0x80, DEFCODE_4TH,   label)
#define ASMWORD(name, label)	DEFWORD(name,    0, DEFCODE_ASM,   label)
#define CONSTANT(name, label)	DEFWORD(name,    0, DEFCODE_CONST, label)
#define VARIABLE(name, label)	DEFWORD(name,    0, DEFCODE_VAR,   label)

#define SLITERAL(str)		  \
     /* .long */ branch, 19f	; \
11:	.ascii	str		; \
12:	.align	2		; \
19:	.long	lit, 11b, lit, (12b - 11b)



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
	.long	0xdeadbeef	! dummy


/*
 * Headerless asm word to return back to C that called us.  Will not
 * be necessary in a real system.  Used only by the test entry point
 * instead of EXIT.
 */
DEFCODE_ASM(return_to_c)
	mov.l	@RSP+, r0
	jmp	@r0
	 nop


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


/*
 * "does" code of CONSTANT
 *
 * For reference here are both asm and forth versions.
 * See DEFCODE_CONST above, where you can select which one is used.
 */
	!! forth version: ... DOES> @ ;
DOES_4TH(const_does_4th)
	.long	fetch
	EXIT_4TH

	!! asm version is basically inlined "fetch"
DOES_ASM(const_does_asm)
	jmp	@NEXT
	 mov.l	@TOS, TOS


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
ASMWORD("rot", minus_rot)
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

/*

6.1.0400 2OVER
6.1.0430 2SWAP

*/

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


!!! (do) - auxiliary for "do"
!!!     ( limit first -- R: addr limit current )
WORD("(do)", do_parens)
	.long	r_from, dup	! next cell after compiled "(do)"
	.long	fetch, to_r 	! is end of loop address - stash it for "leave"
	.long	rot, to_r       ! limit
	.long	swap, to_r	! current = first
	.long	cell_plus, to_r	! skip literal after (do)
	EXIT_4TH

!!! (?do) - auxiliary for "?do";
!!!     like "do", but does nothing if first == limit
!!!     ( limit first -- R: addr limit current )
WORD("(?do)", question_do_parens)
	.long	two_dup, equals, question_branch, 1f
	.long	two_drop
	.long	r_from, fetch, to_r, exit_4th
1:
	.long	r_from, dup	! next cell after compiled "(do)"
	.long	fetch, to_r 	! is end of loop address - stash it for "leave"
	.long	rot, to_r       ! limit
	.long	swap, to_r	! current = first
	.long	cell_plus, to_r	! skip literal after (do)
	EXIT_4TH

!!! (loop) - auxiliary for "loop" with the address of (do) compiled
!!!          as literal after it
!!! 	( R: addr limit current )
WORD("(loop)", loop_parens)
	.long	r_from			! take (loop)'s return address
	.long	r_from, one_plus, dup	! take and increment current
	.long	r_fetch			! peek at limit
	.long	u_less_than, question_branch, 1f
	.long	    to_r		  ! new current
	.long	    fetch, to_r, exit_4th ! return to (do)
1:	!! loop is done, clean up return stack
	.long	drop		      ! new current
	.long	two_r_from, two_drop  ! address for leave and limit
	.long	cell_plus, to_r	      ! skip literal (do) address
	EXIT_4TH

!!! I - CORE 6.1.1680
WORD("i", i_4th)
	.long	two_r_fetch	! current and i'th return address
	.long	drop
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
	mov	TOS, r1
	bt/s	.L_divide_by_zero
	 FILL_TOS

	mov	#0, r2
	div0s	r2, TOS
	subc	r3, r3
	subc	r2, TOS
	div0s	r1, r3
	.rept /* DIVSTEP */ 32 /* times { */
	rotcl	TOS
	div1	r1, r3
	.endr /* } */
	rotcl	TOS
	jmp	@NEXT
	 addc	r2, TOS

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
	

! "lshift" - CORE 6.1.1805
! "rshift" - CORE 6.1.2162
! "max" - CORE 6.1.1870
! "min" - CORE 6.1.1880


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
	.long 0x10


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
	.long	dup, zero_greater, question_branch, 9f
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
	.long	zero_less, question_branch, 1f
	.long	    lit, 45 /* '-' */, hold
1:	EXIT_4TH

!!! # - CORE 6.1.0030
!!!     XXX: FIXME: single cell!
WORD("#", hash)
	.long	base, fetch, slash_mod, swap
	.long	dup, lit, 9, greater_than, question_branch, 1f
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
	.long	question_branch, 1b
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
	.long	dup, question_branch, 9f
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
	.long	question_branch, 9f	! exit if >in greater/equal #tib
	.long	dup, to_in, fetch, plus, c_fetch
	.long	bl, equals
	.long	question_branch, 9f	! exit if not a space
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
	.long	question_branch, 1f

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
	.long	equals, question_branch, 7f	   ! .. is a delim?
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


/*
 * >NUMBER and related words
 */

!!! ( base - '0' upper ) range of (arabic) digits; for "within"
WORD("base-0", base_zero)
	.long	dup, lit, 10, less_than_equals
	.long	question_branch, 1f
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
	.long	question_branch, 1f
	.long	drop, r_from, lit, '0, minus, true
	.long	exit_4th
1:
	!! does base use alphabet digits?
	.long	dup, lit, 10, greater_than
	.long	question_branch, 9f

	!! a digit from 'a'.. range?
	.long	dup, base_a
	.long	r_from, lit, 0x20, or, dup, to_r ! downcased
	.long	minus_rot, within
	.long	question_branch, 9f
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
	.long	question_branch, 2f

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
	

!!! XXX: TODO: Vocabularies

VARIABLE("current", current)
	.long	. + 4
	!! this will be part of a vocabulary
	.long .LATEST

WORD("latest", latest)
	.long	current, fetch, fetch
	EXIT_4TH

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
1:
	.long	to_r			! save current word
	.long	two_dup			! - the word we search for
	.long	r_fetch			! - get current word
	.long	name_count

#if 0
	!! debug print
	.long	two_dup, swap, dot
	.long	lit, 43, emit
	.long	space, dot, space
	.long	two_dup, type, cr
#endif
	.long	compare			! zero if equals
	.long	r_from			! get back current word
	.long	n_to_link
	.long	swap
	.long	question_branch, 2f	! break if found

	.long	fetch			! previous word nfa
	.long	dup, zero_equals, question_branch, 1b

	!! we reached the end of the wordlist
	.long	drop, two_drop, lit, 0
	.long	exit_4th		! with zero nfa already on stack

2:	!! we found the word
	.long	link_from		! get cfa
	.long	to_r, two_drop, r_from	! get rid of the search word
	.long	lit, 1
	EXIT_4TH


!!! ====================================================================
/*
 * Test code is in a separate file so that random test changes don't
 * pollute commits
 */
#include "test.S"		/* define test_word */

!!!
	.align	2
ALTENTRY(entry_point)		! call me via NEXT !!!
	.long	test_word
	.long	return_to_c
	.long	0xdeadbeef

!!! value for CURRENT
.LATEST = .LASTNFA

	.section ".bss"
.L_DATA_SPACE: /* begins HERE */
	.space	8192
