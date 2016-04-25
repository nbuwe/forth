#include <machine/asm.h>

/*
 * Should we provide just the bare minimum of asm words to test MI
 * forth implementations and simplify porting, or more asm words to
 * get a bit more speed once initial porting is done?
 */
#undef MORE_ASM_WORDS

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

	!! these will be reset by ABORT
	mov	#0, TOS
	mov	#0, PSP
	mov	#0, RSP

	!! ABORT to forth
	mov.l	.L_abort_body, IP
	mov.l	@IP+, r0		! CFA of (abort), the first word
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

	.p2align 2
.L_next:	.long	next_code
.L_abort_body:	.long	abort_4th_body


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
	.p2align 2		; \
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
	.p2align 2, 0			; \
	.local	NFA_LABEL(label)	; \
  NFA_LABEL(label):			  \
	.byte	flags | (2f-1f)		; \
1:	.ascii	name			; \
2:	.p2align 2, 0


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


#define QUOTESTR(word, str)				  \
	/* .long */ word/**/_quote_parens, (22f - 21f)	; \
21:	.ascii	str					; \
22:	.p2align 2, 0 /* force new directive with ; */	;

#define SQ(str)		QUOTESTR(s, str)	/* s"     */
#define DOTQ(str)	QUOTESTR(dot, str)	/* ."     */
#define ABORTQ(str)	QUOTESTR(abort, str)	/* abort" */

#define THR13	"Undefined word"
#define THR14	"Interpreting a compile-only word"
#define THR16	"Attempt to use zero-length string as a name"
#define THR22	"Control structure mismatch"
#define THR32	"Invalid name argument"

/* XXX: for now, implement in terms of abort" */
#define THROW(code) \
	/* .long */ lit, (code), ABORTQ(THR/**/code)

#define THEN_THROW(code)			  \
	/* .long */ OR_ELSE(0f), THROW(code)	; \
0:


!!! ====================================================================
/**
 *
 *				FORTH
 *
 */

	.data			! XXX: need writable+executable

	.p2align 2
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


!!! Asm part of ABORT that resets parameter and return stacks
!!! Must be the first word called by ABORT.
ASMWORD("(abort)", abort_parens)
	mov.l	rp0_const, RSP	! reset return stack
	mov.l	sp0_const, PSP	! reset parameter stack
	jmp	@NEXT
	 mov	#0, TOS		! clear TOS just in case


!!! Asm part of QUIT that resets return stack.
!!! Must be the first word called by QUIT.
ASMWORD("(quit)", quit_parens)
	mov.l	rp0_const, RSP	! reset return stack
	jmp	@NEXT
	 nop


!!! Where the stacks are.  (ABORT) and (QUIT) just above use these
!!! directly in assembler as literals.
CONSTANT("sp0", sp0)	.long	stack_bottom
CONSTANT("rp0", rp0)	.long	rstack_bottom


!!! Current stack pointer.
!!!     ( -- sp )
!!!
!!! XXX: Both SP@ and SP0 don't try to hide the fact that we keep TOS
!!! in a register.  This has implications in MI forth code, e.g. in .S
!!! or DEPTH.
!!!
!!! The cell just above SP0 is not part of stack - it's a place to
!!! spill invalid TOS register when we push the first value onto the
!!! stack.  The TOS is in a register, so to spill it into memory, DUP
!!! it.
ASMWORD("sp@", sp_fetch)
	mov	PSP, r0
	SPILL_TOS
	jmp	@NEXT
	 mov	r0, TOS


!!! Asm part of BYE that returns control to the C code that started
!!! the Forth system.  The stacks are not reset and can be inspected.
ASMWORD("(bye)", bye_parens)
	mov.l	.L_return, r0
	jmp	@r0
	 nop

	.p2align 2
.L_return:
	.long	.L_BYE_RETURNS_HERE


!!! nop to use as a gdb breakpoint
ASMWORD("breakpoint", breakpoint)
	jmp	@NEXT
	 nop


/**
 * Threaded code interpreter.
 *
 * Executing a word means calling it's code with it's body at the top
 * of the parameter stack.
 *
 * The real core of the interpreter is "next_code" that interprets
 * endless stream of words.  Some of those words are forth definitions
 * that begin with "call_code" in their code field and end with EXIT.
 * That diverts execution by saving/restoring IP before proceeding to
 * "next_code".
 */

!!! EXECUTE - CORE 6.1.1370
!!!     ( i*x xt -- j*x )
!!!
!!! With indirect threaded code the execution token (xt) is the
!!! address of the code field (CFA).  See also next_code.
ASMWORD("execute", execute)
	mov.l	@TOS+, r0	! fetch code address and advance TOS to PFA
	jmp	@r0		! call word's code on word's body
	 nop


!!! Code field of asm words points here.
!!! This should eventually become the DOES> part of CODE
DOES_ASM(asm_does)
	jmp	@TOS		! PFA: word's body is its asm code
	 FILL_TOS		! PFA is consumed


!!! Code field of colon definitions (i.e. forth words)  points here.
!!! This should eventually become the DOES> part of ":"
DOES_ASM(call_code)
	mov.l	IP, @-RSP	! save caller's IP
	bra	.L_next_code_no_spill
	 mov	TOS, IP		! set my IP (consume PFA from TOS)
	!! don't refill TOS here, just skip the spill in next_code
	!! and let next_code set vacant TOS to next word PFA


!!! EXIT - CORE 6.1.1380
!!!     ( -- ) ( R: nest-sys -- )
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


!!! tail-call - note that the difference from EXIT is the order of
!!! CFA := @IP and IP := @RSP+
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



/**
 * Stack manipualtion
 */

!!! DROP - CORE 6.1.1260
!!!     ( x -- )
ASMWORD("drop", drop)
	jmp	@NEXT
	 FILL_TOS


!!! DUP - CORE 6.1.1290
!!!     ( x -- x x )
ASMWORD("dup", dup)
	jmp	@NEXT
	 SPILL_TOS


#ifdef MORE_ASM_WORDS
!!! ?DUP - CORE 6.1.0630
!!!     ( x -- 0 | x x )
ASMWORD("?dup", question_dup)
	tst	TOS, TOS
	bt	1f
	jmp	@NEXT
	 SPILL_TOS		! dup if non-zero
1:	jmp	@NEXT
	 nop			! do nothing if zero
#endif


!!! SWAP - CORE 6.1.2260
!!!     ( x1 x2 -- x2 x1 )
ASMWORD("swap", swap)
	mov.l	@PSP, r0
	mov.l	TOS, @PSP
	jmp 	@NEXT
	 mov	r0, TOS


!!! OVER - CORE 6.1.1990
!!!     ( x1 x2 -- x1 x2 x1 )
ASMWORD("over", over)
	mov.l	@PSP, r0
	SPILL_TOS
	jmp 	@NEXT
	 mov	r0, TOS


#ifdef MORE_ASM_WORDS
!!! NIP - CORE EXT 6.2.1930
!!!     ( x1 x2 -- x2 )
ASMWORD("nip", nip)
	jmp	@NEXT
	 add	#4, PSP
#endif


#ifdef MORE_ASM_WORDS
!!! TUCK - CORE EXT 6.2.2300
!!!     ( x1 x2 -- x2 x1 x2 )
ASMWORD("tuck", tuck)
	mov.l	@PSP+, r0
	SPILL_TOS
	jmp	@NEXT
	 mov.l	r0, @-PSP
#endif


#ifdef MORE_ASM_WORDS
!!! ROT - CORE 6.1.2160
!!!     ( x1 x2 x3 -- x2 x3 x1 )
ASMWORD("rot", rot)
	mov.l	@PSP+, r0
	mov.l	@PSP+, r1
	mov.l	r0, @-PSP
	mov.l	TOS, @-PSP
	jmp	@NEXT
	 mov	r1, TOS
#endif


#ifdef MORE_ASM_WORDS
!!! -ROT - non-standard, inverse of ROT
!!!     ( x1 x2 x3 -- x3 x1 x2 )
ASMWORD("-rot", minus_rot)
	mov.l	@PSP+, r1
	mov.l	@PSP+, r2
	mov.l	TOS, @-PSP
	mov.l	r2, @-PSP
	jmp	@NEXT
	 mov	r1, TOS
#endif


!!! PICK - CORE EXT 6.2.2030
!!!     ( xu ... x1 x0 u -- xu ... x1 x0 xu )
ASMWORD("pick", pick)
	mov	PSP, r0
	shll2	TOS
	jmp	@NEXT
	 mov.l	@(r0, TOS), TOS


!!! ROLL - CORE EXT 6.2.2150
!!!     ( xu xu-1 ... x0 u -- xu-1 ... x0 xu )
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


#ifdef MORE_ASM_WORDS
!!! 2DROP - CORE 6.1.0370
!!!     ( x1 x2 -- )
ASMWORD("2drop", two_drop)
	add	#4, PSP
	jmp	@NEXT
	 FILL_TOS
#endif


#ifdef MORE_ASM_WORDS
!!! 2DUP - CORE 6.1.0380
!!!     ( x1 x2 -- x1 x2 x1 x2 )
ASMWORD("2dup", two_dup)
	mov.l	@PSP, r0
	SPILL_TOS
	jmp	@NEXT
	 mov.l	r0, @-PSP
#endif


#ifdef MORE_ASM_WORDS
!!! 2SWAP - CORE 6.1.0430
!!!     ( x1 x2 x3 x4 -- x3 x4 x1 x2 )
ASMWORD("2swap", two_swap)
	mov.l	@PSP, r1
	mov.l	@(4, PSP), r2
	mov.l	@(8, PSP), r3
	mov.l	r1, @(8, PSP)
	mov.l	TOS, @(4, PSP)
	mov.l	r3, @PSP
	jmp	@NEXT
	 mov	r2, TOS
#endif


#ifdef MORE_ASM_WORDS
!!! 2OVER - CORE 6.1.0400
!!!     ( x1 x2 x3 x4 -- x1 x2 x3 x4 x1 x2 )
ASMWORD("2over", two_over)
	mov.l	@(4, PSP), r0
	mov.l	@(8, PSP), r1
	SPILL_TOS
	mov.l	r1, @-PSP
	jmp	@NEXT
	 mov	r0, TOS
#endif


#ifdef MORE_ASM_WORDS
!!! DEPTH - CORE 6.1.1200
!!!     ( -- +n )
ASMWORD("depth", depth)
	mov	PSP, r1
	SPILL_TOS
	mov.l	.L_depth_sp0, TOS
	sub	r1, TOS
	jmp	@NEXT
	 shlr2	TOS
	.p2align 2
.L_depth_sp0:	.long	stack_bottom
#endif


!!! >R - CORE 6.1.0580
!!!     ( x -- ) ( R: -- x )
ASMWORD(">r", to_r)
	mov.l	TOS, @-RSP
	jmp	@NEXT
	 FILL_TOS


!!! R> - CORE 6.1.2060
!!!     ( -- x ) ( R: x -- )
ASMWORD("r>", r_from)
	SPILL_TOS
	jmp	@NEXT
	 mov.l	@RSP+, TOS


!!! R@ - CORE 6.1.2070
!!!     ( -- x ) ( R: x -- x )
ASMWORD("r@", r_fetch)
	SPILL_TOS
	jmp	@NEXT
	 mov.l	@RSP, TOS


!!! 2>R CORE EXT 6.2.0340
!!!     ( -- x1 x2 ) ( R: x1 x2 -- )
ASMWORD("2>r", two_to_r)
	mov.l	@PSP+, r0
	mov.l	r0, @-RSP
	mov.l	TOS, @-RSP
	jmp	@NEXT
	 FILL_TOS


!!! 2R> - CORE EXT 6.2.0410
!!!     ( -- x1 x2 ) ( R: x1 x2 -- )
ASMWORD("2r>", two_r_from)
	SPILL_TOS
	mov.l	@RSP+, TOS
	mov.l	@RSP+, r0
	jmp	@NEXT
	 mov.l	r0, @-PSP


!!! 2R@ - CORE EXT 6.2.0415
!!!     ( -- x1 x2 ) ( R: x1 x2 -- x1 x2 )
ASMWORD("2r@", two_r_fetch)
	SPILL_TOS
	mov.l	@RSP, TOS
	mov.l	@(4, RSP), r0
	jmp	@NEXT
	 mov.l	r0, @-PSP



/**
 * Comparison
 */

!!! 0= - CORE 6.1.0270
!!!     ( x -- flag )
ASMWORD("0=", zero_equals)
	tst	TOS, TOS
	movt	TOS
	jmp	@NEXT
	 neg	TOS, TOS


#ifdef MORE_ASM_WORDS
!!! 0<> - CORE-EXT 6.2.0260
!!!     ( x -- flag )
ASMWORD("0<>", zero_not_equals)
	tst	TOS, TOS
	movt	TOS
	neg	TOS, TOS
	jmp	@NEXT
	 not	TOS, TOS
#endif


!!! 0> - CORE-EXT 6.2.0280
!!!     ( x -- flag )
ASMWORD("0>", zero_greater)
	cmp/pl	TOS
	movt	TOS
	jmp	@NEXT
	 neg	TOS, TOS


!!! 0< - CORE 6.1.0250
!!!     ( x -- flag )
ASMWORD("0<", zero_less)
	mov	#0, r0
	cmp/gt	TOS, r0
	movt	TOS
	jmp	@NEXT
	 neg	TOS, TOS


!!! = - CORE 6.1.0530
!!!     ( x1 x2 -- flag )
ASMWORD("=", equals)
	mov.l	@PSP+, r0
	cmp/eq	TOS, r0
	movt	TOS
	jmp	@NEXT
	 neg	TOS, TOS


!!! <> - CORE-EXT 6.2.0500
!!!     ( x1 x2 -- flag )
ASMWORD("<>", not_equals)
	mov.l	@PSP+, r0
	cmp/eq	TOS, r0
	movt	TOS
	neg	TOS, TOS
	jmp	@NEXT
	 not	TOS, TOS


!!! < - CORE 6.1.0480
!!!     ( n1 n2 -- flag )
ASMWORD("<", less_than)
	mov.l	@PSP+, r0
	cmp/gt	r0, TOS
	movt	TOS
	jmp	@NEXT
	 neg	TOS, TOS


!!! > - CORE 6.1.0540
!!!     ( n1 n2 -- flag )
ASMWORD(">", greater_than)
	mov.l	@PSP+, r0
	cmp/gt	TOS, r0
	movt	TOS
	jmp	@NEXT
	 neg	TOS, TOS


!!! U< - CORE 6.1.2340
!!!     ( u1 u2 -- flag )
ASMWORD("u<", u_less_than)
	mov.l	@PSP+, r0
	cmp/hi	r0, TOS
	movt	TOS
	jmp	@NEXT
	 neg	TOS, TOS


!!! U> - CORE-EXT 6.2.2350
!!!     ( u1 u2 -- flag )
ASMWORD("u>", u_greater_than)
	mov.l	@PSP+, r0
	cmp/hi	TOS, r0
	movt	TOS
	jmp	@NEXT
	 neg	TOS, TOS



/**
 * Arithmetic and bit-wise operations
 */

!!! FALSE - CORE-EXT 6.2.1485
ASMWORD("false", false)
	SPILL_TOS
	jmp	@NEXT
	 mov	#0, TOS


!!! TRUE - CORE-EXT 6.2.2298
ASMWORD("true", true)
	SPILL_TOS
	jmp	@NEXT
	 mov	#-1, TOS


!!! ABS - CORE 6.1.0690
ASMWORD("abs", abs)
	cmp/pz	TOS
	bt	1f
	neg	TOS, TOS
1:	jmp	@NEXT
	 nop


!!! NEGATE - CORE 6.1.1910
ASMWORD("negate", negate)
	jmp	@NEXT
	 neg	TOS, TOS


!!! + - CORE 6.1.0120
ASMWORD("+", plus)
	mov.l	@PSP+, r0
	jmp	@NEXT
	 add	r0, TOS


!!! - - CORE 6.1.0160
ASMWORD("-", minus)
	mov.l	@PSP+, r0
	sub	TOS, r0
	jmp	@NEXT
	 mov	r0, TOS


!!! * - CORE 6.1.0090
ASMWORD("*", star)
	mov.l	@PSP+, r0
	mul.l	r0, TOS
	jmp	@NEXT
	 sts	macl, TOS


!!! / - CORE 6.1.0230
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


!!! MOD - CORE 6.1.1890
WORD("mod", mod)
	.long two_dup, slash, star, minus
	EXIT_4TH


!!! /MOD - CORE 6.1.0240
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


!!! 1+ - CORE 6.1.0290
ASMWORD("1+", one_plus)
	jmp	@NEXT
	 add	#1, TOS


!!! 1- - CORE 6.1.0300
ASMWORD("1-", one_minus)
	jmp	@NEXT
	 add	#-1, TOS


!!! 2* - CORE 6.1.0320
ASMWORD("2*", two_star)
	jmp	@NEXT
	 shal	TOS


!!! 2/ - CORE 6.1.0330
ASMWORD("2/", two_slash)
	jmp	@NEXT
	 shar	TOS


!!! INVERT - CORE 6.1.1720
ASMWORD("invert", invert)
	jmp	@NEXT
	 not	TOS, TOS


!!! AND - CORE 6.1.0720
ASMWORD("and", and)
	mov.l	@PSP+, r0
	jmp	@NEXT
	 and	r0, TOS


!!! OR - CORE 6.1.1980
ASMWORD("or", or)
	mov.l	@PSP+, r0
	jmp	@NEXT
	 or	r0, TOS


!!! XOR - CORE 6.1.2490
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


!!! @ - CORE 6.1.0650
ASMWORD("@", fetch)
	jmp	@NEXT
	 mov.l	@TOS, TOS


!!! ! - CORE 6.1.0010
ASMWORD("!", store)
	mov.l	@PSP+, r0	! value
	mov.l	r0, @TOS	! store value
	jmp	@NEXT
	 FILL_TOS


!!! +! - CORE 6.1.0130
ASMWORD("+!", plus_store)
	mov.l	@PSP+, r0	! value increment
	mov.l	@TOS, r1	! fetch current value
	add	r1, r0
	mov.l	r0, @TOS	! store new value
	jmp	@NEXT
	 FILL_TOS


!!! 1+! - nonstandard
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


!!! LIT - auxiliary for LITERAL (CORE 6.1.1780)
ASMWORD("lit", lit)
	SPILL_TOS
	jmp	@NEXT
	 mov.l	@IP+, TOS


!!! BRANCH - auxiliary for ELSE &co
ASMWORD("branch", branch)
	jmp	@NEXT
	 mov.l	@IP+, IP


!!! ?BRANCH - auxiliary for IF &co
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


!!! (DO) - auxiliary for DO
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


!!! (?DO) - auxiliary for ?DO
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
#if 0
WORD("i", i_4th)
	.long	r_from		! return address
	.long	two_r_fetch	! limit and current
	.long	swap, minus	! normalize current - see (do)
	.long	swap, to_r	! restore return address
	EXIT_4TH
#else
ASMWORD("i", i_4th)
	SPILL_TOS
	mov.l	@RSP, TOS
	mov.l	@(4, RSP), r0
	jmp	@NEXT
	 sub	r0, TOS
#endif


!!! J - 6.1.1730
#if 0
WORD("j", j_4th)
	.long	two_r_from, two_r_from ! return address and inner loop params
	.long	two_r_fetch	       ! limit and current of the outer loop
	.long	swap, minus	       ! normalize current - see (do)
	.long	minus_rot, two_to_r
	.long	minus_rot, two_to_r
	EXIT_4TH
#else
ASMWORD("j", j_4th)
	SPILL_TOS
	mov.l	@(12, RSP), TOS
	mov.l	@(16, RSP), r0
	jmp	@NEXT
	 sub	r0, TOS
#endif


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


!!! 2LIT - auxiliary for "2LITERAL" DOUBLE 8.6.1.0390
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


!!! S>D - CORE 6.1.2170
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
	.p2align 2
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


!!! FILL - CORE 6.1.1540
!!!     ( c-addr u char -- )
ASMWORD("fill", fill)
	!! cheat, use memset(3)
	mov.l	1f, r0
	mov.l	@(4, PSP), r4	! dst
	mov	TOS, r5		! char
	mov.l	@PSP, r6	! len
	add	#8, PSP
	jsr	@r0
	 FILL_TOS
	jmp	@NEXT
	 nop
	.p2align 2
1:	.long	memset


!!! ERASE - CORE EXT 6.2.1350
!!!     ( c-addr u -- )
WORD("erase", erase)
	.long	dup, zero_equals, OR_ELSE(1f)
	.long	    two_drop, exit_4th
1:	
	.long	    lit, 0, fill
	EXIT_4TH


!!! BLANK - STRING 17.6.1.0780
!!!     ( c-addr u -- )
WORD("blank", blank)
	.long	dup, zero_equals, OR_ELSE(1f)
	.long	    two_drop, exit_4th
1:	
	.long	    bl, fill
	EXIT_4TH


!!! MOVE - CORE 6.1.1900
!!!     ( src dst len -- )
ASMWORD("move", move)
	!! cheat, use memmove(3)
	mov.l	1f, r0
	mov.l	@PSP, r4	! dst
	mov.l	@(4, PSP), r5	! src
	mov	TOS, r6		! len
	add	#8, PSP
	jsr	@r0
	 FILL_TOS
	jmp	@NEXT
	 nop
	.p2align 2
1:	.long	memmove


/* Machine-independent words in threaded code. */
#include "forth.S"

/* Random test code. */
#include "test.S"

	.data
!!! value for CURRENT
.LATEST = .LASTNFA

	.size	heap, . - heap

	.section ".bss"
.L_DATA_SPACE: /* begins HERE */
	.space	8192
