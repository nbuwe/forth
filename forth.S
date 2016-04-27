/**
 * Machine-independent words in threaded code.
 */


/**
 * Stack manipualtion
 */

.ifnotdef question_dup
!!! ?DUP - CORE 6.1.0630
!!!     ( x -- 0 | x x )
WORD("?dup", question_dup)
	.long	dup, zero_not_equals, OR_ELSE(9f)
	.long	    dup
9:	EXIT_4TH
.endif


.ifnotdef nip
!!! NIP - CORE EXT 6.2.1930
!!!     ( x1 x2 -- x2 )
WORD("nip", nip)
	.long	swap, drop
	EXIT_4TH
.endif


.ifnotdef tuck
!!! TUCK - CORE EXT 6.2.2300
!!!     ( x1 x2 -- x2 x1 x2 )
WORD("tuck", tuck)
	.long	swap, over
	EXIT_4TH
.endif


.ifnotdef rot
!!! ROT - CORE 6.1.2160
!!!     ( x1 x2 x3 -- x2 x3 x1 )
WORD("rot", rot)
	.long	lit, 2, roll
	EXIT_4TH
.endif


.ifnotdef minus_rot
!!! -ROT - non-standard, inverse of ROT
!!!     ( x1 x2 x3 -- x3 x1 x2 )
WORD("-rot", minus_rot)
	.long	rot, rot
	EXIT_4TH
.endif


.ifnotdef two_drop
!!! 2DROP - CORE 6.1.0370
!!!     ( x1 x2 -- )
WORD("2drop", two_drop)
	.long	drop, drop
	EXIT_4TH
.endif


.ifnotdef two_dup
!!! 2DUP - CORE 6.1.0380
!!!     ( x1 x2 -- x1 x2 x1 x2 )
WORD("2dup", two_dup)
	.long	over, over
	EXIT_4TH
.endif


.ifnotdef two_swap
!!! 2SWAP - CORE 6.1.0430
!!!     ( x1 x2 x3 x4 -- x3 x4 x1 x2 )
WORD("2swap", two_swap)
	.long	lit, 3, roll, lit, 3, roll
	EXIT_4TH
.endif


.ifnotdef two_over
!!! 2OVER - CORE 6.1.0400
!!!     ( x1 x2 x3 x4 -- x1 x2 x3 x4 x1 x2 )
WORD("2over", two_over)
	.long	lit, 3, pick, lit, 3, pick
	EXIT_4TH
.endif


.ifnotdef depth
!!! DEPTH - CORE 6.1.1200
!!!     ( -- +n )
WORD("depth", depth)
	.long	sp_fetch, sp0, minus
	.long	lit, 1, cells, slash
	EXIT_4TH
.endif



/**
 * Comparison
 */

!!! NOT - non standard (see discussion in A.6.1.1720 INVERT)
!!! Negate the sense of a flag, not necessarily a well-formed flag.
!!!     ( x -- flag)
WORD("not", not)
	.long	zero_equals
	EXIT_4TH


.ifnotdef zero_not_equals
!!! 0<> - CORE-EXT 6.2.0260
!!!     ( x -- flag )
WORD("0<>", zero_not_equals)
	.long	zero_equals, not
	EXIT_4TH
.endif


.ifnotdef less_than_equals
!!! <= - nonstandard
!!!     ( n1 n2 -- flag )
WORD("<=", less_than_equals)
	.long	greater_than, not
	EXIT_4TH
.endif


.ifnotdef greater_than_equals
!!! >= - nonstandard
!!!     ( n1 n2 -- flag )
WORD(">=", greater_than_equals)
	.long	less_than, not
	EXIT_4TH
.endif


.ifnotdef max
!!! MAX - CORE 6.1.1870
!!!     ( n1 n2 -- n3 )
WORD("max", max)
	.long	two_dup, greater_than, OR_ELSE(0f)
	.long	    drop, exit_4th
0:	!!	else
	.long	    nip
	EXIT_4TH
.endif


.ifnotdef min
!!! MIN - CORE 6.1.1880
!!!     ( n1 n2 -- n3 )
WORD("min", min)
	.long	two_dup, less_than, OR_ELSE(0f)
	.long	    drop, exit_4th
0:	!!	else
	.long	    nip
	EXIT_4TH
.endif


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



/**
 * Arithmetic and bit-wise operations
 */

.ifnotdef false
!!! FALSE - CORE-EXT 6.2.1485
!!!     ( -- false )
WORD("false", false)
	.long	lit, 0
	EXIT_4TH
.endif


.ifnotdef true
!!! TRUE - CORE-EXT 6.2.2298
!!!     ( -- true )
WORD("true", true)
	.long	lit, -1
	EXIT_4TH
.endif


.ifnotdef abs
!!! ABS - CORE 6.1.0690
!!!     ( n -- u )
WORD("abs", abs)
	.long	dup, zero_less, OR_ELSE(9f)
	.long	    negate
9:	EXIT_4TH
.endif


.ifnotdef one_plus
!!! 1+ - CORE 6.1.0290
!!!     ( n1|u1 -- n2|u2 )
WORD("1+", one_plus)
	.long	lit, 1, plus
	EXIT_4TH
.endif


.ifnotdef one_minus
!!! 1- - CORE 6.1.0300
!!!     ( n1|u1 -- n2|u2 )
WORD("1-", one_minus)
	.long	lit, 1, minus
	EXIT_4TH
.endif


.ifnotdef mod
!!! MOD - CORE 6.1.1890
!!!     ( n1 n2 -- n3 )
WORD("mod", mod)
	.long two_dup, slash, star, minus
	EXIT_4TH
.endif


.ifnotdef slash_mod
!!! /MOD - CORE 6.1.0240
!!!     ( n1 n2 -- n3 n4 )
WORD("/mod", slash_mod)
	.long two_dup, slash
	.long tuck, star
	.long lit, 2, roll
	.long swap, minus
	.long swap
	EXIT_4TH
.endif


.ifnotdef u_slash_mod
!!! U/MOD - nonstandard unsigned division with remainder
!!!     ( u1 u2 -- u3 u4 )
WORD("u/mod", u_slash_mod)
	.long two_dup, u_slash
	.long tuck, star
	.long lit, 2, roll
	.long swap, minus
	.long swap
	EXIT_4TH
.endif


!!! MS/REM - nonstandard
!!!     Divide 64 by 32 bit, producing 32 bit remainder and 64 bit quotient.
!!!     ( d1 n1 -- n2 d2 )
CWORD("ms/rem", ms_slash_rem)
	.long	ms_slash_rem_impl


!!! SM/REM - CORE 6.1.2214
!!!     ( d1 n1 -- n2 n3 )
WORD("sm/rem", sm_slash_rem)
	.long	ms_slash_rem	! (d1 n1 -- n2 d2; 64 bit quotient)
	.long	drop		! truncate quotient to 32 bit
	EXIT_4TH


!!! MU/MOD - nonstandard
!!!     Divide 64 by 32 bit, producing 32 bit remainder and 64 bit quotient.
!!!     ( ud1 u1 -- u2 ud2 )
CWORD("mu/mod", mu_slash_mod)
	.long	mu_slash_mod_impl


!!! UM/MOD - CORE 6.1.2370
!!!     ( ud u1 -- u2 u3 )
WORD("um/mod", um_slash_mod)
	.long	mu_slash_mod	! (ud1 u1 -- u2 ud2; 64 bit quotient)
	.long	drop		! truncate quotient to 32 bit
	EXIT_4TH


/**
 * Address alignment
 * XXX: This is 32-bit specific for now.
 */

.ifnotdef aligned
!!! ALIGNED - CORE 6.1.0706
!!!     ( addr -- a-addr )
WORD("aligned", aligned)
	.long	lit, 3, plus, lit, -4, and
	EXIT_4TH
.endif


.ifnotdef cell_plus
!!! CELL+ - CORE 6.1.0880
!!!     ( a-addr1 -- a-addr2 )
WORD("cell+", cell_plus)
	.long	lit, 4, plus
	EXIT_4TH
.endif


.ifnotdef cell_minus
!!! CELL- - nonstandard
!!!     ( a-addr1 -- a-addr2 )
WORD("cell-", cell_minus)
	.long	lit, 4, minus
	EXIT_4TH
.endif


.ifnotdef cells
!!! CELLS - CORE 6.1.0890
!!!     ( n1 -- n2 )
WORD("cells", cells)
	.long	two_star, two_star
	EXIT_4TH
.endif


.ifnotdef char_plus
!!! CHAR+ - CORE 6.1.0897
!!!     ( c-addr1 -- c-addr2 )
WORD("char+", char_plus)
	.long	one_plus
	EXIT_4TH
.endif


.ifnotdef chars
!!! CHARS - CORE 6.1.0898
!!!     ( n1 -- n2 )
WORD("chars", chars)
	!! nop
	EXIT_4TH
.endif



/**
 * Fetch and store
 */

.ifnotdef plus_store
!!! +! - CORE 6.1.0130
!!!     ( n|u a-addr -- )
WORD("+!", plus_store)
	.long	dup, fetch
	.long	rot, plus
	.long	swap, store
	EXIT_4TH
.endif


.ifnotdef one_plus_store
!!! 1+! - nonstandard
!!!     ( a-addr -- )
WORD("1+!", one_plus_store)
	.long	dup, fetch
	.long	one_plus
	.long	swap, store
	EXIT_4TH
.endif


.ifnotdef two_fetch
!!! 2@ - CORE 6.1.0350
!!!     ( a-addr -- x1 x2 )
WORD("2@", two_fetch)
	.long	dup, cell_plus, fetch
	.long	swap, fetch
	EXIT_4TH
.endif


.ifnotdef two_store
!!! 2! - CORE 6.1.0310
!!!     ( x1 x2 a-addr -- )
WORD("2!", two_store)
	.long	dup, to_r
	.long	store
	.long	r_from, cell_plus
	.long	store
	EXIT_4TH
.endif



/**
 * Strings
 */

.ifnotdef count
!!! COUNT - CORE 6.1.0980
!!!     ( c-addr1 -- c-addr2 u )
WORD("count", count)
	.long	dup, c_fetch
	.long	swap, one_plus
	.long	swap
	EXIT_4TH
.endif


!!! ERASE - CORE EXT 6.2.1350
!!!     ( c-addr u -- )
WORD("erase", erase)
	.long	dup, zero_equals, OR_ELSE(0f)
	.long	    two_drop, exit_4th
0:
	.long	    lit, 0, fill
	EXIT_4TH


!!! BLANK - STRING 17.6.1.0780
!!!     ( c-addr u -- )
WORD("blank", blank)
	.long	dup, zero_equals, OR_ELSE(0f)
	.long	    two_drop, exit_4th
0:
	.long	    bl, fill
	EXIT_4TH



/**
 * Data-space words
 */

VARIABLE("dp", dp)
	.long	.L_DATA_SPACE

!!! HERE - CORE 6.1.1650
!!!   ( -- addr )
WORD("here", here)
	.long	dp, fetch
	EXIT_4TH

WORD("dp!", dp_store)
	.long	dp, store
	EXIT_4TH

!!! ALIGN - CORE 6.1.0705
!!!     ( -- )
WORD("align", align)
	.long	here, aligned, dp_store
	EXIT_4TH

!!! ALLOT - CORE 6.1.0710
!!!     ( n -- )
WORD("allot", allot)
	.long	here, plus, dp_store
	EXIT_4TH


!!! , - CORE 6.1.0150
!!!     ( x -- )
WORD(",", comma)
	.long	here, lit, 1, cells, allot, store
	EXIT_4TH


!!! C, - CORE 6.1.0860
!!!     ( char -- )
WORD("c,", c_comma)
	.long	here, lit, 1, chars, allot, c_store
	EXIT_4TH


!!! PAD - CORE EXT 6.2.2000
!!!     As far as I understand user can use memory below pad, while
!!!     the system uses the area between DP and PAD as hold space.
!!!     ( -- c-addr )
WORD("pad", pad)
	.long	here, lit, 128, plus
	EXIT_4TH



!!! ====================================================================
!!! I/O - Common

!!! BASE - CORE 6.1.0750
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

!!! BL - CORE 6.1.0770
CONSTANT("bl", bl) .long 0x20


!!! EMIT - CORE 6.1.1320
ASMWORD("emit", emit)
	mov.l	1f, r0
	mov	TOS, r4
	jsr	@r0
	 FILL_TOS
	jmp	@NEXT
	 nop
	.p2align 2
1:	.long	_C_LABEL(emit_impl)


!!! CR - CORE 6.1.0990
WORD("cr", cr)
	.long	lit, '\n, emit
	EXIT_4TH


!!! SPACE - CORE 6.1.2220
WORD("space", space)
	.long	bl, emit
	EXIT_4TH


!!! SPACES - CORE 6.1.2230
WORD("spaces", spaces)
	.long	dup, zero_greater, zero_equals, OR_ELSE(0f)
	.long	    drop, exit_4th
0:
	.long	lit, 0, do_parens, 9f
1:	.long	    space
	.long	loop_parens, 1b
9:	EXIT_4TH


!!! TYPE - CORE 6.1.2310
ASMWORD("type", type)
	mov.l	1f, r0
	mov	TOS, r5
	mov.l	@PSP+, r4
	jsr	@r0
	 FILL_TOS
	jmp	@NEXT
	 nop
	.p2align 2
1:	.long	_C_LABEL(type_impl)


!!! Beginning of the pictured numeric output string.
VARIABLE("hld", hld)
	.long	0


!!! <# - CORE 6.1.0490
!!!     Initialize the pictured numeric output conversion process.
!!!     ( -- )
WORD("<#", less_hash)
	.long	pad, hld, store
	EXIT_4TH


!!! HOLD - CORE 6.1.1670
!!!     Add char to the beginning of the pictured numeric output string.
!!!     ( char -- )
WORD("hold", hold)
	.long	lit, -1, hld, plus_store
	.long	hld, fetch, c_store
	EXIT_4TH


!!! #> - CORE 6.1.0040
!!!     Make the pictured numeric output string available as a
!!!     character string.
!!!     ( xd -- c-addr u )
WORD("#>", hash_greater)
	.long	two_drop
	.long	hld, fetch, pad, over, minus
	EXIT_4TH


!!! SIGN - CORE 6.1.2210
!!!     ( n -- )
WORD("sign", sign)
	.long	zero_less, OR_ELSE(1f)
	.long	    lit, '-, hold
1:	EXIT_4TH


!!! # - CORE 6.1.0030
!!!     Convert one digit.
!!!     ( ud1 -- ud2 )
WORD("#", hash)
	.long	base, fetch, mu_slash_mod
	.long	rot	! remainder
	.long	dup, lit, 9, greater_than, OR_ELSE(1f)
	.long		lit, 'a - 10
	.long		branch, 2f
1:		/* else */
	.long		lit, '0
2:		/* then */
	.long	plus, hold
	EXIT_4TH


!!! #S - CORE 6.1.0050
!!!     Use # to convert all digits, leaves double-cell zero on the stack.
!!!     ( ud -- 0. )
WORD("#s", hash_s)
0:	!! begin
	.long	hash
	!! until
	.long	two_dup
	.long	or, zero_equals	! XXX: TODO: D0=
	.long	OR_ELSE(0b)
	EXIT_4TH


!!! Convert N to string in the transient hold space.
!!!     ( n -- )
WORD("(.)", dot_parens)
	.long	dup			! keep a copy for SIGN
	.long	abs, lit, 0		! positive, extend to double-cell
	.long	less_hash, hash_s	! convert to string in hold buffer
	.long	rot, sign		! add minus sign for negative
	.long	hash_greater		! finish conversion, return string
	EXIT_4TH


!!! . - CORE 6.1.0180
!!!     ( n -- )
WORD(".", dot)
	.long	dot_parens, type, space
	EXIT_4TH


!!! Convert U to string in the transient hold space.
!!!     ( u -- )
WORD("(u.)", u_dot_parens)
	.long	lit, 0		! unsigned extend to double-cell
	.long	less_hash, hash_s, hash_greater
	EXIT_4TH


!!! U. - CORE 6.1.2320
!!!     ( u -- )
WORD("u.", u_dot)
	.long	u_dot_parens, type, space
	EXIT_4TH


!!! .S - TOOLS 15.6.1.0220
WORD(".s", dot_s)
	.long	sp_fetch, sp0, equals, OR_ELSE(0f), exit_4th; 0:;
	!! 
	.long	dup			! spill TOS to memory
	.long	sp_fetch, cell_plus	! below spilled TOS 
	.long	sp0, cell_minus		! skip invalid TOS of an empty stack
	.long	do_parens, 9f
1:	!!
	.long	i_4th, cell_minus, fetch, dot
	.long	lit, 0, cell_minus
	.long	plus_loop_parens, 1b
9:
	.long	drop
	EXIT_4TH


!!! ====================================================================
!!! I/O - Input

!!! ACCEPT - CORE 6.1.0695
ASMWORD("accept", accept_4th)	! XXX: avoid name clash with accept(2)
	mov.l	1f, r0
	mov	TOS, r5
	mov.l	@PSP+, r4
	jsr	@r0
	 FILL_TOS
	SPILL_TOS
	jmp	@NEXT
	 mov	r0, TOS		! push +n2 returned by the C code
	.p2align 2
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


!!! Advance >IN to the first character that is not a delimiter or to
!!! the end of SOURCE.
!!!     ( delim -- )
WORD("skip-delim", skip_delim)
	.long	to_r		! stash delimiter away
	.long	source, swap
1:	!! begin 		\ buflen buf --
	.long	over, to_in, fetch, swap, less_than
	.long	OR_ELSE(9f)		! exit if >in greater/equal #tib
	.long	dup, to_in, fetch, plus, c_fetch
	.long	r_fetch, equals
	.long	OR_ELSE(9f)		! exit if not a delimiter
	.long	to_in, one_plus_store	! advance >in
	.long	branch, 1b		! again
9:
	.long	two_drop		! buflen and buf
	.long	r_from, drop		! delimiter
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


!!! Skip leading spaces and parse a space delimited word.  This is an
!!! alternative to tradtional BL WORD that doesn't require copying the
!!! result into a temporary space for counted space terminated string.
!!!     ( -- str len )
WORD("parse-word", parse_word)
	.long	bl, skip_delim, bl, parse
	EXIT_4TH


!!! WORD - CORE 6.1.2450
!!!     ( char "<chars>ccc<char>" -- c-addr )
WORD("word", word)
	.long	dup, skip_delim, parse
	!! str len --
	.long	dup, to_r		! stash away count
	.long	here, one_plus		! dst temp area, reserve count byte
	.long	swap, cmove		! copy parsed string
	.long	r_fetch, here, c_store	! write count
	.long	bl, here, one_plus, r_from, plus, c_store ! space after string
	.long	here
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


!!! ( char base -- value true | false )
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
!!!     ( ud1 s1 l1 -- ud2 s2 l2)
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


!!! str len -- 0 | n 1 | d 2
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
	.long	swap, one_plus	    ! ++address
	.long	swap, one_minus	    ! --length

	!! if there's just one char, it cannot be a minus sign
	.long	dup, lit, 1, greater_than, OR_ELSE(.L_check_number)

.L_check_sign:
	.long	over, c_fetch, lit, '-, equals
	.long	OR_ELSE(.L_check_number)

	.long	r_from, drop, true, to_r ! note that we have a minus

	.long	swap, one_plus		! ++address
	.long	swap, one_minus		! --length

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
	.long	false
	EXIT_4TH


!!! CONVERT - CORE EXT 6.2.0970
!!!     ( ud1 cstr -- ud2 caddr )
WORD("convert", convert)
	.long	char_plus	! skip the count byte of cstr
	.long	lit, 256	! provide fake length
	.long	to_number	! convert
	.long	drop		! remainder of the fake length
	EXIT_4TH



/**
 * Literals (though these are usually in asm).
 */

#if 0
.ifnotdef lit
!!! LIT - auxiliary for LITERAL (CORE 6.1.1780)
!!!   To use this forth version you must provide asm version of CELL+
!!!   to avoid recursive call to LIT from forth version of CELL+.
WORD("lit", lit)
	.long	r_from, dup, fetch
	.long	swap, cell_plus, to_r
	EXIT_4TH
.endif
#endif


.ifnotdef two_lit
!!! 2LIT - auxiliary for 2LITERAL (DOUBLE 8.6.1.0390)
WORD("2lit", two_lit)
	.long	r_from, dup, two_fetch
	.long	rot, lit, 2, cells, plus, to_r
	EXIT_4TH
.endif



/**
 * Compiled string literals:
 *
 *     (c") <cnt byte> <cnt bytes of string> [align]
 *     (s") <len cell> <len bytes of string> [align]
 *
 * and ditto for (.") and (abort")
 */

!!! Put counted string on the stack (for C" - CORE EXT 6.2.0855)
WORD("(c\")", c_quote_parens)
	.long	r_from, dup
	.long	count, plus, aligned, to_r	! skip string
	EXIT_4TH


!!! Put <c-addr, u> string on the stack (for S" - CORE 6.1.2165)
WORD("(s\")", s_quote_parens)
	.long	r_from, dup
	.long	cell_plus, swap			! address of string
	.long	fetch				! length
	.long	two_dup, plus, aligned, to_r	! skip string
	EXIT_4TH


!!! Type string (for ." - CORE 6.1.0190)
WORD("(.\")", dot_quote_parens)
	.long	r_from, dup
	.long	cell_plus, swap			! address of string
	.long	fetch				! length
	.long	two_dup, plus, aligned, to_r	! skip string
	!! the above is a copy of (s")
	.long	type
	EXIT_4TH


!!! Abort with a message (for ABORT" - CORE 6.1.0680)
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



/**
 * Compilied "word" literals.
 */

!!! COMPILE, - CORE EXT 6.2.0945
!!!     since this is a naive indirect threaded code implementation we
!!!     can compile the xt/address directly
WORD("compile,", compile_comma)
	.long	comma
	EXIT_4TH


!!! COMPILE - Forth 83
!!! 
!!! Pick the next word (xt) and compile it.  Creatively abuses the
!!! fact that for non-immediate word <w> the sequence 
!!! 
!!!     : ... compile <w> ... ;
!!! 
!!! within a colon definition will be compiled as-is and at run time
!!! COMPILE can act like LIT, except it compiles its "literal", <w>,
!!! instead of pushing it on to the stack.
WORD("compile", compile)
	.long	r_from, dup, cell_plus, to_r
	.long	fetch, compile_comma
	EXIT_4TH



/**
 * XXX: TODO: Vocabularies
 */

VARIABLE("current", current)
	.long	. + 4
	!! this will be part of a vocabulary
	.long .LATEST

WORD("latest", latest)
	.long	current, fetch, fetch
	EXIT_4TH


/**
 * Definition structure.
 *
 * A word is identified by its CFA (tick &c).  This is implicit source
 * or destination of ">" if omitted.
 */

!!! >BODY - CORE 6.1.0550
!!!     ( xt -- pfa )
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


!!! CREATE - CORE 6.1.1000
WORD("create", create)
	.long	parse_word, question_dup ! 0 only on EOF
	.long	zero_equals, OR_ELSE(0f)
	.long	    drop, THROW(16)
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
!!!     : search-current ( c-addr u -- 0 | xt 1 | xt -1 )
!!!         get-current search-wordlist ;
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
	.long	false		! indicate failure
	EXIT_4TH


!!! FIND - CORE 6.1.1550
!!!     ( c-addr -- caddr 0 | xt 1 | xt -1 )
WORD("find", find)
	.long	dup, count, search_current
	.long	dup, zero_not_equals, OR_ELSE(9f)
	.long	    rot, drop
9:	EXIT_4TH


!!! Common helper for ' and the like that does parse/search combo.
!!!     ( "<spaces>name" -- 0 | xt 1 | xt -1 )
WORD("(')", tick_parens)
	.long	parse_word, question_dup
	.long	zero_equals, OR_ELSE(0f)
	.long	    drop, THROW(16)
0:
	.long	two_dup, search_current, question_dup
	.long	zero_equals, OR_ELSE(0f)
	.long	    type, DOTQ(" - ")
	.long	    THROW(13)
0:
	.long	two_swap, two_drop
	EXIT_4TH


!!! ' - CORE 6.1.0070
!!!     ( "<spaces>name" -- xt )
WORD("'", tick)
	.long	tick_parens, drop ! immediate flag
	EXIT_4TH



/**
 * Forth interpreter.
 */

!!! STATE - CORE 6.1.2250
VARIABLE("state", state)
	.long	0		! interpretation


!!! [ - CORE 6.1.2500
IMMWORD("[", left_bracket)
	.long	false, state, store
	EXIT_4TH


!!! ] - CORE 6.1.2540
WORD("]", right_bracket)
	.long	true, state, store
	EXIT_4TH


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
	.long	type, DOTQ(" - ")
	.long	THROW(13)

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

	.long	one_plus, OR_ELSE(3f)

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
	.long	quit_parens	! reset data stack, must be the first
	.long	left_bracket	! interpret
1:	!! begin
	.long	    refill
	.long	OR_ELSE(9f)
	!! while
	.long	    interpret
	.long	    DOTQ(" ok")
	.long	    cr
	!! repeat
	.long	branch, 1b
9:
	.long	bye
	EXIT_4TH


!!! ABORT - CORE 6.1.0670
WORD("abort", abort_4th)
	.long	abort_parens	! reset return stack, must be first
	.long	goto_parens, quit
	EXIT_4TH



!!! ====================================================================
!!! 
!!! Words for interacting with the interpreter and defining new words.
!!! 


!!! ?COMP - assert that the interpreter is in compilation state
WORD("?comp", question_comp)
	.long	state, fetch, zero_equals, THEN_THROW(14)
	EXIT_4TH


/**
 * Comments
 */

!!! ( - CORE 6.1.0080
!!!     Comment until the closing paren
IMMWORD("(", paren)
	.long	lit, '), parse, two_drop
	EXIT_4TH


!!! \ - CORE EXT 6.2.2535
!!!     Comment until the end of line
IMMWORD("\\", backslash)
	.long	lit, '\n, parse, two_drop
	EXIT_4TH


/**
 * Character literals
 */

!!! CHAR - CORE 6.1.0895
!!!     ( "<spaces>name" -- char )
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


/**
 * Literals
 */

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


/**
 * String literals
 */

!!! COMPILE" - common helper for S" ." and ABORT" that takes
!!! corresponding "LIT-like" word that implements the run-time
!!! semantics and compiles it followed by the parsed string literal.
!!! 
!!!     ( xt "ccc<quote>" -- )
WORD("compile\"", compile_quote)
	.long	lit, '\", parse	! XXX: handle failure
	!! compile: <xt> <len> <len bytes of string>
	.long	rot, compile_comma	! compile run-time word
	.long	dup, comma	! string length
	.long	here, swap	! copy destination
	.long	dup, allot	! reserve space
	.long	cmove, align	! copy string
	EXIT_4TH


!!! S" - CORE 6.1.2165
IMMWORD("s\"", s_quote)
	.long	question_comp
	.long	lit, s_quote_parens, compile_quote
	EXIT_4TH


!!! ." - CORE 6.1.0190
IMMWORD(".\"", dot_quote)
	.long	question_comp
	.long	lit, dot_quote_parens, compile_quote
	EXIT_4TH


!!! ABORT" - CORE 6.1.0680
IMMWORD("abort\"", abort_quote)
	.long	question_comp
	.long	lit, abort_quote_parens, compile_quote
	EXIT_4TH


!!! C" - CORE EXT 6.2.0855
!!!     Like COMPILE" above, but with counted string.
!!! 
!!!     ( "ccc<quote" -- )
IMMWORD("c\"", c_quote)
	.long	question_comp
	.long	lit, '\", parse	! XXX: handle failure
	!! XXX: throw if string is too long
	!! compile: (c") <count> <count bytes of string>
	.long	compile, c_quote_parens
	.long	dup, c_comma	! string count
	.long	here, swap	! copy destination
	.long	dup, allot	! reserve space
	.long	cmove, align	! copy string
	EXIT_4TH


/**
 * Word literals
 */

!!! ['] - CORE 6.1.2510
IMMWORD("[']", tick_brackets)
	.long	question_comp
	.long	tick, literal
	EXIT_4TH


!!! [COMPILE] - CORE EXT 6.2.2530
IMMWORD("[compile]", compile_brackets)
	.long	question_comp
	.long	tick, compile_comma
	EXIT_4TH


!!! POSTPONE - CORE 6.1.2033
IMMWORD("postpone", postpone)
	.long	question_comp
	.long	tick_parens, one_plus, OR_ELSE(1f)
		!! immediate
	.long	    compile_comma
	.long	    branch, 9f
1:		!! normal
	.long	    compile, compile, compile_comma
9:	EXIT_4TH


/**
 * Colon definitions
 */

!!! (;CODE) - set CFA of the latest word to the asm code that directly
!!!     follows compiled (;code)
WORD("(;code)", semicolon_code_parens)
	.long	r_from, latest, name_from, store
	EXIT_4TH		! to the caller's caller


!!! (;ALIT) - set CFA of the latest word to the address compiled
!!!     directly after this word.
WORD("(;alit)", semicolon_alit_parens)
	.long	r_from, fetch, latest, name_from, store
	EXIT_4TH		! to the caller's caller


!!! : - CORE 6.1.0450
WORD(":", colon)
	.long	create, smudge, right_bracket
	.long	semicolon_alit_parens, call_code
	!! We prefer (;alit) to (;code) here since "call_code" must be
	!! right next to EXIT and its "next_code" so that pc-relative
	!! branch can reach it.  Alternatively, we can move the
	!! definition of colon up there before EXIT, but I'd like to
	!! segregate MD asm code and MI forth code as much as possible.


!!! DOES> - CORE 6.1.1250
IMMWORD("does>", does)
	.long	compile, semicolon_code_parens
	!! DOES_4TH:
	!!   01 c7	mova	.+8, r0
	!!   b6 2a	mov.l	IP, @-RSP
	!!   2b 4d	jmp	@NEXT
	!!   03 6b	 mov	r0, IP
	.long	lit, 0x2ab6c701, comma
	.long	lit, 0x6b034d2b, comma
	EXIT_4TH


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


WORD("?pairs", question_pairs)
	.long	minus, zero_not_equals, THEN_THROW(22)
	EXIT_4TH


WORD(">mark", mark_fwd)
	.long	here, lit, 0, comma
	EXIT_4TH


WORD("resolve>", resolve_fwd)
	.long	here, swap, store
	EXIT_4TH


WORD("<mark", mark_back)
	.long	here
	EXIT_4TH

WORD("<resolve", resolve_back)
	.long	comma
	EXIT_4TH


!!! IF - CORE 6.1.1700
IMMWORD("if", if)
	.long	question_comp
	.long	compile, question_branch
	.long	mark_fwd
	.long	lit, 1		! for ?pairs
	EXIT_4TH


!!! ELSE - CORE 6.1.1310
IMMWORD("else", else)
	.long	question_comp
	.long	lit, 1, question_pairs
	.long	compile, branch, mark_fwd ! will be resolved by THEN
	.long	swap, resolve_fwd	  ! address left by IF
	.long	lit, 1			  ! for ?pairs
	EXIT_4TH


!!! THEN - CORE 6.1.2270
IMMWORD("then", then)
	.long	question_comp
	.long	lit, 1, question_pairs
	.long	resolve_fwd	! MARK_FWD of IF or ELSE
	EXIT_4TH


!!! BEGIN - CORE 6.1.0760
IMMWORD("begin", begin)
	.long	question_comp
	.long	mark_back
	.long	lit, 2		! for ?pairs
	EXIT_4TH


!!! AGAIN - CORE EXT 6.2.0700
IMMWORD("again", again)
	.long	question_comp
	.long	lit, 2, question_pairs
	.long	compile, branch, resolve_back
	EXIT_4TH


!!! UNTIL - CORE 6.1.2390
IMMWORD("until", until)
	.long	question_comp
	.long	lit, 2, question_pairs
	.long	compile, question_branch, resolve_back
	EXIT_4TH


!!! WHILE - CORE 6.1.2430
!!!     ( C: dest -- orig dest )
IMMWORD("while", while)
	.long	question_comp
	.long	lit, 2, question_pairs
	.long	lit, 2		! for ?pairs
	.long	if, two_swap
	EXIT_4TH


!!! 6.1.2140 REPEAT
!!!     ( C: orig dest -- )
IMMWORD("repeat", repeat)
	.long	again, then
	EXIT_4TH


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


!!! DO - CORE 6.1.1240
IMMWORD("do", do)
	.long	question_comp
	.long	compile, do_parens
	.long	mark_fwd	! address for leave
	.long	mark_back	! address to loop back to
	.long	lit, 3		! for ?pairs
	EXIT_4TH


!!! ?DO - CORE EXT 6.2.0620
!!!     Differs from DO only in the auxiliary it compiles.
IMMWORD("question_do", question_do)
	.long	question_comp
	.long	compile, question_do_parens
	.long	mark_fwd	! address for leave
	.long	mark_back	! address to loop back to
	.long	lit, 3		! for ?pairs
	EXIT_4TH


.ifnotdef i_4th
!!! I - CORE 6.1.1680
WORD("i", i_4th)
	.long	r_from		! return address
	.long	two_r_fetch	! limit and current
	.long	swap, minus	! normalize current - see (do)
	.long	swap, to_r	! restore return address
	EXIT_4TH
.endif


.ifnotdef j_4th
!!! J - 6.1.1730
WORD("j", j_4th)
	.long	two_r_from, two_r_from ! return address and inner loop params
	.long	two_r_fetch	       ! limit and current of the outer loop
	.long	swap, minus	       ! normalize current - see (do)
	.long	minus_rot, two_to_r
	.long	minus_rot, two_to_r
	EXIT_4TH
.endif


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


!!! LOOP - CORE 6.1.1800
IMMWORD("loop", loop)
	.long	question_comp
	.long	lit, 3, question_pairs
	.long	compile, loop_parens, resolve_back
	.long	resolve_fwd
	EXIT_4TH


!!! +LOOP - CORE 6.1.0140
!!!     Differs from LOOP only in the auxiliary it compiles.
IMMWORD("+loop", plus_loop)
	.long	question_comp
	.long	lit, 3, question_pairs
	.long	compile, plus_loop_parens, resolve_back
	.long	resolve_fwd
	EXIT_4TH


/**
 * Values (read-mostly and hence auto-fetching).
 */

!!! VALUE - CORE EXT 6.2.2405
WORD("value", value)
	.long	create, immediate
	.long	comma
	.long	semicolon_code_parens
DOES_4TH(value_does)
	.long	state, fetch, OR_ELSE(1f)
	    !! compilation
	.long	    literal		! compile pfa as literal
	.long	    compile, fetch
	.long	    branch, 9f
1:	    !! interpretation
	.long	    fetch
9:	EXIT_4TH


!!! TO - CORE EXT 6.2.2295
IMMWORD("to", to_4th)
	.long	tick_parens
#if 1
	.long	drop
#else
	.long	lit, 1, not_equals, question_branch, 0f
	.long	    drop, THROW(32)
0:
#endif
	.long	dup, fetch, lit, value_does, not_equals, OR_ELSE(0f)
	.long	    drop, THROW(32)
0:
	.long	to_body
	!! like DOES> part of VALUE above but with ! instead of @ 
	.long	state, fetch, OR_ELSE(1f)
	    !! compilation
	.long	    literal		! compile pfa as literal
	.long	    compile, store
	.long	    branch, 9f
1:	    !! interpretation
	.long	    store
9:	EXIT_4TH
