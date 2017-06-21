/*
 * Bits and pieces that the transpiler doesn't generate.
 * For all of these there are symbols involved one way of the other.
 *
 * For CWORD()s it's easier to manually write the definition here and
 * predef it rather than predef the C symbol and invent Forth syntax
 * so that the transpiler can emit the same definition.	
 *
 * For variables that can be statically initialized the additional
 * problem is that Forth variables are not initialized when created.
 * Normally you do VARIABLE V <value> V ! - but transpiler cannot
 * easily merge those.
 *
 * Another problem with that, as well as for the CONSTANT values, is
 * that the transpiler would need to somehow distinguish between, say
 * 32 CONSTANT BL (and emit 32) and SOME-SYMBOL CONSTANT S (and emit
 * symbol name).  And things begin to get even more interesting with
 * expressions like SOME-SYMBOL 4 CELLS +
 */

VARIABLE("dp", dp)
	.long	.L_DATA_SPACE

CONSTANT("tib", tib)
	.long	terminal_input_buffer

CWORD("ms/rem", ms_slash_rem)
	.long	ms_slash_rem_impl
CWORD("mf/mod", mf_slash_mod)
	.long	mf_slash_mod_impl
CWORD("mu/mod", mu_slash_mod)
	.long	mu_slash_mod_impl

CWORD("emit", emit)
	.long	_C_LABEL(emit_impl)
CWORD("type", type)
	.long	_C_LABEL(type_impl)
CWORD("accept", accept_4th)	/* XXX: avoid name clash with accept(2) */
	.long	_C_LABEL(accept_impl)

CONSTANT("forth-wordlist", forth_wordlist)
	.long	.LFORTH_WID

VARIABLE("current", current)
	.long	.LFORTH_WID

#include "forth-throw.S"
