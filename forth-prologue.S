/*
 * XXX: Bits and pieces that the transpiler doesn't generate yet.
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

VARIABLE("current", current)
	.long	. + 4
	/* this will be part of a vocabulary */
	.long .LATEST
