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

CONSTANT("ib0", ib0)
	.long	input_buffer_start

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

#ifndef _KERNEL
CWORD("open-file", open_file)
	.long	_C_LABEL(open_file_impl)
CWORD("create-file", create_file)
	.long	_C_LABEL(create_file_impl)
CWORD("close-file", close_file)
	.long	_C_LABEL(close_file_impl)
CWORD("read-line", read_line)
	.long	_C_LABEL(read_line_impl)
#endif /* _KERNEL */

VARIABLE("fence", fence)
	.long	.FENCE

VARIABLE("wid-list", wid_list)
	.long	.LATESTWID

CONSTANT("forth-wordlist", forth_wordlist)
	.long	forth_wid

VARIABLE("current", current)
	.long	forth_wid

/*
 * 16 cells constant #order-stack 
 * #order-stack buffer: order-stack
 * order-stack #order-stack + constant osp0
 * variable osp
 * only
 */
VARIABLE("order-stack", order_stack)
	.skip	15 * 4
.Lorder_stack_pointer:
	.long	forth_wid
.Lorder_stack_bottom:

CONSTANT("osp0", osp0)
	.long	.Lorder_stack_bottom
VARIABLE("osp", osp)
	.long	.Lorder_stack_pointer

#include "forth-throw.S"
