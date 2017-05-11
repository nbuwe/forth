\ constants
predef sp0
predef rp0

predef~ (abort) abort_parens
predef~ (quit) quit_parens
predef~ sp@ sp_fetch
predef~ (bye) bye_parens
predef~ breakpoint breakpoint
predef~ execute execute
predef~ exit exit_4th
predef~ (goto) goto_parens
predef~ drop drop
predef~ dup dup
predef~ ?dup question_dup
predef~ swap swap
predef~ over over
predef~ nip nip
predef~ tuck tuck
predef~ rot rot
predef~ -rot minus_rot
predef~ pick pick
predef~ roll roll
\ predef~ 2drop two_drop
\ predef~ 2dup _two_dup
\ predef~ 2swap two_swap
\ predef~ 2over two_over
predef~ depth depth
predef~ >r to_r
predef~ r> r_from
predef~ r@ r_fetch
predef~ 2>r two_to_r
predef~ 2r> two_r_from
predef~ 2r@ two_r_fetch
predef~ 0= zero_equals
predef~ 0<> zero_not_equals
predef~ 0> zero_greater
predef~ 0< zero_less
predef~ = equals
predef~ <> not_equals
predef~ < less_than
predef~ > greater_than
predef~ max max
predef~ min min
predef~ u< u_less_than
predef~ u> u_greater_than
predef~ false false
predef~ true true
predef~ negate negate
predef~ abs abs
predef~ + plus
predef~ +? plus_question
predef~ - minus
predef~ 1+ one_plus
predef~ 1- one_minus
predef~ * star
predef~ / slash
predef~ u/ u_slash
predef~ 2* two_star
predef~ 2/ two_slash
predef~ invert invert
predef~ and and
predef~ or or
predef~ xor xor
predef~ lshift lshift
predef~ rshift rshift
\ predef~ aligned aligned
\ predef~ cell+ cell_plus
\ predef~ cell- cell_minus
\ predef~ cells cells
\ predef~ char+ char_plus
\ predef~ chars chars
predef~ @ fetch
predef~ ! store
\ predef~ +! plus_store
\ predef~ 1+! one_plus_store
predef~ c@ c_fetch
predef~ c! c_store
\ predef~ 2@ two_fetch
predef~ 2! two_store
\ predef~ count count
predef~ cmove cmove
predef~ cmove> cmove_up
predef~ compare compare
predef~ fill fill
predef~ move move
predef~ s>d s2d
predef~ dnegate dnegate
predef~ d+ d_plus
predef~ m* m_star
predef~ um* um_star
predef~ lit lit
\ predef~ 2lit two_lit
predef~ branch branch
predef~ ?branch question_branch
\ predef~ i i
\ predef~ j j_4th

\ C words
predef~ ms/rem ms_slash_rem
predef~ mf/mod mf_slash_mod
predef~ mu/mod mu_slash_mod

predef~ emit emit
predef~ type type
predef~ accept accept_4th

predef tib \ constant address
predef dp 
