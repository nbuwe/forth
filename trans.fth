\ Copyright (c) 2017 Valery Ushakov
\ All rights reserved.
\
\ Redistribution and use in source and binary forms, with or without
\ modification, are permitted provided that the following conditions
\ are met:
\ 1. Redistributions of source code must retain the above copyright
\    notice, this list of conditions and the following disclaimer.
\ 2. Redistributions in binary form must reproduce the above copyright
\    notice, this list of conditions and the following disclaimer in the
\    documentation and/or other materials provided with the distribution.
\
\ THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
\ IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
\ OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
\ IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
\ INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
\ NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
\ DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
\ THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
\ (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
\ THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

vocabulary trans        \ main transpiler vocabulary

only forth also trans definitions

\ XXX: gforth - snumber? returns dot position
: number?
   snumber? dup if
      0> if 2 else 1 then
   then ;


vocabulary meta         \ defining words for the target
also meta context @ constant meta-wid
previous

vocabulary target       \ shadow vocabulary for the target
also target context @ constant target-wid
previous

: search-meta   meta-wid search-wordlist ;
: search-target   target-wid search-wordlist ;

: noname-basename s" .Lnoname" ;


: ?parsed
   ?dup 0= if drop  -16 throw then ; \ attempt to use zero-length string as a name

: string,   ( c-addr u -- )   \ reserve space and store string
   here swap dup allot move ;

: word,   \ compile as counted string, like c" but parses at run time
   parse-word ?parsed   ( c-addr u -- )
   \ XXX: TODO: check length
   dup c, string, ;


bl 1+ constant xlat-base
: xlat-off   xlat-base - cells ;

126 constant xlat-last
xlat-last 1+ xlat-off constant xlat-size

create xlat   xlat-size allot   xlat xlat-size 0 fill

: xlat-pos   xlat-off xlat + ;

: xlat:   xlat-pos here word, swap ! ;
: xlat?   xlat-pos @ ;

char ! xlat: exclam     char " xlat: dquote     char # xlat: hash
char $ xlat: dollar     char % xlat: percent    char & xlat: and
char ' xlat: quote      char ( xlat: lparen     char ) xlat: rparen
char * xlat: star       char + xlat: plus       char , xlat: comma
char - xlat: minus      char . xlat: dot        char / xlat: slash

char : xlat: colon      char ; xlat: semicolon  char < xlat: less
char = xlat: equal      char > xlat: greater    char ? xlat: question
char @ xlat: at

char [ xlat: lbrack     char \ xlat: backslash  char ] xlat: rbrack
char ^ xlat: circumflex char _ xlat: _          char ` xlat: backtick

char { xlat: lbrace     char | xlat: bar        char } xlat: rbrace
char ~ xlat: tilde

: word-xlat,
   parse-word ?parsed    ( c-addr u -- )
   here >r 0 c,          \ reserve and save count location
   over c@ [char] 0 [ char 9 1+ ] literal within if
      [char] _ c,
   then
   over + swap do        \ loop over chars
      i c@ dup xlat? ?dup if
         nip                \ lose original char
         [char] _ c,        \ append underscore ...
         count string,      \ ... and char's name
      else
         c,                 \ copy char as-is
      then
   loop
   here r@ - 1- r> c! ;


\ target vocabulary collects shadow definitions with the following
\ parameter data:
\
\    +---+---+---+---+
\    |    version    |
\    +---+---+---+---+
\    |cnt| f | o | o |
\    +---+---+---+---+
\
\ version is inited to -1 when the word is being defined for the first
\ time and tsearch-word handles considers such a word still hidden.
\ redefining a word just bumps its version so that we can generate a
\ new unique asm symbol.

variable tlatest    0 tlatest !
variable tversion   0 tversion !

: treveal   tversion @ tlatest @ ! ;
: thide   tversion @ 1- tlatest @ ! ;

: thidden?   ( xt -- )   >body @ -1 = ;

: tsearch-target   ( c-addr u -- 0 | xt 1 | xt -1 )
   search-target dup if
      over thidden? if 2drop 0 then
   then ;

: t(')   ( "<spaces>name" -- 0 | xt 1 | xt -1 )
   parse-word ?parsed tsearch-target ;

: t'   ( "<spaces>name" -- xt )
   t(') 0= if ( undefined word ) -13 throw then ;


: (tcreate)   ( "<spaces>name" -- )
   create here tlatest !  0 dup , tversion ! ;

: tcreate-new   ( "<spaces>name" -- )
   >in @ (tcreate) >in ! word-xlat, ;

: tcreate-version   ( xt -- )
   >body tlatest !
   tlatest @ @ 1+ tversion !
   treveal ;

: tcreate
   >in @ parse-word ?parsed
   search-target if
      nip   \ consume input word
      tcreate-version
   else
      >in !   \ restore input
      tcreate-new
   then ;

: type-sym  ( body -- )
   dup cell+ count type  \ basename
   @ ?dup if             \ needs version suffix?
      [char] . emit
      0 .r
   then ;

: tlatest-sym   tlatest @ type-sym ;


\ record a mapping in the target dictionary.  this is to let the
\ transpiler know about assembler words written in real assembler in
\ the machine-dependent .S file
: predef~   (tcreate) word, ;   \ to accomodate existing names
: predef   tcreate ;            \ xlated


: "type"
   [char] " emit
   over + swap do
      i c@
      dup [char] \ = over [char] " = or if  [char] \ emit then
      emit
   loop
   [char] " emit ;

: tab   9 emit ;
: .long   tab ." .long" tab ;
: comment   ." /* " -trailing type space ." */" cr ;

: tallot      tab ." .skip" tab 0 .r cr ;
: t,          .long 0 .r cr ;

: tliteral    .long ." lit, " 0 .r cr ;
: t2literal   .long ." two_lit, " 0 .r ." , " 0 .r cr ;
: tcompile,   .long >body type-sym cr ;

: t[']
   t(') if
      .long ." lit" cr
      tcompile,
   else
      -13 throw   \ undefined word
   then ;


\ "compiling" - use target's words, except for immediates
: tsearch-word  ( c-addr u -- 0 | xt 1 | xt -1 )
   2dup 2>r
   search-meta dup 1 <> if
      if drop then   \ xt of non-immediate(?!) word in meta
      2r@ tsearch-target
   then
   2r> 2drop ;

: transpile
   ] begin
      parse-word ?dup if  ( c-addr u -- )
         2dup tsearch-word ?dup if
            \ found a word
            2nip  \ word's c-addr u string
            1+ if
               execute  \ immediate
            else
               tcompile,
            then
         else
            \ not a word, may be a number?
            2dup number? ?dup 0= if
               ." undefined word: " type cr
               -13 throw  \ undefined word
            then
            \ yes, a number
            2 = if  t2literal else  tliteral then
            2drop  \ number's c-addr u string
         then
         state @  \ continue if still compiling
      else
         \ end of input
         drop  \ leftover c-addr from the unsuccessful parse-word
         refill  \ continue if there's still input
      then
   0= until ;

: transpile-begin
   .\" #include \"forth-prologue.S\"" cr
   ." #undef  IMMEDIATE" cr
   ." #define IMMEDIATE .Limm0" cr ;
: transpile-end     ." IMMEDIATE = 0" cr ;


\ takes the name of the CPP macro to use (e.g. WORD or VARIABLE) to
\ define the forth name in the generated output
: emitdef ( c-addr u "<spaces>name" -- )
   >in @ tcreate >in !  \ restore input for parse-word below
   \ the defining macro will use the not yet defined .Limm_name as the
   \ immediate flag.  if "immediate" follows this definition, it will
   \ set the flag.  IMMEDIATE = 0 before the next definition will
   \ provide a default if "immediate" was not used.  as(1) picks the
   \ first definition it sees.
   cr
   ." IMMEDIATE = 0" cr
   ." #undef  IMMEDIATE" cr
   ." #define IMMEDIATE .Limm_" tlatest-sym cr
   type                 \ macro name
   [char] ( emit
   parse-word "type"    \ forth name
   [char] , emit space
   tlatest-sym          \ xlated name
   [char] ) emit
   cr ;

\ make real constant definitions in the meta vocabulary so that
\ interpreted code can use them
: meta-constant
   meta-wid set-current
   >in @  over constant  >in !
   target-wid set-current ;


: ?comp   state @ 0= if  -14 throw then ; \ interpreting a compile-only word
: ?pairs   - 0<> if  ." oops" .s -22 throw then ; \ control structure mismatch

variable lblcnt   0 lblcnt !
: reset-labels   100 lblcnt @  over / 1+ *  lblcnt ! ;

: label-name   ." .L" 0 .r ;
: label-ref   .long 4 spaces label-name cr ;
: label   label-name [char] : emit cr ;

: new-label   lblcnt @ dup 1+ lblcnt ! ;

: >mark   new-label dup label-ref ;
: >resolve    label ;

: <mark   new-label dup label ;
: <resolve   label-ref ;

: (ahead)   .long ." branch"          cr >mark ;
: (if)      .long ." question_branch" cr >mark ;
: (then)    >resolve ;

: (begin)   <mark ;
: (again)   .long ." branch"          cr <resolve ;
: (until)   .long ." question_branch" cr <resolve ;

: (do)   ?comp .long type cr >mark <mark 4 ;
: (loop) ?comp rot 4 ?pairs .long type cr <resolve >resolve ;

4 constant cell

\ defining words go to the meta vocabulary, but that vocabulary is not
\ in the search order
also meta definitions previous

: \   $0a parse comment ; immediate
: (   [char] ) parse comment ; immediate

: cell+   cell + ;
: cells   cell * ;
: aligned   cell+ 1- cell negate and ;
: align   ." p2align 2, 0" cr ;

: constant
   meta-constant
   s" CONSTANT" emitdef t, ;

: variable    s" VARIABLE" emitdef  0 t, ;
: 2variable   s" VARIABLE" emitdef  0 t, 0 t, ;
: buffer:     s" VARIABLE" emitdef  tallot ;

: [   postpone [ ; immediate
: ]   transpile ;

: :   s" WORD" emitdef  thide reset-labels transpile ;
: ;   ?comp  tab ." EXIT_4TH" cr  treveal  postpone [ ; immediate

: :noname
   noname-basename tsearch-target drop
   dup tcreate-version
   cr ." NONAME(" tlatest-sym ." )" cr
   thide reset-labels transpile ;

\ XXX: CONSTANT() macro refers to "constant_does"
: does> ?comp
   .long ." _lparen_semicoloncode_rparen"  cr
   ." DOES_4TH(" treveal tlatest-sym thide ." _does)" cr ; immediate

: recurse   treveal tlatest @ body> tcompile, thide ; immediate


\ don't bother arranging for default ABORT action, just let linking
\ fail if there is no top-level IS call later
: defer
   s" DEFER" emitdef
   .long ." .L" tlatest-sym ." _xt" cr ;

: '   t' ;

: is
   state @ if
      \ postpone ['] postpone defer!
      t['] .long ." defer_exclam" cr
   else
      t' ." .L" >body type-sym ." _xt = "
      >body type-sym cr
   then ; immediate


: immediate
   immediate
   ." IMMEDIATE = IFLAG" cr ; \ see emitdef

: literal
   state @ if tliteral then ; immediate

: [']   t['] ; immediate

: postpone
   t(') ?dup if
      1+ if   \ immediate
         tcompile,
      else
         .long ." compile" cr
         tcompile,
      then
   else
      \ ... undefined word
   then ; immediate

: [char] ?comp
   parse-word if c@ else drop 0 then tliteral ; immediate

: ." ?comp
   [char] " parse
   .long ." DOTQ(" "type" ." )" cr ; immediate

: s"
   ?comp
   [char] " parse
   .long ." SQ(" "type" ." )" cr ; immediate

: ahead  ?comp          (ahead) 1 ; immediate
: if     ?comp          (if) 1 ; immediate
: else   ?comp 1 ?pairs (ahead) swap (then) 1 ;  immediate
: then   ?comp 1 ?pairs (then) ;  immediate

: begin  ?comp          (begin) 2 ; immediate
: again  ?comp 2 ?pairs (again) ; immediate
: until  ?comp 2 ?pairs (until) ; immediate

: while  ?comp 2 ?pairs (if) 3 ;  immediate
: repeat ?comp 3 ?pairs swap (again) (then) ;  immediate

: do    s" _lparendo_rparen"          (do) ; immediate
: ?do   s" _lparen_questiondo_rparen" (do) ; immediate
: loop  s" _lparenloop_rparen"      (loop) ; immediate
: +loop s" _lparen_plusloop_rparen" (loop) ; immediate

\ pre-populate target vocabulary with stubs for the asm words
also target definitions previous

predef~ .Lnoname .Lnoname   \ XXX: placeholder
include asmwords.fth

also meta

\ start processing target's forth text
transpile-begin
include forth.fth
transpile-end
\ bye
