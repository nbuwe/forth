\ forth to asm transpiler (quick and dirty prototype).  tested with
\ gforth, but i haven't even tried to compile its output.

vocabulary trans        \ main transpiler vocabulary

only forth also trans definitions

vocabulary meta         \ defining words for the target
variable meta-wid
also meta context @ meta-wid !
previous

vocabulary target       \ shadow vocabulary for the target
variable target-wid
also target context @ target-wid !
previous

: ?comp   state 0= if  -14 throw then ; \ interpreting a compile-only word


\ XXX: gforth - snumber? returns dot position
: number?
   snumber? dup if
      0> if 2 else 1 then
   then ;

\ search-word is to parse-word what find is to word
\
\ ANSI standard recommends c-addr u representation for strings instead
\ of counted strings, yet it doesn't provide a counterpart to "find"
\ that works with such strings.  There's only search-wordlist that
\ operates on a single word list.  Do I miss something obvious here?
: search-word   ( c-addr u -- 0 | xt 1 | xt -1 )
    get-order >r
    sp@ pad r@ cells move              \ copy order to pad
    r@ 0 ?do drop loop                 \ drop order from stack
    r> 0 ?do
        2dup
        pad i cells + @
        search-wordlist ?dup if
            2swap 2drop unloop exit
        then
    loop
    2drop 0 ;

: ?parsed
   ?dup 0= if drop  -16 throw then ; \ attempt to use zero-length string as a name

: word,   \ compile counted string, like c" but parses at run time
   parse-word ?parsed   ( c-addr u -- )
   dup c,      \ save count
   here swap   \ move destination
   dup allot   \ reserve space
   move ;      \ save name

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
   over + swap do        \ loop over chars
      i c@ dup xlat? ?dup if
         nip                \ original char
         [char] _ c,        \ prepend underscore
         count here swap    ( c-addr here count -- )
         dup allot move
      else
         c, \ copy as-is
      then
   loop
   here r@ - 1- r> c! ;

\ XXX: use this for predef too?
: tcreate  ( " name" -- c-addr )
   >in @ create
   >in ! here word-xlat, ;   \ leave address of xlated word (see colon)
   
\ record a mapping in the target dictionary.  this is to let the
\ transpiler know about assembler words written in real assembler in
\ the machine-dependent .S file
: predef~   \ e.g.  predef~ + plus
   create word, ;

\ like predef~ but the word is its own name
: predef=   \ e.g.  predef= lit
   >in @ create
   >in ! word, ;


: "type"   [char] " emit  type  [char] " emit ;
: tab   9 emit ;
: .long   tab ." .long" tab ;
: comment   ." /* " -trailing type space ." */" cr ;

: tliteral    .long ." lit, " 0 .r cr ;
: t2literal   .long ." two_lit, " 0 .r ." , " 0 .r cr ;
: tcompile,   .long >body count type cr ;


: tsearch-target   target-wid @ search-wordlist ;
: tsearch-meta   meta-wid @ search-wordlist ;

\ "compiling" - use target's words, except for immediates
: tsearch-word  ( c-addr u -- 0 | xt 1 | xt -1 )
   2dup 2>r
   tsearch-target dup 0>= if \ immediate or not yet defined
      drop 2r@
      tsearch-meta dup 0< if
         ." immediate mismatch: " 2r> type cr
         -13 throw  \ undefined word
      then
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

: transpile-begin   ." #define IMMEDIATE .Limm0" cr ;
: transpile-end     ." IMMEDIATE = 0" cr ;


\ takes the name of the CPP macro to use (e.g. WORD or VARIABLE) to
\ define the forth name in the generated output
: emitdef ( c-addr u " name" -- )
   >in @ tcreate        \ leaves xlated name (counted)
   swap >in !           \ restore input for parse-word below
   \ cpp directives cannot be nested in a macro, so this has to be
   \ emitted explicitly over and over (or we can bite the bullet and
   \ use m4)
   \
   \ the defining macro will use the not yet defined .Limm_name as the
   \ immediate flag.  if "immediate" follows this definition, it will
   \ set the flag.  IMMEDIATE = 0 before the next definition will
   \ provide a default if "immediate" was not used.  as(1) picks the
   \ first definition it sees.
   cr
   ." IMMEDIATE = 0" cr
   ." #undef  IMMEDIATE" cr
   ." #define IMMEDIATE .Limm_" dup count type cr
   -rot    \ xlated name away
   type                 \ macro name
   [char] ( emit
   parse-word "type"    \ forth name
   [char] , emit space
   count type           \ xlated name
   [char] ) emit
   cr ;

: ?pairs   - 0<> if  ." oops" .s -22 throw then ; \ control structure mismatch

variable lblcnt
: reset-labels   1000 lblcnt ! ;
reset-labels

: label-name   0 .r [char] $ emit ;
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
   s" CONSTANT" emitdef
   .long 0 .r cr ;

: variable
   s" VARIABLE" emitdef
   .long 0 0 .r cr ;

\ : [   [ ; immediate
: ]   transpile ;

: :   s" WORD" emitdef reset-labels transpile ;
: ;   ?comp tab ." EXIT_4TH" cr postpone [ ; immediate

: immediate   ." IMMEDIATE = IMM_FLAG" cr ; \ see emitdef

: [char] ?comp
   parse-word if c@ else drop 0 then tliteral ; immediate

: ahead  ?comp          (ahead) 1 ; immediate
: if     ?comp          (if) 1 ; immediate
: else   ?comp 1 ?pairs (ahead) swap (then) 1 ;  immediate
: then   ?comp 1 ?pairs (then) ;  immediate

: begin  ?comp          (begin) 2 ; immediate
: again  ?comp 2 ?pairs (again) ; immediate
: until  ?comp 2 ?pairs (until) ; immediate

: while  ?comp 2 ?pairs (if) 3 ;  immediate
: repeat ?comp 3 ?pairs swap (again) (then) ;  immediate


\ pre-populate target vocabulary with stubs for the asm words
also target definitions previous

predef= dup
\ ...
predef~ =       equals
predef~ +       plus

also meta

\ start processing target's forth text

variable question
42 constant answer

: ?! answer ; immediate
: test ( comment ) begin question answer = until ;

\ bye
