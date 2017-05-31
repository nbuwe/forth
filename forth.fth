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

: depth   ( -- +n )   sp@ sp0 swap - 2 rshift ;

: +!   ( n|u a-addr -- )   dup @ rot + swap ! ;
: 1+!   ( a-addr -- )   dup @ 1+ swap ! ;

: count   ( c-addr1 -- c-addr2 u )   dup c@ swap 1+ swap ;

: not   ( x -- flag)   0= ;
: <=   ( n1 n2 -- flag )   > not ;
: >=   ( n1 n2 -- flag )   < not ;

: signum   ( n -- -1|0|1 )   dup 0< swap 0> - ;   \ "sign" is taken
: <=>      ( n1 n2 -- -1|0|1 )   - signum ;       \ 3 way comparison

: within   ( test low high -- flag )
   over - >r   \ high - low
   -           \ test - low
   r> u< ;

: max  ( n1 n2 -- n3 )   2dup < if swap then drop ;
: min  ( n1 n2 -- n3 )   2dup > if swap then drop ;


\ cdef ms/rem   (  d1 n1 -- n2  d2 )   \ symmetric 64/32 -> r32 q64
\ cdef mf/mod   (  d1 n1 -- n2  d2 )   \   floored 64/32 -> m32 q64
\ cdef mu/mod   ( ud1 u1 -- u2 ud2 )   \  unsigned 64/32 -> m32 q64

\ truncate quotient to 32-bit
: sm/rem   ( d1 n1 -- n2 n3 )   ms/rem drop ;
: fm/mod   ( d1 n1 -- n2 n3 )   mf/mod drop ;

\ NB: symmetric to match "native" division
: /mod   ( n1 n2 -- n3 n4 )   >r s>d r> sm/rem ;
: mod    ( n1 n2 -- n3 )      /mod drop ;

: */mod   ( n1 n2 n3 -- n4 n5 )    >r m* r> sm/rem ;
: */   ( n1 n2 n3 -- n4 )   */mod nip ;

: um/mod   ( ud u1 -- u2 u3 )   mu/mod drop ;


: char+   ( c-addr1 -- c-addr2 )   1+ ;
: chars   ( n1 -- n2 )    ( nop ) ;

: cell+   ( a-addr1 -- a-addr2 )   4 + ;
: cell-   ( a-addr1 -- a-addr2 )   4 - ;
: cells   ( n1 -- n2 )   2 lshift ;
: aligned   ( addr -- a-addr )   3 + -4 and ;
: unaligned?   ( addr -- flag )   3 and ;
: aligned?   ( addr -- flag )   unaligned? not ;


: d>s   ( d -- n )   drop ;
: d0=   ( xd -- flag)   or 0= ;
: d0<   ( xd -- flag)   nip 0< ;
: d=    ( xd1 xd2 -- flag )   rot = if = else 2drop false then ;

: dmax  ( d1 d2 -- d3 )   2over 2over d<     if 2swap then 2drop ;
: dmin  ( d1 d2 -- d3 )   2over 2over d< not if 2swap then 2drop ;

: m+   ( d1|ud1 n -- d2|ud2 )   s>d d+ ;

: on     true swap ! ;
: off   false swap ! ;


defer throw


\ LEAVE and UNLOOP are needed pretty early in the parsing code, but
\ they and the rest of the DO loop machinery are rather low level with
\ very few dependencies, so it can be defined early.

: (do)   ( limit first -- R: leave-addr limit current )
   r>
   dup @ >r             \ end of the loop address for LEAVE
   rot                  \ first ret limit --
   \ offset limit so that last + limit causes overflow
   $80000000 swap - >r
   swap                 \ ret first --
   \ offset first accordingly too
   r@ + >r
   cell+ >r ;           \ skip end of the loop address

: (?do)
   2dup = if
      2drop (goto) branch       \ skip the loop
   else
      (goto) (do)               \ enter the loop
   then ;

: leave   ( R: leave-addr limit current -- )  \ return after the loop
   r> drop      \ return address
   2r> 2drop ;  \ loop counter and limit

: unloop   ( R: leave-addr limit current -- ) \ caller wants to exit from loop
   r>           \ save return address
   2r> 2drop    \ loop counter and limit
   r> drop      \ leave address
   >r ;         \ restore return address

: (+loop)   ( increment -- )
   r> swap      \ get our return address out of the way
   r> +? if     \ get and increment current, check overflow; see (do)
      drop      \ current
      2r> 2drop \ leave-addr and limit
      cell+ >r  \ return after the loop
   else
      >r                \ new current
      >r (goto) branch  \ repeat the loop
   then ;

: (loop)   1 (goto) (+loop) ;


variable handler \ handler off

: catch   ( xt -- error | 0 )
   sp@ >r               \ save data stack pointer
   handler @ >r         \ save previous handler
   rp@ handler !        \ set current handler to this one
   execute              \ execute the word passed
   \ no errors
   r> handler !         \ restore previous handler
   r> drop              \ discard saved stack pointer
   0 ;                  \ normal completion


variable base

: decimal   #10 base ! ;
: hex       #16 base ! ;


\ variable dp
: here   ( -- addr )   dp @ ;
: dp!    ( addr -- )   dp ! ;

: align   ( -- )   here aligned dp! ;
: allot   ( n -- )   here + dp! ;

: ,    (    x -- )   here 1 cells allot  ! ;
: c,   ( char -- )   here 1 chars allot c! ;
: 2,   (    d -- )   here 2 cells allot 2! ;

: string,   ( c-addr u -- )   \ reserve space and store string
   here swap dup allot move ;

: code,   ( c-addr u -- )   \ store binary code
   here over 2>r  string,  2r> sync-i$ ;

: pad   ( -- c-addr )   here 128 + ;


32 constant bl

\ XXX: bare minimum for the case-insensitive ICOMPARE below
: isupper   ( char -- flag )   [char] A [ char Z 1+ ] literal within ;
: tolower   ( char -- char )   dup isupper if $20 or then ;

: erase   ( c-addr u -- )   dup 0= if 2drop else  0 fill then ;
: blank   ( c-addr u -- )   dup 0= if 2drop else bl fill then ;

: /string   ( c-addr1 u1 n -- c-addr2 u2 )   rot over +   -rot - ;

: cmove   ( src dst len -- )   \ left-to-right char-by-char
   dup 1 < if exit then
   ( len ) 0 do
      over i + c@ over i + c!
   loop
   2drop ;

: cmove>   ( src dst len -- )   \ right-to-left char-by-char
   dup 1 < if exit then
   ( len ) 1- 0 swap do
      over i + c@ over i + c!
   -1 +loop
   2drop ;

: compare   ( c-addr1 u1 c-addr2 u2 -- -1|0|1 )
   rot swap     \ c-addr1 c-addr2 u1 u2
   2dup <=> >r  \ if strings are equal up to the shorter length
   min 0 ?do    \ c-addr1 c-addr2
      over i + c@  over i + c@
      <=> ?dup if
         unloop r> drop         \ drop length comparison
         -rot 2drop             \ drop strings
         exit                   \ return result of <=>
      then
   loop
   2drop r> ;                   \ return length comparison

: icompare   ( c-addr1 u1 c-addr2 u2 -- -1|0|1 )   \ case-insensitive
   rot swap     \ c-addr1 c-addr2 u1 u2
   2dup <=> >r  \ if strings are equal up to the shorter length
   min 0 ?do    \ c-addr1 c-addr2
      over i + c@ tolower  over i + c@ tolower
      <=> ?dup if
         unloop r> drop         \ drop length comparison
         -rot 2drop             \ drop strings
         exit                   \ return result of <=>
      then
   loop
   2drop r> ;                   \ return length comparison

: search   ( c-addr1 u1 c-addr2 u2 -- c-addr3 u3 flag )
   2 pick over -   \ c-addr1 u1 c-addr2 u2 u1-u2
   dup 0< if
      drop 2drop
      false exit
   then
   1+ 0 ?do   \ c-addr1 u1 c-addr2 u2
      3 pick i + over 2over   \ c-addr1 u1 c-addr2 u2 c-addr1+i u2
      compare 0= if
         2drop i /string
         unloop
         true exit
      then
   loop ;


: cr      $0a emit ;
: space   bl emit ;
: spaces   ( n -- )   0 max 0 ?do space loop ;

\ cword~ emit emit_impl
\ cword~ type emit_impl
\ cword~ accept accept_4th


variable hld
: <#   ( -- )   pad hld ! ;
: hold   ( char -- )   -1 hld +! hld @ c! ;
: #>   ( xd -- c-addr u )   2drop hld @ pad over - ;

: sign   ( n -- )   0< if [char] - hold then ;

: #   ( ud1 -- ud2 )   \ convert one digit
   base @ mu/mod rot   \ remainder
   dup 9 > if
      [ char a #10 - ] literal
   else
      [char] 0
   then
   + hold ;

: #s   ( ud -- 0. )   begin # 2dup d0= until ;

: (.)   ( n -- c-addr u )
   dup     \ keep a copy for sign
   abs 0   \ make positive, extend to double cell
   <# #s rot sign #> ;

: .   ( n -- )   (.) type space ;
: .r   ( n1 n2 -- )
   >r (.) r>   over - spaces   type ;

: (u.)   ( u -- c-addr u )   0 <# #s #> ;
: u.   ( u -- )   (u.) type space ;
: u.r   ( u n -- )
   >r (u.) r>   over - spaces   type ;

: (d.)   ( d -- c-addr u )
   tuck    \ keep a copy for sign
   dabs
   <# #s rot sign #> ;

: d.   ( d -- )   (d.) type space ;
: d.r   ( d n -- )
   >r (d.) r>   over - spaces   type ;

: (ud.)   ( ud -- c-addr u )   <# #s #> ;
: ud.   ( ud -- )   (ud.) type space ;
: ud.r   ( ud n -- )
   >r (ud.) r>   over - spaces   type ;

: ?   @ . ;

: .s
   sp@ sp0 = if exit then
   dup         \ spill TOS to memory
   sp@         \ points to the spilled TOS
   sp0 cell- cell-   \ bottommost element
   do
      i @ .
   [ -1 cells ] literal +loop
   drop ;


\ XXX: only support tib for now
\ constant tib
variable #tib
variable >in

: refill
   tib 4096 accept
   dup -1 = if drop false exit then
   #tib !  0 >in !  true ;

: source   ( -- c-addr u )   tib #tib @ ;

: skip-delim   ( char -- )
   >r   \ stash away the delimiter
   source swap begin   \ buflen buf --
      over >in @ >  dup if drop      \ range check ... and
         dup >in @ + c@ r@ =  then   \ check for delimiter
   while
         >in 1+!
   repeat
   2drop
   r> drop ;

\ XXX: TODO: loop with pointer not index?
: parse   ( char "ccc<char>" -- c-addr u )
   source nip >in @   \ parse area limits
   2dup <= if
      \ parse area exhausted, return empty word at the end
      2drop drop   \ limits and delimiter
      source drop >in @ +   \ result address
      0                     \ result length
      exit
   then
   \ delim buflen pos --
   dup >r   \ stash away current >in
   0 -rot   \ init result length
   \ delim 0 buflen pos --
   do
      >in 1+!
      over   \ delimiter
      source drop i + c@ = if leave then
      1+     \ increment result length
   loop
   nip               \ delimiter
   source drop r> +  \ result address (at old >in)
   swap ;            \ result length

: \   $0a parse 2drop ; immediate
: (   [char] ) parse 2drop ; immediate

: .(   [char] ) parse type ; immediate

: parse-word   ( "<spaces>name<space>" -- c-addr u )
   bl skip-delim bl parse ;

: ?parsed   \ attempt to use zero-length string as a name
   ?dup 0= if drop  -16 throw then ;

: char   ( "<spaces>name" -- char )
   parse-word if c@ else drop 0 then ;

: word   ( char "<chars>ccc<char>" -- c-addr )
   dup skip-delim parse   \ str len --
   dup >r
   here 1+ swap move   \ copy parsed string
   r@ here c!          \ set its count
   bl here 1+ r> + c!  \ space after string
   here ;


: base-0   ( base -- '0' upper )   \ range of arabic digits; for "within"
   dup #10 <= if
      [char] 0 tuck +
   else
      drop [char] 0 [ char 9 1+ ] literal
   then ;

: base-a   ( base  -- 'a' upper)   \ range of alphabetic digits; for within
   10 - [char] a tuck + ;

: digit?   ( char base -- value true | false )
   swap >r   \ stash away the char
   \ numeric digit?
   dup base-0
   r@
   -rot within if
      drop r> [char] 0 -
      true exit
   then
   \ does base use alpha digits?
   dup 10 > if
      \ alpha digit?
      dup base-a
      r> $20 or dup >r   \ downcase char
      -rot within if
         drop r> [ char a #10 - ] literal -
         true exit
      then
   then
   \ not a digit
   drop r> drop
   false ;

: >number   ( ud1 s1 l1 -- ud2 s2 l2)
   2dup >r >r    \ stash away initial string
   over + swap   \ pointer loop range
   ?do
      i c@ base @ digit? if
         \ lo hi digit --
         swap   \ hi
         base @ um* drop   \ lo(hi * base)
         \ lo digit lo(hi*base) --
         \ NB: digit lo(hi*base) is already a double-cell sum!
         rot    \ lo
         base @ um* \ double cell lo*base
         d+         \ digit + ud1 * base
      else
         \ bad character, compute new string position
         i unloop    \ current pointer (NB: while we still can)
         dup         \ new position
         r> -        \ distance from the original position
         r> swap -   \ decrease lenth by it
         exit
      then
   loop
   \ loop ended normally, so we used up the whole string
   2r> + 0 ;

\ XXX: TODO: refactor and split
\ XXX: TODO: handle <cnum> := '<char>'
: number?   ( str len -- 0 | n 1 | d 2 )
   base @ >r    \ save base in case of sigil
   false >r     \ minus sign
   \ Forth 200x #<dec>, $<hex>, %<bin> base sigils
   dup 1 > if
      over c@
      \ XXX: TODO: case
           dup [char] # = if #10
      else dup [char] $ = if #16
      else dup [char] % = if #2
      else                   0
      then then then
      nip
      ?dup if
         base !
         1 /string
      then
   then
   \ minus sign
   dup 1 > if
      over c@ [char] - = if
         r> drop true >r
         1 /string
      then
   then
   \ try to convert to number
   0. 2swap >number
   dup 0= if
      \ no trailer, single cell number
      2drop   \ string
      drop    \ to single cell
      r> if negate then
      r> base !
      1 exit
   else
      dup 1 = if
         over c@ [char] . = if
            \ dot at the end, double cell number
            2drop   \ string
            r> if dnegate then
            r> base !
            2 exit
         then
      then
   then
   \ failed to convert
   2drop 2drop
   r> drop
   r> base !
   false ;

: convert   ( ud1 cstr -- ud2 caddr )   char+ 256 >number drop ;


\ ====================

variable state
: [   state off ; immediate
: ]   state on ;

: ?comp   \ interpreting a compile-only word
   state @ 0= if  -14 throw then ;

: compile,   , ;
: compile
   r> dup cell+ >r
   @ compile, ;

: literal    state @ if compile lit , then ; immediate
: 2literal   state @ if compile 2lit , , then ; immediate
: [char]     ?comp char postpone literal ; immediate


: (c")
   r> dup count + aligned >r ;

: (s")
   r> dup cell+ swap @
   2dup + aligned >r ;

: (.")
   r> dup cell+ swap @
   2dup + aligned >r
   \ XXX: the above is (s")
   type ;


\ XXX: 2variable current
predef current
: latest   current @ @ ;


: >body   cell+ ;
: body>   cell- ;
: >link   cell- ;
: link>   cell+ ;

$80 constant &iflag
$40 constant &sflag

: immediate  latest c@ [ &iflag ] literal or latest c! ;
: smudge     latest c@ [ &sflag ] literal or latest c! ;
: unsmudge   latest c@ [ &sflag invert ] literal and latest c! ;

\ XXX: TODO? when DOES> is implemented
\ : flag-test create, does> @ swap @ and ;
\ &iflag flag-test immediate?
\ &sflag flag-test smudged?
: immediate?   ( nfa -- flag )   c@ [ &iflag ] literal and ;
: smudged?     ( nfa -- flag )   c@ [ &sflag ] literal and ;

: name-count   count $1F and ;

: n>link   name-count + aligned ;
: name>    n>link link> ;

: >name   ( xt -- nfa)
   dup unaligned? if drop 0 exit then \ must be aligned
   >link        \ LFA is the aligned address after name
   dup          \ keep a copy for comparisons
   dup 32 -     \ limit - the farthest away that NFA can be
   swap cell- do        \ start one cell before LFA and step backwards
      \ The byte may have its upper bit set (potential immediate
      \ flag), since 8-bit characters are not allowed in a name.
      \ If the byte is less than 32, it's not a printable character
      \ and must be the name count byte.
      i c@ [ &iflag $1f or invert ] literal and 0= if
         \ looks like a count byte, but is the count right?
         dup i n>link = if
            drop i
            unloop exit
         then
      then
   [ -1 cells ] literal +loop
   drop 0 ;


\ Traditional Forth's FIND takes counted string from traditional
\ WORD, but that requires temporary space to hold the counted
\ string, so ANS Forth suggests using PARSE instead.  The
\ corresponding word to search the <c-addr, u> string is
\ SEARCH-WORDLIST.
\
\ SEARCH-CURRENT is interim chimera until proper vocabularies are
\ provided.  For now we only have single wordlist, so:
\
\    : search-current ( c-addr u -- 0 | xt 1 | xt -1 )
\       get-current search-wordlist ;
: search-current
   ?dup 0= if drop 0 exit then   \ can't find empty string
   latest begin   \ ... nfa --
      dup smudged? not if
         >r   \ stash away NFA
         2dup r@ name-count icompare if
            \ not the word we are searching for
            r>   \ get back NFA
         else
            \ we've got a match
            2drop   \ the word we searched for
            r>      \ word's NFA
            dup name> swap   \ xt nfa --
            immediate? 6 rshift 1- \ nfa -> 1 for immediate, -1 otherwise
            exit
         then
      then
      n>link @   \ ... previous-nfa --
      ?dup 0=
   until
   2drop false ;

: find   ( c-addr -- c-addr 0 | xt 1 | xt -1 )
   dup count search-current
   dup if rot drop then ;


\ helper for ' and the like that do the parse/search combo
: (')   ( "<spaces>name" -- 0 | xt 1 | xt -1 )
   parse-word ?parsed search-current ;

: '   (') ?dup if drop else ( undefined )  -13 throw then ;
: [']   ?comp ' postpone literal ; immediate \ XXX: use compile,

: [compile]   ?comp ' compile, ; immediate

: postpone
   ?comp
   (') 0< if
      compile compile
   then
   compile, ; immediate


\ ==================== repl

: interpret
   begin
      parse-word ?dup 0= if drop exit then
      2dup search-current ?dup if
         2swap 2drop
         1+ if
            execute
         else
            state @ if compile, else execute then
         then
      else
         2dup number? ?dup if
            2 = if
               2swap 2drop
               postpone 2literal
            else
               -rot 2drop
               postpone literal
            then
         else
            type [char] ? emit cr
         then
      then
   again ;

: bye   (bye) ;


variable abort-message

predef~ throw-msgtab throw_msgtab

: throw-message ( code -- flag )
   dup 0< if                    \ reserved throw codes are negative
      negate
      dup throw-msgtab @ < if   \ at offset 0 is the number of messages
         cells throw-msgtab + @ \ message for this code
         exit
      then
   then
   drop 0 ;

: report-exception   ( code -- )
   dup -2 = if
      drop
      abort-message @ ?dup if
         dup @ swap cell+ swap
         type cr
         abort-message off
      then
   else
      dup throw-message ?dup if
         count type cr
         drop
      else
         dup -1 <> if ." THROW " . cr else drop then
      then
   then ;


: quit
   (quit)
   decimal
   postpone [
   begin
      state @ not if ." ok " then
      refill while
         ['] interpret catch
         ?dup if
            dup report-exception
            -2 0 within if  \ ABORT" (-2) or ABORT (-1)?
               ." ABORT" cr
               \ open-coded ABORT to break mutual dependency
               (abort) recurse
            then
         then

   repeat
   bye ;

: abort   (abort) quit ;


:noname ( is throw )   ( code | 0 -- )
   ?dup if
      handler @ ?dup 0= if   \ is there a catch?
         report-exception
         ." ABORT" cr
         abort
      then
      rp!               \ restore saved return stack
      r> handler !      \ restore previous handler
      r>                \ get saved stack pointer
      swap >r           \ stash away error code so that it survives sp!
      sp!               \ restore stack pointer
      drop              \ invalid TOS (was xt)
      r>                \ get error code back
   then ;
is throw

: abort   -1 throw ;




\ ==================== defining words &co


\ XXX: TODO: symbol next-code
\ where symbol is like constant with asm symbol as value
predef~ next-code next_code

: create
   parse-word ?parsed
   align here >r   \ save NFA
   \ Name Field
   dup c, string, align
   \ Link Field
   latest ,
   r> current @ !
   \ Code Field
   \ XXX: for now we need to protect next-code with compile
   compile next-code ;

: variable    create 1 cells allot ;
: 2variable   create 2 cells allot ;
: buffer:     create allot ;

\ set CFA of the latest word to ...
: (;code)   r>   latest name> ! ; \ the asm code after this word
: (;alit)   r> @ latest name> ! ; \ the address compiled after this word

: constant    create  , does>  @ ;
: 2constant   create 2, does> 2@ ;

predef~ call-code call_code \ XXX

: :
   create smudge ]
   (;alit) call-code ;

: ;
   compile exit
   unsmudge postpone [ ; immediate

: does> ?comp
   compile (;code)
   does-thunk  dup @ swap cell+ swap \ code len -- (like sliteral)
   code, ; immediate


: :noname
   \ Name Field: empty name, smudged
   align [ &sflag ] literal c,
   \ Link Field: no link
   align 0 ,
   \ Code Field
   here   \ xt we leave on the stack
   compile call-code ] ;

\ XXX: doesn't work with :NONAME
: recurse   latest name> compile, ; immediate


: defer ( "name" -- )
   create compile abort
 does>
   @ execute ;

: defer@   ( xt1 -- xt2 )   >body @ ;
: defer!   ( xt2 xt1 -- )   >body ! ;

: is
   state @ if
      postpone ['] postpone defer!
   else
      ' defer!
   then ; immediate

: action-of
   state @ if
      postpone ['] postpone defer@
   else
      ' defer@
   then ; immediate


: compile"
   r> dup cell+ >r @   \ fetch runtime word after us (like compile)
   [char] " parse      \ XXX: TODO: handle failure
   rot compile,        \ compile runtime word
   dup , string,       \ compile string
   align ;

: s"   ?comp compile" (s") ; immediate
: ."   ?comp compile" (.") ; immediate

: c"
   ?comp
   [char] " parse   \ XXX: TODO: handle failure
   compile (c")
   dup c, string,
   align ; immediate

: (abort")
   if
      r@ abort-message !
      -2 throw
   else
      \ skip the abort message after us; cf. (s")
      r> dup cell+ swap @ + aligned >r
   then ;

: abort"  ( x | 0 -- )
   ?comp compile" (abort") ; immediate


\ forward - mark is the location to patch with the destination
: >mark      ( C: -- mark )   here 0 , ;
: >resolve   ( C: mark -- )   here swap ! ;

\ backward - mark is the destination address
: <mark      ( C: -- mark )   here ;
: <resolve   ( C: mark -- )   , ;

\ forward jumps
: (ahead)   compile  branch >mark ;
: (if)      compile ?branch >mark ;
: (then)    >resolve ;

\ backward jumps
: (begin)   <mark ;
: (again)   compile  branch <resolve ;
: (until)   compile ?branch <resolve ;


: ?pairs   ( C: expected actual -- )
   <> if -22 throw then ;   \ control structure mismatch

: ahead   ?comp          (ahead) 1 ; immediate
: if      ?comp          (if) 1 ; immediate
: else    ?comp 1 ?pairs (ahead) swap (then) 1 ; immediate
: then    ?comp 1 ?pairs (then) ; immediate

: begin  ?comp          (begin) 2 ; immediate
: again  ?comp 2 ?pairs (again) ; immediate
: until  ?comp 2 ?pairs (until) ; immediate

: while  ?comp 2 ?pairs (if) 3 ;  immediate
: repeat ?comp 3 ?pairs swap (again) (then) ;  immediate

: do
   ?comp
   compile (do)
   >mark \ leave address
   <mark \ address to loop back to
   4 ; immediate

: ?do
   ?comp
   compile (?do)
   >mark \ leave address
   <mark \ address to loop back to
   4 ; immediate

: loop
   ?comp 4 ?pairs
   compile (loop)
   <resolve     \ jump to the beginning of the loop
   >resolve ; immediate \ leave address after the loop

: +loop
   ?comp 4 ?pairs
   compile (+loop)
   <resolve     \ jump to the beginning of the loop
   >resolve ; immediate \ leave address after the loop

\ XXX: stub for now
: environment?   2drop false ;
