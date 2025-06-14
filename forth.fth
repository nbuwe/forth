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

\ Portions of this code are based on Forth 2012 reference
\ implementation https://forth-standard.org/standard/implement

: depth   ( -- +n )   sp@ sp0 swap - 2 rshift ;

: +!   ( n|u a-addr -- )   dup @ rot + swap ! ;
: 1+!   ( a-addr -- )   dup @ 1+ swap ! ;

: not   ( x -- flag )   0= ;

: <=   ( n1 n2 -- flag )   > not ;
: >=   ( n1 n2 -- flag )   < not ;

: u<=   ( u1 u2 -- flag )   u> not ;
: u>=   ( u1 u2 -- flag )   u< not ;

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

: round-down   ( addr1 align -- addr2 )   1- invert and ;
: round-up     ( addr1 align -- addr2 )   1- tuck + swap invert and ;


: d>s   ( d -- n )   drop ;
: d0=   ( xd -- flag )   or 0= ;
: d0<   ( xd -- flag )   nip 0< ;
: d=    ( xd1 xd2 -- flag )   rot = if = else 2drop false then ;

: dmax  ( d1 d2 -- d3 )   2over 2over d<     if 2swap then 2drop ;
: dmin  ( d1 d2 -- d3 )   2over 2over d< not if 2swap then 2drop ;

: m+   ( d1|ud1 n -- d2|ud2 )   s>d d+ ;

\ transfer n items and count to the return stack
: n>r   ( xn .. x1 n -- ; R: -- x1 .. xn n )
   dup
   begin
      dup while
         rot r> swap >r >r       \ xn .. n n -- ;  R: .. x1 --
         1-                      \ xn .. n 'n -- ; R: .. x1 --
   repeat
   drop                          \ n -- ; R: x1 .. xn --
   r> swap >r >r ;

\ pull N items and count off the return stack
: nr> ( -- xn .. x1 n ; R: x1 .. xn n -- )
   r> r> swap >r dup
   begin
      dup while
         r> r> swap >r -rot
         1-
   repeat
   drop ;

: on     true swap ! ;
: off   false swap ! ;

: bounds   ( start len -- start+len start )
   over + swap ;


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


\ variable dp
: here   ( -- addr )   dp @ ;
: dp!    ( addr -- )   dp ! ;

: align   ( -- )   here aligned dp! ;
: allot   ( n -- )   here + dp! ;

: ,    (    x -- )   here [ 1 cells ] literal allot  ! ;
: c,   ( char -- )   here [ 1 chars ] literal allot c! ;
: 2,   (    d -- )   here [ 2 cells ] literal allot 2! ;

: string,   ( c-addr u -- )   \ reserve space and store string
   here swap dup allot move ;

: code,   ( c-addr u -- )   \ store binary code
   here over 2>r  string,  2r> sync-i$ ;

: pad   ( -- c-addr )   here 128 + ;


32 constant bl

\ XXX: bare minimum for the case-insensitive ICOMPARE below
: isupper   ( char -- flag )   'A' [ 'Z' 1+ ] literal within ;
: tolower   ( char -- char )   dup isupper if $20 or then ;

: erase   ( c-addr u -- )   dup if  0 fill else 2drop then ;
: blank   ( c-addr u -- )   dup if bl fill else 2drop then ;

: count    ( c-addr1 -- c-addr2 u )   dup 1+    swap c@ ;
: $count   ( c-addr1 -- c-addr2 u )   dup cell+ swap  @ ;

\ store as counted (packed) string (from OpenBoot)
: pack   ( c-addr u pstr -- )
   over $ff u> ( result out of range ) -11 and throw
   over over c!   1+ swap move ;

: /string   ( c-addr1 u1 n -- c-addr2 u2 )   rot over +   -rot - ;

: cmove   ( src dst len -- )   \ left-to-right char-by-char
   ?dup if
      0 do
         over i + c@  over i + c!
      loop
   then 2drop ;

: cmove>   ( src dst len -- )   \ right-to-left char-by-char
   ?dup if
      ( len ) 1- 0 swap do
         over i + c@  over i + c!
      -1 +loop
   then 2drop ;

: -trailing   ( c-addr u1 -- c-addr u2 )
   dup if
      2dup over + 1-   \ str len str str+len-1
      do
         i c@ bl <> if
            drop              \ u1
            dup i 1+ swap -   \ u2 = s+1-str
            unloop exit
         then
      -1 +loop
      drop 0
   then ;

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
         2drop i /string true
         unloop exit
      then
   loop
   2drop false ;

\ like SEARCH but for single character
: search-char ( c-addr1 u1 char -- c-addr2 u2 flag )
   over 0= if
      drop false exit then
   over 0 do
      2 pick i + c@ over = if
	 drop i /string true
	 unloop exit
      then
   loop
   drop false ;


\ cword~ emit emit_impl
\ cword~ type type_impl

: cr      $0a emit ;
: space   bl emit ;
: spaces   ( n -- )   0 max 0 ?do space loop ;

variable base
: decimal   #10 base ! ;
: hex       #16 base ! ;

variable hld
: <#   ( -- )   pad hld ! ;
: hold   ( char -- )   -1 hld +! hld @ c! ;
: #>   ( xd -- c-addr u )   2drop hld @ pad over - ;

: sign   ( n -- )   0< if '-' hold then ;

: #   ( ud1 -- ud2 )   \ convert one digit
   base @ mu/mod rot   \ remainder
   dup 9 > if
      [ 'a' #10 - ] literal
   else
      '0'
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


\ DUMP - this naive version always rounds the range down and up to get
\ full lines.  The original start address is marked with a tick mark
\ in the dump header.

: .2   s>d <# # # #> type ;

: (dump-header)   ( addr -- )
   #10 spaces          \ address (8), colon and extra space
   $f and              \ last digit of addr, it will get the tick mark
   #16 0 do
      i 8 = negate 1+ spaces
      dup i = if '|' else bl then emit
      i 0 u.r
   loop drop
   3 spaces
   ." 0123456789abcdef"
   cr ;

: dump   ( addr length -- )
   base @ >r   hex                      \ switch to hex temporarily
   over (dump-header)
   over + #16 round-up                  \ end
   swap #16 round-down                  \ start
   do
      i 8 u.r ':' emit                  \ address
      i #16 bounds do                   \ bytes
         i $7 and 0= negate 1+ spaces
         i c@ .2
      loop
      ."   |"                           \ chars
      i #16 bounds do
         i c@   dup bl $7f within not   \ not printable?
         if drop '.' then emit
      loop
      '|' emit cr
   #16 +loop
   r> base ! ;                          \ restore base


\ TODO: use VALUE
variable (source-id)
2variable (source)
variable >in

: source-id   ( -- -1 | 0 | fileid )   (source-id) @ ;
: source   ( -- c-addr u )   (source) 2@ ;

: (>in-addr)   ( -- source-in )   source drop >in @ + ;
: (unparsed)   ( -- source-in len )   source >in @ /string ;

\ ... constant ib0
#4096 constant #ib-size
2variable (input)   \ same as (SOURCE) most of the time, except during EVALUATE

: reset-input-buffer   ib0 0 (input) 2! ;
: input-buffer   ( -- c-addr u )
   (input) 2@ drop  dup ib0 -  #ib-size swap - ;

: terminal-input
   reset-input-buffer  (input) 2@ (source) 2!
   (source-id) off  >in off ;

: string-input   ( c-addr u -- )
   (source) 2!  (source-id) on  >in off ;

: default-input   terminal-input ;

6 constant #save-input
: save-input   ( -- xn ... x1 n )
   (input) 2@  >in @  (source) 2@  (source-id) @  #save-input ;

: restore-input   ( xn ... x1 n -- flag )
   #save-input - ?dup if
      #save-input + 0 ?do drop loop
      true
   else
      (source-id) !  (source) 2!  >in !  (input) 2!
      false
   then ;

: refill
   source-id invert if
      input-buffer
      source-id if   \ file
         [ have-file [if] ]
            2 - source-id read-line   \ nread flag ior --
            0<> ( READ-LINE exception ) -71 and throw
            not             \ nread eof --
         [ [else] ]
            -71 throw   \ always throw READ-LINE exception
         [ [then] ]
      else   \ terminal
         accept dup 0<   \ nread eof --
      then
      if
         drop false   \ eof
      else   \ nread --
         input-buffer drop swap
         2dup (input) 2! (source) 2!
         >in off true
      then
   else
      \ source-id -1 - string input, can't be refilled
      drop false
   then ;

: skip-delim   ( char -- )
   (unparsed) bounds ?do
      dup i c@ = if >in 1+! else leave then
   loop drop ;

: parse   ( char "ccc<char>" -- c-addr u )
   (>in-addr) swap   \ stash result address (at current >IN)
   0                 \ init result length
   \ source-in delimiter len --
   (unparsed) bounds ?do
      >in 1+!
      over i c@ = if leave then   \ delimiter?
      1+     \ increment result length
   loop
   nip ;   \ delimiter

: \   $0a parse 2drop ; immediate
: (   ')' parse 2drop ; immediate

: .(   ')' parse type ; immediate

: parse-name   ( "name" -- c-addr u )
   bl skip-delim bl parse ;

: ?parsed   \ attempt to use zero-length string as a name
   dup 0= -16 and throw ;

: char   ( "name" -- char )
   parse-name if c@ else drop 0 then ;

: word   ( char "<chars>ccc<char>" -- c-addr )
   dup skip-delim parse   \ str len --
   dup >r
   here 1+ swap move   \ copy parsed string
   r@ here c!          \ set its count
   bl here 1+ r> + c!  \ space after string
   here ;


: char-literal?  ( str len -- str len 0 | n 1 )   \ Forth 2012 'c' literals
   dup 3 = if
      over   \ get a copy of "str" to the top
      dup c@ ''' = if
         dup 2 + c@ ''' = if
            1+ c@ dup bl > and   \ str len 0|c --
            dup if
               >r 2drop r>
               1 exit
            then
         then
      then
      drop
   then false ;

: base-0   ( base -- '0' upper )  \ range of arabic digits; for WITHIN
   dup #10 <= if
      '0' tuck +
   else
      drop '0' [ '9' 1+ ] literal
   then ;

: base-a   ( base  -- 'a' upper ) \ range of alphabetic digits; for WITHIN
   #10 - 'a' tuck + ;

: digit?   ( char base -- value true | false )
   swap >r   \ stash away the char
   \ numeric digit?
   dup base-0
   r@
   -rot within if
      drop r> '0' -
      true exit
   then
   \ does base use alpha digits?
   dup #10 > if
      \ alpha digit?
      dup base-a
      r> $20 or dup >r   \ downcase char
      -rot within if
         drop r> [ 'a' #10 - ] literal -
         true exit
      then
   then
   \ not a digit
   drop r> drop
   false ;

: >number   ( ud1 s1 l1 -- ud2 s2 l2 )
   2dup >r >r    \ stash away initial string
   bounds ?do    \ pointer loop over s1
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
: number?   ( str len -- 0 | n 1 | d 2 )
   char-literal? ?dup if exit then
   base @ >r    \ save base in case of sigil
   false >r     \ minus sign
   \ Forth 2012 #<dec>, $<hex>, %<bin> base sigils
   dup 1 > if
      over c@
      \ XXX: TODO: case
           dup '#' = if #10
      else dup '$' = if #16
      else dup '%' = if #2
      else               0
      then then then
      nip
      ?dup if
         base !
         1 /string
      then
   then
   \ minus sign
   dup 1 > if
      over c@ '-' = if
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
         over c@ '.' = if
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


\ ==================== Wordlists

$80 constant &iflag   \ immediate
$40 constant &sflag   \ smudged
\ $20 is unused
$1f constant #name    \ name length/mask

: latestwid   wid-list @ ;

: wordlist   ( -- wid )
   align here                   \ address of this stub header
   [ &sflag ] literal c,        \ Name Field: empty name, smudged
   align 0 ,                    \ Link Field: no link
   wid-list @ ,                 \ Code Field: abused for the wid list link
   here                         \ value of the wordlist - PFA
   dup wid-list !
   swap , ;                     \ Parameters Field: latest word = this

: context  osp @ ;

: osp-   context  dup order-stack = ( overflow  ) -49 and throw  cell- osp ! ;
: osp+   context         dup osp0 = ( udnerflow ) -50 and throw  cell+ osp ! ;

: (set-wid)  context ! ;   \ replace top value

\ : forth   forth-wordlist (set-wid) ;

: only   osp0 cell- osp !  forth ;
: also   context @  osp- (set-wid) ;
: previous   osp+ ;

: get-order   ( -- widn ... wid1 n )
   0  context osp0 cell- do
      1+  i @ swap
   [ 1 cells negate ] literal +loop ;

: set-order   ( widn ... wid1 n -- )
   dup -1 = if
      drop only
   else
      osp0 over cells -
      dup order-stack u< ( overflow ) -49 and throw
      osp !
      0 ?do
         osp @ i cells + !
      loop
   then ;

: set-current   current ! ;
: get-current   current @ ;

: ?current   ( wid -- flag )   0= ( deleted ) -47 and throw ;
: check-current   ( -- )   get-current ?current ;

: latest   get-current dup ?current @ ;
: definitions   context @ set-current ;


\ XXX: TODO: symbol var-does
\ where symbol is like constant with asm symbol as value
predef~ var-does var_does

variable latest-cfa
: latestxt   latest-cfa @ ;

: create
   check-current
   parse-name ?parsed
   dup #name > ( name too long ) -19 and throw
   align here >r   \ save NFA
   \ Name Field
   dup c, string, align
   \ Link Field
   latest ,
   r> get-current !
   \ Code Field
   here latest-cfa !
   \ XXX: we should comma var-does here, but need to teach transpiler
   \ about asm symbols (see above); for now abuse POSTPONE
   postpone var-does ;

: immediate  latest c@ [ &iflag ] literal or latest c! ;
: smudge     latest c@ [ &sflag ] literal or latest c! ;
: unsmudge   latest c@ [ &sflag invert ] literal and latest c! ;

\ XXX: TODO? when DOES> is implemented
\ : flag-test create, does> @ swap @ and ;
\ &iflag flag-test immediate?
\ &sflag flag-test smudged?
: immediate?   ( nfa -- flag )   c@ [ &iflag ] literal and ;
: smudged?     ( nfa -- flag )   c@ [ &sflag ] literal and ;

: >body   cell+ ;
: body>   cell- ;
: >link   cell- ;
: link>   cell+ ;

: >wid-link   body> ; \ wordlists abuse code field as a link to previous wid

: name-count   count #name and ;

: n>link   name-count + aligned ;
: name>    n>link link> ;

: >name   ( xt -- nfa )
   dup unaligned? if drop 0 exit then \ must be aligned
   >link        \ LFA is the aligned address after name
   dup          \ keep a copy for comparisons
   dup 32 -     \ limit - the farthest away that NFA can be
   swap cell- do        \ start one cell before LFA and step backwards
      \ The byte may have its upper bit set (potential immediate
      \ flag), since 8-bit characters are not allowed in a name.
      \ If the byte is less than 32, it's not a printable character
      \ and must be the name count byte.
      i c@ [ &iflag &sflag or #name or invert ] literal and 0= if
         \ looks like a count byte, but is the count right?
         dup i n>link = if
            drop i
            unloop exit
         then
      then
   [ -1 cells ] literal +loop
   drop 0 ;

: search-wordlist   ( c-addr u wid -- 0 | xt 1 | xt -1 )
   over 0= if drop 2drop false exit then   \ can't find empty string
   @ begin   \ ... nfa --
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

\ for FORGET and friends (CURRENT is not searched normally)
: search-current   ( c-addr u -- 0 | xt 1 | xt -1 )
   get-current if
      get-current search-wordlist
   else
      2drop 0
   then ;

: search-context   ( c-addr u -- 0 | xt 1 | xt -1 )
   osp0 context ?do
      2dup i @
      search-wordlist ?dup if
         2nip
         unloop exit
      then
   [ 1 cells ] literal +loop
   2drop false ;

: find   ( c-addr -- c-addr 0 | xt 1 | xt -1 )
   dup count search-context
   dup if rot drop then ;


\ helper for ' and the like that do the parse/search combo
: (')   ( "name" -- 0 | xt 1 | xt -1 )
   parse-name ?parsed search-context ;

: ?defined   0= ( undefined word ) -13 and throw ;

: '   (') ?defined ;


\ ====================

variable state
: [   state off ; immediate
: ]   state on ;

: ?comp   \ interpreting a compile-only word
   state @ 0= -14 and throw ;

: compile,   , ;
: compile
   r> dup cell+ >r
   @ compile, ;

: literal    state @ if postpone lit , then ; immediate
: 2literal   state @ if postpone 2lit , , then ; immediate
: [char]     ?comp char postpone literal ; immediate


: (c")   r> dup count + aligned >r ;
: (s")   r> $count 2dup + aligned >r ;

: (.")
   r> $count 2dup + aligned >r   \ XXX: (S") - a copy b/c of R>
   type ;


: [']   ?comp ' postpone literal ; immediate

: [compile]   ?comp ' compile, ; immediate

: postpone
   ?comp
   (') dup ?defined
   1+ if   \ immediate
      compile,
   else
      compile compile ,
   then ; immediate


\ ==================== repl

: interpret
   begin
      parse-name ?dup 0= if drop exit then
      2dup search-context ?dup if
         2nip
         1+ if
            execute
         else
            state @ if compile, else execute then
         then
      else
         2dup number? ?dup if
            2 = if
               2nip
               postpone 2literal
            else
               -rot 2drop
               postpone literal
            then
         else
            type ."  ?" cr  false ?defined
         then
      then
   again ;

: bye   (bye) ;


variable abort-message

predef~ throw-msgtab throw_msgtab

: throw-message   ( code -- flag )
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
         $count type cr
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

: interpret-loop
   begin
      refill while
         interpret
   repeat ;

: quit
   (quit)
   default-input
   decimal
   postpone [
   begin
      source-id 0= if
         state @ 0= if ." ok " then
      then
      refill while
         ['] interpret catch
         ?dup if
            postpone [ \ if we failed while compiling, abort compilation
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


: evaluate   ( i*x c-addr u -- j*x )
   save-input n>r
   string-input
   ['] interpret catch
   nr> restore-input abort" RESTORE-INPUT failed"
   throw ;

\ top-level non-interactive evaluate+bye
: eval1   ( i*x c-addr u -- j*x \ input stack set up by the caller trampoline )
   (quit)   \ reset return stack
   decimal
   postpone [
   \ ." EVAL: " 2dup type cr
   ['] evaluate catch ?dup if
      report-exception clear
   then
   bye ;


\ ==================== defining words &co

: variable    create [ 1 cells ] literal allot ;
: 2variable   create [ 2 cells ] literal allot ;
: buffer:     create allot ;

\ set CFA of the latest word to ...
: (;code)   r>   latest-cfa @ ! ; \ the asm code after this word
: (;alit)   r> @ latest-cfa @ ! ; \ the address compiled after this word

: constant    create  , does>  @ ;
: 2constant   create 2, does> 2@ ;

: vocabulary
   \ NB: WORDLIST allots, so can't just use  CREATE WORDLIST ,
   \ see also .WID below for the name heuristic
   create  here [ 1 cells ] literal allot
   wordlist swap !
 does>
   @ (set-wid) ;

: .wid   ( wid -- )   \ try to print wid by its vocabulary name
   dup body> >name                      \ wid wid-header --
   cell- @ over = if                    \ right after VOCABULARY body
      body> >name                       \ wid-header
      cell- body> >name                 \ vocabulary's nfa
      name-count type space
   else
      .
   then ;

: order   ( -- )   \ XXX: TODO: try to identify vocabularies
   get-order 0 ?do .wid loop
   5 spaces get-current .wid ;


predef~ call-code call_code \ XXX

: :
   create smudge ]
   (;alit) call-code ;

: ;
   postpone exit
   unsmudge postpone [ ; immediate

: does> ?comp
   postpone (;code)
   does-thunk $count \ code len -- (like sliteral)
   code, ; immediate


: :noname
   \ Name Field: empty name, smudged
   align [ &sflag ] literal c,
   \ Link Field: no link
   align 0 ,
   \ Code Field
   here   \ xt we leave on the stack
   dup latest-cfa !
   \ XXX: we should comma call-code here (cf. CREATE)
   postpone call-code ] ;

: recurse   latestxt compile, ; immediate


: defer   ( "name" -- )
   create ['] abort ,
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
   '"' parse
   rot compile,        \ compile runtime word
   dup , string,       \ compile string
   align ;

: s"   ?comp compile" (s") ; immediate
: ."   ?comp compile" (.") ; immediate

: sliteral   ( c-addr u -- ;  run-time -- c-addr u )
   ?comp
   postpone (s")
   dup , string, align ; immediate

: c"
   ?comp
   '"' parse
   dup $ff > ( string overflow ) -18 and throw
   postpone (c")
   dup c, string,
   align ; immediate

: (abort")
   if
      r@ abort-message !
      -2 throw
   else
      \ skip the abort message after us; cf. (s")
      r> $count + aligned >r
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
: (ahead)   postpone branch >mark ;
: (if)      postpone ?branch >mark ;
: (then)    >resolve ;

\ backward jumps
: (begin)   <mark ;
: (again)   postpone  branch <resolve ;
: (until)   postpone ?branch <resolve ;


: ?pairs   ( C: expected actual -- )   \ control structure mismatch
   <> -22 and throw ;

: ahead   ?comp          (ahead) 1 ; immediate
: if      ?comp          (if) 1 ; immediate
: else    ?comp 1 ?pairs (ahead) swap (then) 1 ; immediate
: then    ?comp 1 ?pairs (then) ; immediate

: begin  ?comp          (begin) 2 ; immediate
: again  ?comp 2 ?pairs (again) ; immediate
: until  ?comp 2 ?pairs (until) ; immediate

: while  ?comp 2 ?pairs (if) 3 ; immediate
: repeat ?comp 3 ?pairs swap (again) (then) ; immediate

: do
   ?comp
   postpone (do)
   >mark \ leave address
   <mark \ address to loop back to
   4 ; immediate

: ?do
   ?comp
   postpone (?do)
   >mark \ leave address
   <mark \ address to loop back to
   4 ; immediate

: loop
   ?comp 4 ?pairs
   postpone (loop)
   <resolve     \ jump to the beginning of the loop
   >resolve ; immediate \ leave address after the loop

: +loop
   ?comp 4 ?pairs
   postpone (+loop)
   <resolve     \ jump to the beginning of the loop
   >resolve ; immediate \ leave address after the loop

\ XXX: stub for now
: environment?   2drop false ;

\ file operations are not available when embedded in the kernel
have-file [if] include file.fth [then]

include forget.fth
include smartif.fth
