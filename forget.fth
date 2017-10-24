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

: forget?     ( nfa x -- flag )   u<= ;
: remember?   ( nfa x -- flag )   u> ;

: get-unforgotten-order   ( nfa --- widn ... wid1 n )
   0  context osp0 cell- do     \ ... widm nfa m --
      over i @ remember? if
         1+  i @ -rot
      then
   [ 1 cells negate ] literal +loop
   nip ;

: prune-order   ( nfa -- )
   get-unforgotten-order
   ?dup 0= if
      ." resetting forgotten search order" cr
      -1
   then
   set-order ;

: gc-forgotten-order   ( nfa -- )
   dup prune-order
   current @ forget? if  0 set-current then ;


: wid-list-trim  ( nfa -- )
   latestwid begin                      \ nfa wid --
      2dup forget? while
         >wid-link @                    \ nfa prev-wid --
         dup wid-list !
   repeat
   2drop ;

: wid-trim   ( nfa wid -- )
   tuck @                               \ wid nfa latest --
   begin
      2dup forget? while
         n>link @
   repeat
   rot !  drop ;

: ?forgettable   ( nfa -- )
   fence @ here within not
   ( invalid forget ) -15 and throw ;

: (forget)   ( nfa -- )
   dup ?forgettable
   dup gc-forgotten-order
   dup wid-list-trim
   latestwid begin                      \ nfa wid --
      dup while
         2dup wid-trim
         >wid-link @
   repeat
   drop dp! ;

: (forget-search)   ( c-addr u -- xt )
   2dup search-current ?dup if
      2nip
   else
      search-context
   then ;

: forget   ( "name" -- )
   parse-word ?parsed                   \ c-addr u --
   (forget-search)  0= ( undefined ) -13 and throw
   >name (forget) ;

: marker   ( "name" -- )
   : latest postpone literal postpone (forget) postpone ; ;
