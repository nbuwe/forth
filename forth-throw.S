/*
 * Copyright (c) 2017 Valery Ushakov
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

CONSTANT("throw-msgtab", throw_msgtab)
	.long	0f
	/* XXX: can move to rodata */
0:	.long	(9f - 0b) / (1f - 0b)
1:	                  .long        0;    .long        0;  .long .Lthrmsg3;
	.long .Lthrmsg4;  .long .Lthrmsg5;  .long .Lthrmsg6;  .long .Lthrmsg7;
	.long .Lthrmsg8;  .long .Lthrmsg9;  .long .Lthrmsg10; .long .Lthrmsg11;
	.long .Lthrmsg12; .long .Lthrmsg13; .long .Lthrmsg14; .long .Lthrmsg15;
	.long .Lthrmsg16; .long .Lthrmsg17; .long .Lthrmsg18; .long .Lthrmsg19;
	.long .Lthrmsg20; .long .Lthrmsg21; .long .Lthrmsg22; .long .Lthrmsg23;
	.long .Lthrmsg24; .long .Lthrmsg25; .long .Lthrmsg26; .long .Lthrmsg27;
	.long .Lthrmsg28; .long .Lthrmsg29; .long .Lthrmsg30; .long .Lthrmsg31;
	.long .Lthrmsg32; .long .Lthrmsg33; .long .Lthrmsg34; .long .Lthrmsg35;
	.long .Lthrmsg36; .long .Lthrmsg37; .long .Lthrmsg38; .long .Lthrmsg39;
	.long .Lthrmsg40; .long .Lthrmsg41; .long .Lthrmsg42; .long .Lthrmsg43;
	.long .Lthrmsg44; .long .Lthrmsg45; .long .Lthrmsg46; .long .Lthrmsg47;
	.long .Lthrmsg48; .long .Lthrmsg49; .long .Lthrmsg50; .long .Lthrmsg51;
	.long .Lthrmsg52; .long .Lthrmsg53; .long .Lthrmsg54; .long .Lthrmsg55;
	.long .Lthrmsg56; .long .Lthrmsg57; .long .Lthrmsg58; .long .Lthrmsg59;
	.long .Lthrmsg60; .long .Lthrmsg61; .long .Lthrmsg62; .long .Lthrmsg63;
	.long .Lthrmsg64; .long .Lthrmsg65; .long .Lthrmsg66; .long .Lthrmsg67;
	.long .Lthrmsg68; .long .Lthrmsg69; .long .Lthrmsg70; .long .Lthrmsg71;
	.long .Lthrmsg72; .long .Lthrmsg73; .long .Lthrmsg74; .long .Lthrmsg75;
	.long .Lthrmsg76; .long .Lthrmsg77; .long .Lthrmsg78; .long .Lthrmsg79;
9:	/* end of table */

#define	THRMSG(code, message)	\
.Lthrmsg##code:		\
	.byte	  (9f - 1f);	\
1:	.ascii	  message;	\
9:	.p2align  2, 0

/*	THRMSG(1, "ABORT")	*/
/*	THRMSG(2, "ABORT\"")	*/
	THRMSG(3, "stack overflow")
	THRMSG(4, "stack underflow")
	THRMSG(5, "return stack overflow")
	THRMSG(6, "return stack underflow")
	THRMSG(7, "do-loops nested too deeply during execution")
	THRMSG(8, "dictionary overflow")
	THRMSG(9, "invalid memory address")
	THRMSG(10, "division by zero")
	THRMSG(11, "result out of range")
	THRMSG(12, "argument type mismatch")
	THRMSG(13, "undefined word")
	THRMSG(14, "interpreting a compile-only word")
	THRMSG(15, "invalid FORGET")
	THRMSG(16, "attempt to use zero-length string as a name")
	THRMSG(17, "pictured numeric output string overflow")
	THRMSG(18, "parsed string overflow")
	THRMSG(19, "definition name too long")
	THRMSG(20, "write to a read-only location")
	THRMSG(21, "unsupported operation")
	THRMSG(22, "control structure mismatch")
	THRMSG(23, "address alignment exception")
	THRMSG(24, "invalid numeric argument")
	THRMSG(25, "return stack imbalance")
	THRMSG(26, "loop parameters unavailable")
	THRMSG(27, "invalid recursion")
	THRMSG(28, "user interrupt")
	THRMSG(29, "compiler nesting")
	THRMSG(30, "obsolescent feature")
	THRMSG(31, ">BODY used on non-CREATEd definition")
	THRMSG(32, "invalid name argument")
	THRMSG(33, "block read exception")
	THRMSG(34, "block write exception")
	THRMSG(35, "invalid block number")
	THRMSG(36, "invalid file position")
	THRMSG(37, "file I/O exception")
	THRMSG(38, "non-existent file")
	THRMSG(39, "unexpected end of file")
	THRMSG(40, "invalid BASE for floating point conversion")
	THRMSG(41, "loss of precision")
	THRMSG(42, "floating-point divide by zero")
	THRMSG(43, "floating-point result out of range")
	THRMSG(44, "floating-point stack overflow")
	THRMSG(45, "floating-point stack underflow")
	THRMSG(46, "floating-point invalid argument")
	THRMSG(47, "compilation word list deleted")
	THRMSG(48, "invalid POSTPONE")
	THRMSG(49, "search-order overflow")
	THRMSG(50, "search-order underflow")
	THRMSG(51, "compilation word list changed")
	THRMSG(52, "control-flow stack overflow")
	THRMSG(53, "exception stack overflow")
	THRMSG(54, "floating-point underflow")
	THRMSG(55, "floating-point unidentified fault")
	THRMSG(56, "QUIT exception")
	THRMSG(57, "exception in sending or receiving a character")
	THRMSG(58, "[IF], [ELSE], or [THEN] exception")
	THRMSG(59, "ALLOCATE exception")
	THRMSG(60, "FREE exception")
	THRMSG(61, "RESIZE exception")
	THRMSG(62, "CLOSE-FILE exception")
	THRMSG(63, "CREATE-FILE exception")
	THRMSG(64, "DELETE-FILE exception")
	THRMSG(65, "FILE-POSITION exception")
	THRMSG(66, "FILE-SIZE exception")
	THRMSG(67, "FILE-STATUS exception")
	THRMSG(68, "FLUSH-FILE exception")
	THRMSG(69, "OPEN-FILE exception")
	THRMSG(70, "READ-FILE exception")
	THRMSG(71, "READ-LINE exception")
	THRMSG(72, "RENAME-FILE exception")
	THRMSG(73, "REPOSITION-FILE exception")
	THRMSG(74, "RESIZE-FILE exception")
	THRMSG(75, "WRITE-FILE exception")
	THRMSG(76, "WRITE-LINE exception")
	THRMSG(77, "Malformed xchar")
	THRMSG(78, "SUBSTITUTE exception")
	THRMSG(79, "REPLACES exception")
#undef	THRMSG
