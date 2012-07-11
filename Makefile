# $Id$

PROG=	ftest
NOMAN=

DBG=	-g
SRCS=	main.c forth.S

# use traditional cpp for asm, since sh3 uses # for immediates
CPPFLAGS.forth.S = --traditional-cpp

.include <bsd.prog.mk>
