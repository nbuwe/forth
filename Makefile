# $Id$

PROG=	ftest
NOMAN=

DBG=	-g
SRCS=	main.c cpu_forth.S

# use traditional cpp for asm, since sh3 uses # for immediates
CPPFLAGS.cpu_forth.S = --traditional-cpp

cpu_forth.o: cpu_forth.S forth.S test.S

.include <bsd.prog.mk>
