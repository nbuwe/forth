PROG=	forth
NOPIE=
NOMAN=

.include <bsd.own.mk>

DBG = -g
CPPFLAGS += -I. -I${.CURDIR} -I${.CURDIR}/${MACHINE_CPU}

SRCS = main.c forth_machdep.S
GDBINIT = ${.CURDIR}/${MACHINE_CPU}/forth.gdb

# use traditional cpp for asm, since sh3 uses # for immediates
CPPFLAGS.forth_machdep.S = --traditional-cpp

forth.S: forth.fth asmwords.fth trans.fth
	gforth ${.CURDIR}/trans.fth -e bye > forth.S

# XXX: there must be a more sane way to do this, but my make-fu is weak...
forth_machdep.d: ${MACHINE_CPU}/forth_machdep.S forth.S
forth_machdep.o: ${MACHINE_CPU}/forth_machdep.S forth.S

OBJMACHINE=yes
.include <bsd.prog.mk>
