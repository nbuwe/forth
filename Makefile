PROG=	forth
NOMAN=

.include <bsd.own.mk>

DBG = -g
CPPFLAGS += -I${.CURDIR} -I${.CURDIR}/${MACHINE_CPU}

SRCS = main.c forth_machdep.S
GDBINIT = ${.CURDIR}/${MACHINE_CPU}/forth.gdb

# use traditional cpp for asm, since sh3 uses # for immediates
CPPFLAGS.forth_machdep.S = --traditional-cpp

# XXX: there must be a more sane way to do this, but my make-fu is weak...
forth_machdep.d: ${MACHINE_CPU}/forth_machdep.S
forth_machdep.o: ${MACHINE_CPU}/forth_machdep.S

.include <bsd.prog.mk>
