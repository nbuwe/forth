PROG = forth
PAXCTL_FLAGS = +m

NOPIE=
NOMAN=
NOCTF=

.include <bsd.own.mk>

DBG = -g
CPPFLAGS += -I. -I${.CURDIR} -I${.CURDIR}/${MACHINE_CPU}

# Enable this workaround if you get this error on older NetBSD/powerpc
# "Unsupported relocation type 6 in non-PLT relocations"
# CPPFLAGS.forth_machdep.S += -DPPC_FORCE_RELOCS

SRCS = main.c forth_machdep.S
SRCS += math.c
SRCS += io-stdio.c
SRCS += file.c
GDBINIT = ${.CURDIR}/${MACHINE_CPU}/forth.gdb

# when porting to a new cpu - see test.S
# CPPFLAGS += -DTESTING

CLEANDIRFILES += forth.S
forth.S: forth.fth file.fth forget.fth smartif.fth asmwords.fth trans.fth
	gforth ${.CURDIR}/trans.fth -e bye > forth.S

# XXX: there must be a more sane way to do this, but my make-fu is weak...
forth_machdep.d: ${MACHINE_CPU}/forth_machdep.S forth.S

OBJMACHINE=yes
.include <bsd.prog.mk>

