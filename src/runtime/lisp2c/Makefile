#------------------------------------------------------------------------------
#  Projekt : CLICC - a Common Lisp to C Compiler
#            -----------------------------------
# Function : Generates Lisp runtime-modules for CLICC.
#            Uses optional parameter OPTIMIZE = <-{O | g}>.
#            Uses environment variable CLICCROOT.
#
#  $Source: /home3/apply/public/clicc-0.6.4/src/runtime/lisp2c/RCS/Makefile,v $
#  $Author: hk $
#  $Revision: 1.2 $
#  $Date: 1993/04/22 10:18:45 $
#------------------------------------------------------------------------------

CC	= acc
LIB	= $(CLICCROOT)/lib
OPTIMIZE = -O # alternative to -O is -g
CFLAGS	=  -I$(LIB) $(OPTIMIZE)
ARFLAGS	= r


SOURCES:sh = echo *.c
#OBJECTS	= $(SOURCES:.c=.o)

LIBRARY	= $(LIB)/rtl.a

.KEEP_STATE:
.PRECIOUS: $(LIBRARY)

all: $(LIBRARY)

#$(LIBRARY): $(LIBRARY)($(OBJECTS))
#	ranlib $@

$(LIBRARY): $(SOURCES)
	echo '#include "inline.h"' > lisp.hh
	cat lisp.h >> lisp.hh
	mv lisp.hh lisp.h
	-$(COMPILE.c) $?
	$(AR) $(ARFLAGS) $@ $(?:.c=.o)
	?rm $(?:.c=.o)
	ranlib $@
	cp lisp.syntax $(LIB)
	cp inline.h lisp.h $(LIB)
