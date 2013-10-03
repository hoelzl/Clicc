#------------------------------------------------------------------------------
#  Projekt : CLICC - a Common Lisp to C Compiler
#            -----------------------------------
# Function : Generates Lisp runtime-modules for CLICC.
#            Uses optional parameter OPTIMIZE = <-{O | g}>.
#            Uses environment variable CLICCROOT.
#
#  $Source: /home3/apply/public/clicc-0.6.4/src/runtime/lisp2c/RCS/GNUmakefile,v $
#  $Author: uho $
#  $Revision: 1.9 $
#  $Date: 1994/02/18 12:11:46 $
#------------------------------------------------------------------------------

CC	= acc
LIB	= $(CLICCROOT)/lib
OPTIMIZE = -g # alternative to -O is -g
CFLAGS	= -I$(LIB) $(OPTIMIZE)
ARFLAGS	= r
RANLIB  = ranlib
RM      = rm

SOURCES 	= $(wildcard *.c)
SYNTAX		= $(LIB)/lisp.syntax
DEF		= $(LIB)/lisp.def
LISPH		= $(LIB)/lisp.h
INLINEH		= $(LIB)/inline.h

ifeq ($(findstring -g,$(OPTIMIZE)), -g)
 LIBRARY	= $(LIB)/rtl-g.a
else
 ifeq ($(findstring -pg,$(OPTIMIZE)), -pg)
 LIBRARY	= $(LIB)/rtl-p.a
 else
  LIBRARY	= $(LIB)/rtl.a
 endif
endif

.PRECIOUS: $(LIBRARY)

all:  $(LISPH) $(INLINEH) $(LIBRARY) $(SYNTAX) $(DEF)

$(LISPH): lisp.h	
	echo '#include "inline.h"' > lisp.hh
	cat lisp.h >> lisp.hh
	mv lisp.hh lisp.h
	cp lisp.h $(LISPH)

$(INLINEH): inline.h
	cp inline.h $(INLINEH)

$(SYNTAX): lisp.syntax
	cp lisp.syntax $(SYNTAX)

$(DEF): lisp.def
	cp lisp.def $(DEF)

$(LIBRARY): $(SOURCES)
	-$(COMPILE.c) $?
	$(AR) $(ARFLAGS) $@ *.o
	$(RM) *.o
	$(RANLIB) $@