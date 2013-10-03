#------------------------------------------------------------------------------
# Project  : APPLY - A Practicable And Portable Lisp Implementation
#            ------------------------------------------------------
# Function : Generates C runtime-modules for CLICC.
#            Uses optional parameter OPTIMIZE = <-{O | g}>.
#            Uses Environment variable CLICCROOT.
#
# $Revision: 1.6 $
# $Source: /home3/apply/public/clicc-0.6.4/src/runtime/c/RCS/Makefile,v $
# $Author: uho $
# $Date: 1993/06/04 16:50:19 $
#------------------------------------------------------------------------------

CC	= acc
OPTIMIZE = -O # alternative to -O is -g
CFLAGS	= -I$(CLICCROOT)/lib $(OPTIMIZE)
ARFLAGS	= r

SOURCES:sh = echo *.c
#OBJECTS	= $(SOURCES:.c=.o)

LIBRARY	= $(CLICCROOT)/lib/rtc.a
MAIN.O 	= $(CLICCROOT)/lib/main.o
INITIALIZE.O = $(CLICCROOT)/lib/initialize.o

.KEEP_STATE:
.PRECIOUS: $(LIBRARY)

all: $(LIBRARY) $(MAIN.O) $(INITIALIZE.O)

#$(LIBRARY): $(LIBRARY)($(OBJECTS))
#	ranlib $@

$(LIBRARY): $(SOURCES)
	$(COMPILE.c) $?
	$(AR) $(ARFLAGS) $@ $(?:.c=.o)
	?rm $(?:.c=.o)
	?ranlib $@

$(MAIN.O): $(MAIN.C)
	$(COMPILE.c) -o $@ $<

$(INITIALIZE.O): $(MAIN.C)
	$(COMPILE.c) -DNOMAIN -o $@ $<

