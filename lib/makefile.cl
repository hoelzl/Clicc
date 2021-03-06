#------------------------------------------------------------------------------
# Projekt : CLICC - a Common Lisp to C Compiler
#           -----------------------------------
# Function : Generates executable file from C file wich was generated by clicc.
#            Needs parameter SOURCE = <name> and
#            optionally OPTIMIZE = <-{O | g}>.
#            Uses Environment variable CLICCROOT.
#            Called by clicc/bin/cl and clicc/bin/clg.
#
#            Dependencies:
#            start -> rtl(startup.o) -> program -> rtl -> rtc
#
# $Source: /home3/apply/public/clicc-0.6.4/lib/RCS/makefile.cl,v $
# $Author: hk $
# $Revision: 1.13 $
# $Date: 1993/11/18 16:00:56 $
#------------------------------------------------------------------------------

MODULES =
LIBRARIES =
OPTIMIZE = -O # alternative to -O is -g
CFLAGS	= $(OPTIONS) -I$(CLICCROOT)/lib $(OPTIMIZE)
CC	= acc

ifeq ($(findstring -g,$(OPTIMIZE)), -g)
 RTC	= $(CLICCROOT)/lib/rtc-g.a
 RTL	= $(CLICCROOT)/lib/rtl-g.a
 START	= $(CLICCROOT)/lib/main-g.o
else
 ifeq ($(findstring -pg,$(OPTIMIZE)), -pg)
  RTC	= $(CLICCROOT)/lib/rtc-p.a
  RTL	= $(CLICCROOT)/lib/rtl-p.a
  START	= $(CLICCROOT)/lib/main-p.o
 else
  RTC	= $(CLICCROOT)/lib/rtc.a
  RTL	= $(CLICCROOT)/lib/rtl.a
  START	= $(CLICCROOT)/lib/main.o
 endif
endif

OMODULES = $(MODULES:%.c=%.o)
OBJECT = $(SOURCE:%.c=%.o)
PROGRAM= $(SOURCE:%.c=%)

$(PROGRAM): $(OBJECT) $(START) $(RTC) $(RTL) $(OMODULES)
	$(LINK.c) -o $@ $(START) $(OBJECT) $(OMODULES) \
	$(RTL) $(RTC) $(RTL) $(LIBRARIES) -lm


