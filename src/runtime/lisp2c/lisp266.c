/* This file was generated by CLiCC 0.6.4 [obrep 1] */
#define __OBREP 1
#include <c_decl.h>
#include "sys.h"
#include "lisp.h"

void Fgraphic_char_p(CL_FORM *base)
{
	if(CL_CHARP(ARG(0)))
	{
		COPY(ARG(0), ARG(1));
	}
	else
	{
		COPY(SYMVAL(Slisp, 58), ARG(1));	/* WRONG_TYPE */
		COPY(ARG(0), ARG(2));
		LOAD_SYMBOL(SYMBOL(Slisp, 18), ARG(3));	/* CHARACTER */
		Ferror(ARG(1), 3);
	}
	rt_graphic_char_p(ARG(1));
	COPY(ARG(1), ARG(0));
}
