/* This file was generated by CLiCC 0.6.4 [obrep 1] */
#define __OBREP 1
#include <c_decl.h>
#include "sys.h"
#include "lisp.h"

void Fset_schar(CL_FORM *base)
{
	if(CL_SMSTRP(ARG(1)))
	{
	}
	else
	{
		COPY(SYMVAL(Slisp, 58), ARG(3));	/* WRONG_TYPE */
		COPY(ARG(1), ARG(4));
		LOAD_SYMBOL(SYMBOL(Slisp, 40), ARG(5));	/* SIMPLE-STRING */
		Ferror(ARG(3), 3);
	}
	if(CL_CHARP(ARG(0)))
	{
	}
	else
	{
		COPY(SYMVAL(Slisp, 58), ARG(3));	/* WRONG_TYPE */
		COPY(ARG(0), ARG(4));
		LOAD_SYMBOL(SYMBOL(Slisp, 18), ARG(5));	/* CHARACTER */
		Ferror(ARG(3), 3);
	}
	set_pvref(ARG(0));
}