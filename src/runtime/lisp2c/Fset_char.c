/* This file was generated by CLiCC 0.6.4 [obrep 1] */
#define __OBREP 1
#include <c_decl.h>
#include "sys.h"
#include "lisp.h"

void Fset_char(CL_FORM *base)
{
	COPY(ARG(1), ARG(3));
	Fstringp(ARG(3));
	if(CL_TRUEP(ARG(3)))
	{
	}
	else
	{
		COPY(SYMVAL(Slisp, 58), ARG(3));	/* WRONG_TYPE */
		COPY(ARG(1), ARG(4));
		LOAD_SYMBOL(SYMBOL(Slisp, 44), ARG(5));	/* STRING */
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
	Fset_row_major_aref(ARG(0));
}
