/* This file was generated by CLiCC 0.6.4 [obrep 1] */
#define __OBREP 1
#include <c_decl.h>
#include "sys.h"
#include "lisp.h"

void Ppackage_internal(CL_FORM *base)
{
	COPY(ARG(0), ARG(1));
	LOAD_SYMBOL(SYMBOL(Slisp, 354), ARG(2));	/* PACKAGE */
	rt_struct_typep(ARG(1));
	if(CL_TRUEP(ARG(1)))
	{
		COPY(OFFSET(AR_BASE(GET_FORM(ARG(0))), 2 + 1), ARG(0));
	}
	else
	{
		COPY(SYMVAL(Slisp, 352), ARG(1));	/* NO_STRUCT */
		COPY(ARG(0), ARG(2));
		LOAD_SYMBOL(SYMBOL(Slisp, 354), ARG(3));	/* PACKAGE */
		Ferror(ARG(1), 3);
	}
}