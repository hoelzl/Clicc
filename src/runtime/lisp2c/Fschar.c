/* This file was generated by CLiCC 0.6.4 [obrep 1] */
#define __OBREP 1
#include <c_decl.h>
#include "sys.h"
#include "lisp.h"

void Fschar(CL_FORM *base)
{
	if(CL_SMSTRP(ARG(0)))
	{
	}
	else
	{
		COPY(SYMVAL(Slisp, 58), ARG(2));	/* WRONG_TYPE */
		COPY(ARG(0), ARG(3));
		LOAD_SYMBOL(SYMBOL(Slisp, 40), ARG(4));	/* SIMPLE-STRING */
		Ferror(ARG(2), 3);
	}
	pvref(ARG(0));
}