/* This file was generated by CLiCC 0.6.4 [obrep 1] */
#define __OBREP 1
#include <c_decl.h>
#include "sys.h"
#include "lisp.h"

void Fnot(CL_FORM *base)
{
	if(CL_TRUEP(ARG(0)))
	{
		LOAD_NIL(ARG(0));
	}
	else
	{
		LOAD_SYMBOL(SYMBOL(Slisp, 48), ARG(0));	/* T */
	}
}
