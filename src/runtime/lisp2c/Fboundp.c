/* This file was generated by CLiCC 0.6.4 [obrep 1] */
#define __OBREP 1
#include <c_decl.h>
#include "sys.h"
#include "lisp.h"

void Fboundp(CL_FORM *base)
{
	if(CL_SYMBOLP(ARG(0)))
	{
		COPY(SYM_VALUE(ARG(0)), ARG(1));
		LOAD_BOOL(CL_UNBOUNDP(ARG(1)), ARG(1));
		if(CL_TRUEP(ARG(1)))
		{
			LOAD_NIL(ARG(0));
		}
		else
		{
			LOAD_SYMBOL(SYMBOL(Slisp, 48), ARG(0));	/* T */
		}
	}
	else
	{
		if(CL_TRUEP(ARG(0)))
		{
			COPY(SYMVAL(Slisp, 676), ARG(1));	/* SYM_EXPECTED */
			COPY(ARG(0), ARG(2));
			Ferror(ARG(1), 2);
		}
		else
		{
			LOAD_SYMBOL(SYMBOL(Slisp, 48), ARG(0));	/* T */
		}
	}
}
