/* This file was generated by CLiCC 0.6.4 [obrep 1] */
#define __OBREP 1
#include <c_decl.h>
#include "sys.h"
#include "lisp.h"

void Fendp(CL_FORM *base)
{
	if(CL_TRUEP(ARG(0)))
	{
		if(CL_CONSP(ARG(0)))
		{
			LOAD_NIL(ARG(0));
		}
		else
		{
			LOAD_SMSTR((CL_FORM *)&KClisp[228], ARG(1));	/* ~a is not a list */
			COPY(ARG(0), ARG(2));
			Ferror(ARG(1), 2);
		}
	}
	else
	{
		LOAD_SYMBOL(SYMBOL(Slisp, 48), ARG(0));	/* T */
	}
}
