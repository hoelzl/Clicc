/* This file was generated by CLiCC 0.6.4 [obrep 1] */
#define __OBREP 1
#include <c_decl.h>
#include "sys.h"
#include "lisp.h"

void Ftailp(CL_FORM *base)
{
	M1_1:;
	if(EQL(ARG(0), ARG(1)))
	{
		LOAD_SYMBOL(SYMBOL(Slisp, 48), ARG(0));	/* T */
	}
	else
	{
		if(CL_ATOMP(ARG(1)))
		{
			LOAD_NIL(ARG(0));
		}
		else
		{
			COPY(GET_CDR(ARG(1)), ARG(1));
			goto M1_1;
		}
	}
	goto RETURN1;
	RETURN1:;
}
