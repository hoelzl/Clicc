/* This file was generated by CLiCC 0.6.4 [obrep 1] */
#define __OBREP 1
#include <c_decl.h>
#include "sys.h"
#include "lisp.h"

GEN_GLOBAL_FUNARG(CFcar, Fcar, 1);

void Fcar(CL_FORM *base)
{
	if(CL_CONSP(ARG(0)))
	{
		COPY(GET_CAR(ARG(0)), ARG(0));
	}
	else
	{
		if(CL_TRUEP(ARG(0)))
		{
			LOAD_SMSTR((CL_FORM *)&KClisp[264], ARG(1));	/* ~a is not a list */
			COPY(ARG(0), ARG(2));
			Ferror(ARG(1), 2);
		}
		else
		{
		}
	}
}