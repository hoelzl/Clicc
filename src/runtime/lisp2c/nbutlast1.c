/* This file was generated by CLiCC 0.6.4 [obrep 1] */
#define __OBREP 1
#include <c_decl.h>
#include "sys.h"
#include "lisp.h"

void nbutlast1(CL_FORM *base)
{
	COPY(ARG(0), ARG(2));
	Flength(ARG(2));
	COPY(ARG(1), ARG(3));
	Fminus(ARG(2), 2);
	LOAD_NIL(ARG(3));
	COPY(ARG(2), ARG(4));
	LOAD_FIXNUM(ARG(5), 0, ARG(5));
	Fle(ARG(4), 2);
	if(CL_TRUEP(ARG(4)))
	{
		LOAD_NIL(ARG(0));
	}
	else
	{
		COPY(ARG(2), ARG(3));
		F1minus(ARG(3));
		COPY(ARG(0), ARG(4));
		Fnthcdr(ARG(3));
		if(CL_CONSP(ARG(3)))
		{
			LOAD_NIL(ARG(5));
			COPY(ARG(5), GET_CDR(ARG(3)));
		}
		else
		{
			LOAD_SMSTR((CL_FORM *)&KClisp[258], ARG(4));	/* ~a is not a cons */
			COPY(ARG(3), ARG(5));
			Ferror(ARG(4), 2);
		}
	}
}