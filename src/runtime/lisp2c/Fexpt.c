/* This file was generated by CLiCC 0.6.4 [obrep 1] */
#define __OBREP 1
#include <c_decl.h>
#include "sys.h"
#include "lisp.h"

void Fexpt(CL_FORM *base)
{
	COPY(ARG(1), ARG(2));
	Fminusp(ARG(2));
	if(CL_TRUEP(ARG(2)))
	{
		LOAD_FIXNUM(ARG(2), 1, ARG(2));
		COPY(ARG(0), ARG(3));
		COPY(ARG(1), ARG(4));
		Fminus(ARG(4), 1);
		Fexpt(ARG(3));
		Fdiv(ARG(2), 2);
		COPY(ARG(2), ARG(0));
	}
	else
	{
		rt_expt(ARG(0));
	}
}
