/* This file was generated by CLiCC 0.6.4 [obrep 1] */
#define __OBREP 1
#include <c_decl.h>
#include "sys.h"
#include "lisp.h"

void FFI_make_c_string(CL_FORM *base)
{
	COPY(ARG(0), ARG(1));
	Fstringp(ARG(1));
	if(CL_TRUEP(ARG(1)))
	{
		rt_internal_make_c_string(ARG(0));
	}
	else
	{
		LOAD_SMSTR((CL_FORM *)&KClisp[18], ARG(1));	/* MAKE-C-STRING */
		LOAD_SMSTR((CL_FORM *)&KClisp[16], ARG(2));	/* The evaluated value ~S is not of type string. */
		COPY(ARG(0), ARG(3));
		error_in(ARG(1), 3);
	}
}