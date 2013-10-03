/* This file was generated by CLiCC 0.6.4 [obrep 1] */
#define __OBREP 1
#include <c_decl.h>
#include "sys.h"
#include "lisp.h"

void FFI_c_long_double(CL_FORM *base)
{
	if(CL_FLOATP(ARG(0)))
	{
		rt_make_c_long_double(ARG(0));
	}
	else
	{
		COPY(ARG(0), ARG(1));
		FFI_c_long_double_p(ARG(1));
		if(CL_TRUEP(ARG(1)))
		{
			rt_cast_c_long_double(ARG(0));
		}
		else
		{
			LOAD_SMSTR((CL_FORM *)&KClisp[22], ARG(1));	/* C-LONG-DOUBLE */
			LOAD_SMSTR((CL_FORM *)&KClisp[20], ARG(2));	/* The evaluated value ~S is not of type float. */
			COPY(ARG(0), ARG(3));
			error_in(ARG(1), 3);
		}
	}
}
