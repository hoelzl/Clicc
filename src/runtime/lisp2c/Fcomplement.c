/* This file was generated by CLiCC 0.6.4 [obrep 1] */
#define __OBREP 1
#include <c_decl.h>
#include "sys.h"
#include "lisp.h"

static void Z145_lambda(CL_FORM *base, int nargs);

void Fcomplement(CL_FORM *base)
{
	GEN_HEAPVAR(ARG(0), ARG(1));
	{
		GEN_CLOSURE(array, ARG(1), 4, Z145_lambda, -1);
		COPY(ARG(0), &array[3]);
		LOAD_CLOSURE(array, ARG(1));
	}
	COPY(ARG(1), ARG(0));
}

static void Z145_lambda(CL_FORM *base, int nargs)
{
	CL_FORM *rest_0;
	CL_FORM *local;
	rest_0 = ARG(1);
	local = ARG(nargs + 1);
	{
		COPY(INDIRECT(GET_FORM(ARG(0)) + 3), LOCAL(0));
		REST_APPLY(LOCAL(0), 1, rest_0);
		mv_count = 1;
		if(CL_TRUEP(LOCAL(0)))
		{
			LOAD_NIL(ARG(0));
		}
		else
		{
			LOAD_SYMBOL(SYMBOL(Slisp, 48), ARG(0));	/* T */
		}
	}
}