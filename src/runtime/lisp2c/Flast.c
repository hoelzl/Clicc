/* This file was generated by CLiCC 0.6.4 [obrep 1] */
#define __OBREP 1
#include <c_decl.h>
#include "sys.h"
#include "lisp.h"

void Flast(CL_FORM *base, int nargs)
{
	switch(nargs)
	{
		case 1:
		LOAD_FIXNUM(ARG(1), 1, ARG(1));
		case 2:
		break;
		default:
		Labort(TOO_MANY_ARGS);
	}
	last1(ARG(0));
}
