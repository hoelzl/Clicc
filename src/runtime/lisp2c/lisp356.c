/* This file was generated by CLiCC 0.6.4 [obrep 1] */
#define __OBREP 1
#include <c_decl.h>
#include "sys.h"
#include "lisp.h"

void Fwild_pathname_p(CL_FORM *base, int nargs)
{
	switch(nargs)
	{
		case 1:
		LOAD_NIL(ARG(1));
		case 2:
		break;
		default:
		Labort(TOO_MANY_ARGS);
	}
	wild_pathname_p1(ARG(0));
}
