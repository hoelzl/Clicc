/* This file was generated by CLiCC 0.6.4 [obrep 1] */
#define __OBREP 1
#include <c_decl.h>
#include "sys.h"
#include "lisp.h"

void Fset_get(CL_FORM *base, int nargs)
{
	switch(nargs)
	{
		case 3:
		LOAD_NIL(ARG(3));
		case 4:
		break;
		default:
		Labort(TOO_MANY_ARGS);
	}
	set_get1(ARG(0));
}
