/* This file was generated by CLiCC 0.6.4 [obrep 1] */
#define __OBREP 1
#include <c_decl.h>
#include "sys.h"
#include "lisp.h"

void Fvector_push_extend(CL_FORM *base, int nargs)
{
	switch(nargs)
	{
		case 2:
		LOAD_NIL(ARG(2));
		case 3:
		break;
		default:
		Labort(TOO_MANY_ARGS);
	}
	vector_push_extend1(ARG(0));
}
