/* This file was generated by CLiCC 0.6.4 [obrep 1] */
#define __OBREP 1
#include <c_decl.h>
#include "sys.h"
#include "lisp.h"

void Fbreak(CL_FORM *base, int nargs)
{
	switch(nargs)
	{
		case 0:
		LOAD_NIL(ARG(0));
		nargs = 1;
	}
	Flist(ARG(1), nargs - 1);
	break1(ARG(0));
}
