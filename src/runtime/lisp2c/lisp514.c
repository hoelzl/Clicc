/* This file was generated by CLiCC 0.6.4 [obrep 1] */
#define __OBREP 1
#include <c_decl.h>
#include "sys.h"
#include "lisp.h"

void Fset_macro_character(CL_FORM *base, int nargs)
{
	switch(nargs)
	{
		case 2:
		LOAD_NIL(ARG(2));
		case 3:
		COPY(SYMVAL(Slisp, 454), ARG(3));	/* *READTABLE* */
		case 4:
		break;
		default:
		Labort(TOO_MANY_ARGS);
	}
	set_macro_character1(ARG(0));
}
