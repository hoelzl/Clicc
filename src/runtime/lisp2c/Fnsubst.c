/* This file was generated by CLiCC 0.6.4 [obrep 1] */
#define __OBREP 1
#include <c_decl.h>
#include "sys.h"
#include "lisp.h"

void Fnsubst(CL_FORM *base, int nargs)
{
	BOOL supl_flags[3];
	static CL_FORM * keylist[] =
	{
		SYMBOL(Slisp, 282),	/* TEST */
		SYMBOL(Slisp, 550),	/* TEST-NOT */
		SYMBOL(Slisp, 209),	/* KEY */
	};
	keysort(ARG(3), nargs - 3, 3, keylist, supl_flags, FALSE);
	if(NOT(supl_flags[0]))
	{
		LOAD_NIL(ARG(3));
	}
	if(NOT(supl_flags[1]))
	{
		LOAD_NIL(ARG(4));
	}
	if(NOT(supl_flags[2]))
	{
		LOAD_NIL(ARG(5));
	}
	nsubst1(ARG(0));
}
