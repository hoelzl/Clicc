/* This file was generated by CLiCC 0.6.4 [obrep 1] */
#define __OBREP 1
#include <c_decl.h>
#include "sys.h"
#include "lisp.h"

void get1(CL_FORM *base)
{
	if(CL_SYMBOLP(ARG(0)))
	{
		COPY(SYM_PLIST(ARG(0)), ARG(3));
	}
	else
	{
		if(CL_TRUEP(ARG(0)))
		{
			COPY(SYMVAL(Slisp, 676), ARG(3));	/* SYM_EXPECTED */
			COPY(ARG(0), ARG(4));
			Ferror(ARG(3), 2);
		}
		else
		{
			COPY(SYMVAL(Slisp, 678), ARG(3));	/* *NIL-PLIST* */
		}
	}
	COPY(ARG(3), ARG(0));
	getf1(ARG(0));
}
