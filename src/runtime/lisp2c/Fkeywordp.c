/* This file was generated by CLiCC 0.6.4 [obrep 1] */
#define __OBREP 1
#include <c_decl.h>
#include "sys.h"
#include "lisp.h"

void Fkeywordp(CL_FORM *base)
{
	if(CL_SYMBOLP(ARG(0)) || CL_NILP(ARG(0)))
	{
		COPY(ARG(0), ARG(1));
		Fsymbol_package(ARG(1));
		LOAD_BOOL(EQ(ARG(1), SYMVAL(Slisp, 390)), ARG(0));	/* *KEYWORD-PACKAGE* */
	}
	else
	{
		LOAD_NIL(ARG(0));
	}
}