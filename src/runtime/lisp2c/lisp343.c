/* This file was generated by CLiCC 0.6.4 [obrep 1] */
#define __OBREP 1
#include <c_decl.h>
#include "sys.h"
#include "lisp.h"

void Fmerge_pathnames(CL_FORM *base, int nargs)
{
	switch(nargs)
	{
		case 1:
		COPY(SYMVAL(Slisp, 233), ARG(1));	/* *DEFAULT-PATHNAME-DEFAULTS* */
		case 2:
		LOAD_SYMBOL(SYMBOL(Slisp, 269), ARG(2));	/* NEWEST */
		case 3:
		break;
		default:
		Labort(TOO_MANY_ARGS);
	}
	merge_pathnames1(ARG(0));
}
