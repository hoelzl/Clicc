/* This file was generated by CLiCC 0.6.4 [obrep 1] */
#define __OBREP 1
#include <c_decl.h>
#include "sys.h"
#include "lisp.h"

GEN_GLOBAL_FUNARG(CFvector, Fvector, -1);

void Fvector(CL_FORM *base, int nargs)
{
	Flist(ARG(0), nargs - 0);
	COPY(ARG(0), ARG(1));
	Flength(ARG(1));
	LOAD_SYMBOL(SYMBOL(Slisp, 48), ARG(2));	/* T */
	LOAD_NIL(ARG(3));
	COPY(ARG(0), ARG(4));
	LOAD_NIL(ARG(5));
	LOAD_NIL(ARG(6));
	LOAD_NIL(ARG(7));
	LOAD_FIXNUM(ARG(8), 0, ARG(8));
	LOAD_NIL(ARG(9));
	LOAD_SYMBOL(SYMBOL(Slisp, 48), ARG(10));	/* T */
	LOAD_NIL(ARG(11));
	make_array1(ARG(1));
	COPY(ARG(1), ARG(0));
}
