/* This file was generated by CLiCC 0.6.4 [obrep 1] */
#define __OBREP 1
#include <c_decl.h>
#include "sys.h"
#include "lisp.h"

GEN_GLOBAL_FUNARG(Cmake_readtable, make_readtable, -1);

void make_readtable(CL_FORM *base, int nargs)
{
	BOOL supl_flags[2];
	static CL_FORM * keylist[] =
	{
		SYMBOL(Slisp, 460),	/* SYNTAX */
		SYMBOL(Slisp, 461),	/* DISPATCH */
	};
	keysort(ARG(0), nargs - 0, 2, keylist, supl_flags, FALSE);
	if(NOT(supl_flags[0]))
	{
		LOAD_FIXNUM(ARG(2), 256, ARG(2));
		LOAD_SYMBOL(SYMBOL(Slisp, 155), ARG(3));	/* INITIAL-ELEMENT */
		LOAD_NIL(ARG(4));
		Fmake_array(ARG(2), 3);
		COPY(ARG(2), ARG(0));
	}
	if(NOT(supl_flags[1]))
	{
		LOAD_NIL(ARG(1));
	}
	LOAD_SYMBOL(SYMBOL(Slisp, 420), ARG(2));	/* READTABLE */
	COPY(ARG(0), ARG(3));
	COPY(ARG(1), ARG(4));
	rt_make_struct(ARG(2), 3);
	COPY(ARG(2), ARG(0));
}