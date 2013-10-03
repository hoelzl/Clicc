/* This file was generated by CLiCC 0.6.4 [obrep 1] */
#define __OBREP 1
#include <c_decl.h>
#include "sys.h"
#include "lisp.h"

GEN_GLOBAL_FUNARG(Cmake_host, make_host, -1);

void make_host(CL_FORM *base, int nargs)
{
	BOOL supl_flags[7];
	static CL_FORM * keylist[] =
	{
		SYMBOL(Slisp, 256),	/* PARSE */
		SYMBOL(Slisp, 257),	/* UNPARSE */
		SYMBOL(Slisp, 258),	/* UNPARSE-HOST */
		SYMBOL(Slisp, 259),	/* UNPARSE-DIRECTORY */
		SYMBOL(Slisp, 260),	/* UNPARSE-FILE */
		SYMBOL(Slisp, 261),	/* UNPARSE-ENOUGH */
		SYMBOL(Slisp, 262),	/* CUSTOMARY-CASE */
	};
	keysort(ARG(0), nargs - 0, 7, keylist, supl_flags, FALSE);
	if(NOT(supl_flags[0]))
	{
		LOAD_NIL(ARG(0));
	}
	if(NOT(supl_flags[1]))
	{
		LOAD_NIL(ARG(1));
	}
	if(NOT(supl_flags[2]))
	{
		LOAD_NIL(ARG(2));
	}
	if(NOT(supl_flags[3]))
	{
		LOAD_NIL(ARG(3));
	}
	if(NOT(supl_flags[4]))
	{
		LOAD_NIL(ARG(4));
	}
	if(NOT(supl_flags[5]))
	{
		LOAD_NIL(ARG(5));
	}
	if(NOT(supl_flags[6]))
	{
		LOAD_NIL(ARG(6));
	}
	LOAD_SYMBOL(SYMBOL(Slisp, 263), ARG(7));	/* HOST */
	COPY(ARG(0), ARG(8));
	COPY(ARG(1), ARG(9));
	COPY(ARG(2), ARG(10));
	COPY(ARG(3), ARG(11));
	COPY(ARG(4), ARG(12));
	COPY(ARG(5), ARG(13));
	COPY(ARG(6), ARG(14));
	rt_make_struct(ARG(7), 8);
	COPY(ARG(7), ARG(0));
}