/* This file was generated by CLiCC 0.6.4 [obrep 1] */
#define __OBREP 1
#include <c_decl.h>
#include "sys.h"
#include "lisp.h"

void Fparse_integer(CL_FORM *base, int nargs)
{
	BOOL supl_flags[4];
	static CL_FORM * keylist[] =
	{
		SYMBOL(Slisp, 231),	/* START */
		SYMBOL(Slisp, 232),	/* END */
		SYMBOL(Slisp, 469),	/* RADIX */
		SYMBOL(Slisp, 264),	/* JUNK-ALLOWED */
	};
	keysort(ARG(1), nargs - 1, 4, keylist, supl_flags, FALSE);
	if(NOT(supl_flags[0]))
	{
		LOAD_FIXNUM(ARG(5), 0, ARG(1));
	}
	if(NOT(supl_flags[1]))
	{
		COPY(ARG(0), ARG(5));
		Flength(ARG(5));
		COPY(ARG(5), ARG(2));
	}
	if(NOT(supl_flags[2]))
	{
		LOAD_FIXNUM(ARG(5), 10, ARG(3));
	}
	if(NOT(supl_flags[3]))
	{
		LOAD_NIL(ARG(4));
	}
	parse_integer1(ARG(0));
}
