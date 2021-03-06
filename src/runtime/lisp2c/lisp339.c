/* This file was generated by CLiCC 0.6.4 [obrep 1] */
#define __OBREP 1
#include <c_decl.h>
#include "sys.h"
#include "lisp.h"

void Fparse_namestring(CL_FORM *base, int nargs)
{
	switch(nargs)
	{
		case 1:
		LOAD_NIL(ARG(1));
		case 2:
		COPY(SYMVAL(Slisp, 233), ARG(2));	/* *DEFAULT-PATHNAME-DEFAULTS* */
		nargs = 3;
	}
	{
		BOOL supl_flags[3];
		static CL_FORM * keylist[] =
		{
			SYMBOL(Slisp, 231),	/* START */
			SYMBOL(Slisp, 232),	/* END */
			SYMBOL(Slisp, 264),	/* JUNK-ALLOWED */
		};
		keysort(ARG(3), nargs - 3, 3, keylist, supl_flags, FALSE);
		if(NOT(supl_flags[0]))
		{
			LOAD_FIXNUM(ARG(6), 0, ARG(3));
		}
		if(NOT(supl_flags[1]))
		{
			LOAD_NIL(ARG(4));
		}
		if(NOT(supl_flags[2]))
		{
			LOAD_NIL(ARG(5));
		}
	}
	parse_namestring1(ARG(0));
}
