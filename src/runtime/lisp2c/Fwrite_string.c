/* This file was generated by CLiCC 0.6.4 [obrep 1] */
#define __OBREP 1
#include <c_decl.h>
#include "sys.h"
#include "lisp.h"

GEN_GLOBAL_FUNARG(CFwrite_string, Fwrite_string, -2);

void Fwrite_string(CL_FORM *base, int nargs)
{
	switch(nargs)
	{
		case 1:
		LOAD_NIL(ARG(1));
		nargs = 2;
	}
	{
		BOOL supl_flags[2];
		static CL_FORM * keylist[] =
		{
			SYMBOL(Slisp, 231),	/* START */
			SYMBOL(Slisp, 232),	/* END */
		};
		keysort(ARG(2), nargs - 2, 2, keylist, supl_flags, FALSE);
		if(NOT(supl_flags[0]))
		{
			LOAD_FIXNUM(ARG(4), 0, ARG(2));
		}
		if(NOT(supl_flags[1]))
		{
			COPY(ARG(0), ARG(4));
			Flength(ARG(4));
			COPY(ARG(4), ARG(3));
		}
	}
	write_string1(ARG(0));
}
