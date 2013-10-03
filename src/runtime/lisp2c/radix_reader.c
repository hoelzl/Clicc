/* This file was generated by CLiCC 0.6.4 [obrep 1] */
#define __OBREP 1
#include <c_decl.h>
#include "sys.h"
#include "lisp.h"

GEN_GLOBAL_FUNARG(Cradix_reader, radix_reader, 3);

CL_INIT Kradix_reader[] =
{
	MAKE_STRING(42, "#~A (base ~D) value is not a rational: ~S."),	/* 0 */
	MAKE_STRING(25, "Illegal radix for #R: ~D."),	/* 2 */
	MAKE_STRING(20, "Radix missing in #R."),	/* 4 */
};

void radix_reader(CL_FORM *base)
{
	if(CL_TRUEP(SYMVAL(Slisp, 418)))	/* *READ-SUPPRESS* */
	{
		COPY(ARG(0), ARG(3));
		LOAD_SYMBOL(SYMBOL(Slisp, 48), ARG(4));	/* T */
		LOAD_NIL(ARG(5));
		LOAD_SYMBOL(SYMBOL(Slisp, 48), ARG(6));	/* T */
		read_char1(ARG(3));
		COPY(ARG(0), ARG(4));
		COPY(ARG(3), ARG(5));
		read_token(ARG(4));
		LOAD_NIL(ARG(0));
	}
	else
	{
		if(CL_TRUEP(ARG(2)))
		{
			LOAD_FIXNUM(ARG(3), 2, ARG(3));
			COPY(ARG(2), ARG(4));
			LOAD_FIXNUM(ARG(5), 36, ARG(5));
			Fle(ARG(3), 3);
			if(CL_TRUEP(ARG(3)))
			{
				COPY(ARG(2), ARG(3));
				BIND_SPECIAL(SYMBOL(Slisp, 417), ARG(3));	/* *READ-BASE* */
				COPY(ARG(0), ARG(3));
				LOAD_SYMBOL(SYMBOL(Slisp, 48), ARG(4));	/* T */
				LOAD_NIL(ARG(5));
				LOAD_SYMBOL(SYMBOL(Slisp, 48), ARG(6));	/* T */
				read1(ARG(3));
				RESTORE_SPECIAL;
				if(CL_FIXNUMP(ARG(3)))
				{
				}
				else
				{
					LOAD_SMSTR((CL_FORM *)&Kradix_reader[0], ARG(4));	/* #~A (base ~D) value is not a rational: ~S. */
					COPY(ARG(1), ARG(5));
					COPY(ARG(2), ARG(6));
					COPY(ARG(3), ARG(7));
					Ferror(ARG(4), 4);
				}
				COPY(ARG(3), ARG(0));
			}
			else
			{
				LOAD_SMSTR((CL_FORM *)&Kradix_reader[2], ARG(0));	/* Illegal radix for #R: ~D. */
				COPY(ARG(2), ARG(1));
				Ferror(ARG(0), 2);
			}
		}
		else
		{
			LOAD_SMSTR((CL_FORM *)&Kradix_reader[4], ARG(0));	/* Radix missing in #R. */
			Ferror(ARG(0), 1);
		}
	}
}
