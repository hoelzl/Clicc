/* This file was generated by CLiCC 0.6.4 [obrep 1] */
#define __OBREP 1
#include <c_decl.h>
#include "sys.h"
#include "lisp.h"

void Fwrite_to_string(CL_FORM *base, int nargs)
{
	BOOL supl_flags[10];
	static CL_FORM * keylist[] =
	{
		SYMBOL(Slisp, 494),	/* ESCAPE */
		SYMBOL(Slisp, 469),	/* RADIX */
		SYMBOL(Slisp, 495),	/* BASE */
		SYMBOL(Slisp, 496),	/* CIRCLE */
		SYMBOL(Slisp, 497),	/* PRETTY */
		SYMBOL(Slisp, 498),	/* LEVEL */
		SYMBOL(Slisp, 88),	/* LENGTH */
		SYMBOL(Slisp, 277),	/* CASE */
		SYMBOL(Slisp, 499),	/* GENSYM */
		SYMBOL(Slisp, 410),	/* ARRAY */
	};
	keysort(ARG(1), nargs - 1, 10, keylist, supl_flags, FALSE);
	if(NOT(supl_flags[0]))
	{
		COPY(SYMVAL(Slisp, 474), ARG(1));	/* *PRINT-ESCAPE* */
	}
	if(NOT(supl_flags[1]))
	{
		COPY(SYMVAL(Slisp, 479), ARG(2));	/* *PRINT-RADIX* */
	}
	if(NOT(supl_flags[2]))
	{
		COPY(SYMVAL(Slisp, 471), ARG(3));	/* *PRINT-BASE* */
	}
	if(NOT(supl_flags[3]))
	{
		COPY(SYMVAL(Slisp, 473), ARG(4));	/* *PRINT-CIRCLE* */
	}
	if(NOT(supl_flags[4]))
	{
		COPY(SYMVAL(Slisp, 478), ARG(5));	/* *PRINT-PRETTY* */
	}
	if(NOT(supl_flags[5]))
	{
		COPY(SYMVAL(Slisp, 477), ARG(6));	/* *PRINT-LEVEL* */
	}
	if(NOT(supl_flags[6]))
	{
		COPY(SYMVAL(Slisp, 476), ARG(7));	/* *PRINT-LENGTH* */
	}
	if(NOT(supl_flags[7]))
	{
		COPY(SYMVAL(Slisp, 472), ARG(8));	/* *PRINT-CASE* */
	}
	if(NOT(supl_flags[8]))
	{
		COPY(SYMVAL(Slisp, 475), ARG(9));	/* *PRINT-GENSYM* */
	}
	if(NOT(supl_flags[9]))
	{
		COPY(SYMVAL(Slisp, 470), ARG(10));	/* *PRINT-ARRAY* */
	}
	write_to_string1(ARG(0));
}
