/* This file was generated by CLiCC 0.6.4 [obrep 1] */
#define __OBREP 1
#include <c_decl.h>
#include "sys.h"
#include "lisp.h"

void write1(CL_FORM *base)
{
	BIND_SPECIAL(SYMBOL(Slisp, 474), ARG(2));	/* *PRINT-ESCAPE* */
	BIND_SPECIAL(SYMBOL(Slisp, 479), ARG(3));	/* *PRINT-RADIX* */
	BIND_SPECIAL(SYMBOL(Slisp, 471), ARG(4));	/* *PRINT-BASE* */
	BIND_SPECIAL(SYMBOL(Slisp, 473), ARG(5));	/* *PRINT-CIRCLE* */
	BIND_SPECIAL(SYMBOL(Slisp, 478), ARG(6));	/* *PRINT-PRETTY* */
	BIND_SPECIAL(SYMBOL(Slisp, 477), ARG(7));	/* *PRINT-LEVEL* */
	BIND_SPECIAL(SYMBOL(Slisp, 476), ARG(8));	/* *PRINT-LENGTH* */
	BIND_SPECIAL(SYMBOL(Slisp, 472), ARG(9));	/* *PRINT-CASE* */
	BIND_SPECIAL(SYMBOL(Slisp, 475), ARG(10));	/* *PRINT-GENSYM* */
	BIND_SPECIAL(SYMBOL(Slisp, 470), ARG(11));	/* *PRINT-ARRAY* */
	if(CL_TRUEP(ARG(1)))
	{
		if(CL_SYMBOLP(ARG(1)) && GET_SYMBOL(ARG(1)) == SYMBOL(Slisp, 48))	/* T */
		{
			COPY(SYMVAL(Slisp, 59), ARG(1));	/* *TERMINAL-IO* */
		}
	}
	else
	{
		COPY(SYMVAL(Slisp, 61), ARG(1));	/* *STANDARD-OUTPUT* */
	}
	write2(ARG(0));
	RESTORE_SPECIAL;
	RESTORE_SPECIAL;
	RESTORE_SPECIAL;
	RESTORE_SPECIAL;
	RESTORE_SPECIAL;
	RESTORE_SPECIAL;
	RESTORE_SPECIAL;
	RESTORE_SPECIAL;
	RESTORE_SPECIAL;
	RESTORE_SPECIAL;
}
