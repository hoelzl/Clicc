/* This file was generated by CLiCC 0.6.4 [obrep 1] */
#define __OBREP 1
#include <c_decl.h>
#include "sys.h"
#include "lisp.h"

void write_string1(CL_FORM *base)
{
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
	M1_1:;
	COPY(ARG(2), ARG(4));
	COPY(ARG(3), ARG(5));
	Fge(ARG(4), 2);
	if(CL_TRUEP(ARG(4)))
	{
		goto RETURN1;
	}
	COPY(ARG(0), ARG(4));
	Fstringp(ARG(4));
	if(CL_TRUEP(ARG(4)))
	{
	}
	else
	{
		COPY(SYMVAL(Slisp, 58), ARG(4));	/* WRONG_TYPE */
		COPY(ARG(0), ARG(5));
		LOAD_SYMBOL(SYMBOL(Slisp, 44), ARG(6));	/* STRING */
		Ferror(ARG(4), 3);
	}
	COPY(ARG(0), ARG(4));
	COPY(ARG(2), ARG(5));
	Frow_major_aref(ARG(4));
	COPY(ARG(4), ARG(5));
	COPY(ARG(1), ARG(6));
	write_char1(ARG(5));
	mv_count = 1;
	F1plus(ARG(2));
	goto M1_1;
	RETURN1:;
}
