/* This file was generated by CLiCC 0.6.4 [obrep 1] */
#define __OBREP 1
#include <c_decl.h>
#include "sys.h"
#include "lisp.h"

CL_INIT Kread_char1[] =
{
	MAKE_STRING(24, "unexpected end-of-stream"),	/* 0 */
};

void read_char1(CL_FORM *base)
{
	if(CL_TRUEP(ARG(0)))
	{
		if(CL_SYMBOLP(ARG(0)) && GET_SYMBOL(ARG(0)) == SYMBOL(Slisp, 48))	/* T */
		{
			COPY(SYMVAL(Slisp, 59), ARG(0));	/* *TERMINAL-IO* */
		}
	}
	else
	{
		COPY(SYMVAL(Slisp, 60), ARG(0));	/* *STANDARD-INPUT* */
	}
	COPY(ARG(0), ARG(4));
	COPY(ARG(4), ARG(5));
	COPY(ARG(5), ARG(6));
	LOAD_SYMBOL(SYMBOL(Slisp, 64), ARG(7));	/* STREAM */
	rt_struct_typep(ARG(6));
	if(CL_TRUEP(ARG(6)))
	{
		COPY(OFFSET(AR_BASE(GET_FORM(ARG(5))), 2 + 1), ARG(4));
	}
	else
	{
		COPY(SYMVAL(Slisp, 352), ARG(4));	/* NO_STRUCT */
		LOAD_SYMBOL(SYMBOL(Slisp, 64), ARG(6));	/* STREAM */
		Ferror(ARG(4), 3);
	}
	Ffuncall(ARG(4), 1);
	mv_count = 1;
	if(CL_TRUEP(ARG(4)))
	{
		COPY(ARG(4), ARG(0));
	}
	else
	{
		if(CL_TRUEP(ARG(1)))
		{
			goto THEN1;
		}
		else
		{
		}
		if(CL_TRUEP(ARG(3)))
		{
			THEN1:;
			LOAD_SMSTR((CL_FORM *)&Kread_char1[0], ARG(5));	/* unexpected end-of-stream */
			Ferror(ARG(5), 1);
		}
		COPY(ARG(2), ARG(0));
	}
}
