/* This file was generated by CLiCC 0.6.4 [obrep 1] */
#define __OBREP 1
#include <c_decl.h>
#include "sys.h"
#include "lisp.h"

CL_INIT Kremove_backslashes[] =
{
	MAKE_STRING(29, "Backslash in bad place in ~S."),	/* 0 */
};

void remove_backslashes(CL_FORM *base)
{
	COPY(ARG(2), ARG(3));
	COPY(ARG(1), ARG(4));
	Fminus(ARG(3), 2);
	LOAD_CHAR(ARG(4), ' ', ARG(4));
	make_string1(ARG(3));
	LOAD_FIXNUM(ARG(4), 0, ARG(4));
	LOAD_NIL(ARG(5));
	COPY(ARG(1), ARG(6));
	M1_1:;
	COPY(ARG(6), ARG(7));
	COPY(ARG(2), ARG(8));
	Fnumeql(ARG(7), 2);
	if(CL_TRUEP(ARG(7)))
	{
		goto RETURN1;
	}
	if(CL_TRUEP(ARG(5)))
	{
		if(CL_SMSTRP(ARG(0)))
		{
		}
		else
		{
			COPY(SYMVAL(Slisp, 58), ARG(7));	/* WRONG_TYPE */
			COPY(ARG(0), ARG(8));
			LOAD_SYMBOL(SYMBOL(Slisp, 40), ARG(9));	/* SIMPLE-STRING */
			Ferror(ARG(7), 3);
		}
		COPY(ARG(0), ARG(7));
		COPY(ARG(6), ARG(8));
		pvref(ARG(7));
		COPY(ARG(7), ARG(8));
		COPY(ARG(3), ARG(9));
		COPY(ARG(4), ARG(10));
		Fset_schar(ARG(8));
		LOAD_NIL(ARG(5));
		F1plus(ARG(4));
	}
	else
	{
		if(CL_SMSTRP(ARG(0)))
		{
		}
		else
		{
			COPY(SYMVAL(Slisp, 58), ARG(7));	/* WRONG_TYPE */
			COPY(ARG(0), ARG(8));
			LOAD_SYMBOL(SYMBOL(Slisp, 40), ARG(9));	/* SIMPLE-STRING */
			Ferror(ARG(7), 3);
		}
		COPY(ARG(0), ARG(7));
		COPY(ARG(6), ARG(8));
		pvref(ARG(7));
		COPY(ARG(7), ARG(8));
		LOAD_CHAR(ARG(9), '\\', ARG(9));
		rt_charE(ARG(8));
		if(CL_TRUEP(ARG(8)))
		{
			LOAD_SYMBOL(SYMBOL(Slisp, 48), ARG(5));	/* T */
		}
		else
		{
			COPY(ARG(7), ARG(8));
			COPY(ARG(3), ARG(9));
			COPY(ARG(4), ARG(10));
			Fset_schar(ARG(8));
			F1plus(ARG(4));
		}
	}
	F1plus(ARG(6));
	goto M1_1;
	RETURN1:;
	if(CL_TRUEP(ARG(5)))
	{
		LOAD_SMSTR((CL_FORM *)&Kremove_backslashes[0], ARG(6));	/* Backslash in bad place in ~S. */
		COPY(ARG(0), ARG(7));
		Ferror(ARG(6), 2);
	}
	COPY(ARG(3), ARG(0));
	COPY(ARG(4), ARG(1));
	shrink_simple_string(ARG(0));
}
