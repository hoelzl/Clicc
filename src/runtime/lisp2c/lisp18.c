/* This file was generated by CLiCC 0.6.4 [obrep 1] */
#define __OBREP 1
#include <c_decl.h>
#include "sys.h"
#include "lisp.h"

void string_not_greaterp1(CL_FORM *base)
{
	COPY(ARG(0), ARG(6));
	Fstring(ARG(6));
	COPY(ARG(6), ARG(0));
	COPY(ARG(1), ARG(6));
	Fstring(ARG(6));
	COPY(ARG(6), ARG(1));
	COPY(ARG(2), ARG(6));
	COPY(ARG(3), ARG(7));
	COPY(ARG(0), ARG(8));
	Flength(ARG(8));
	check_seq_start_end(ARG(6));
	COPY(ARG(6), ARG(3));
	COPY(ARG(4), ARG(6));
	COPY(ARG(5), ARG(7));
	COPY(ARG(1), ARG(8));
	Flength(ARG(8));
	check_seq_start_end(ARG(6));
	COPY(ARG(6), ARG(5));
	COPY(ARG(2), ARG(6));
	COPY(ARG(4), ARG(7));
	LOAD_NIL(ARG(8));
	LOAD_NIL(ARG(9));
	M1_1:;
	COPY(ARG(6), ARG(10));
	COPY(ARG(3), ARG(11));
	Fnumeql(ARG(10), 2);
	if(CL_TRUEP(ARG(10)))
	{
		COPY(ARG(6), ARG(0));
		goto RETURN2;
	}
	COPY(ARG(7), ARG(10));
	COPY(ARG(5), ARG(11));
	Fnumeql(ARG(10), 2);
	if(CL_TRUEP(ARG(10)))
	{
		LOAD_NIL(ARG(0));
		goto RETURN2;
	}
	COPY(ARG(0), ARG(10));
	COPY(ARG(6), ARG(11));
	COPY(ARG(10), ARG(12));
	Fstringp(ARG(12));
	if(CL_TRUEP(ARG(12)))
	{
	}
	else
	{
		COPY(SYMVAL(Slisp, 58), ARG(12));	/* WRONG_TYPE */
		COPY(ARG(10), ARG(13));
		LOAD_SYMBOL(SYMBOL(Slisp, 44), ARG(14));	/* STRING */
		Ferror(ARG(12), 3);
	}
	COPY(ARG(10), ARG(12));
	COPY(ARG(11), ARG(13));
	Frow_major_aref(ARG(12));
	COPY(ARG(12), ARG(8));
	COPY(ARG(1), ARG(10));
	COPY(ARG(7), ARG(11));
	COPY(ARG(10), ARG(12));
	Fstringp(ARG(12));
	if(CL_TRUEP(ARG(12)))
	{
	}
	else
	{
		COPY(SYMVAL(Slisp, 58), ARG(12));	/* WRONG_TYPE */
		COPY(ARG(10), ARG(13));
		LOAD_SYMBOL(SYMBOL(Slisp, 44), ARG(14));	/* STRING */
		Ferror(ARG(12), 3);
	}
	COPY(ARG(10), ARG(9));
	COPY(ARG(11), ARG(10));
	Frow_major_aref(ARG(9));
	if(CL_CHARP(ARG(8)))
	{
		COPY(ARG(8), ARG(10));
	}
	else
	{
		COPY(SYMVAL(Slisp, 58), ARG(10));	/* WRONG_TYPE */
		COPY(ARG(8), ARG(11));
		LOAD_SYMBOL(SYMBOL(Slisp, 18), ARG(12));	/* CHARACTER */
		Ferror(ARG(10), 3);
	}
	rt_char_upcase(ARG(10));
	rt_char_code(ARG(10));
	if(CL_CHARP(ARG(9)))
	{
		COPY(ARG(9), ARG(11));
	}
	else
	{
		COPY(SYMVAL(Slisp, 58), ARG(11));	/* WRONG_TYPE */
		COPY(ARG(9), ARG(12));
		LOAD_SYMBOL(SYMBOL(Slisp, 18), ARG(13));	/* CHARACTER */
		Ferror(ARG(11), 3);
	}
	rt_char_upcase(ARG(11));
	rt_char_code(ARG(11));
	Flt(ARG(10), 2);
	if(CL_TRUEP(ARG(10)))
	{
		COPY(ARG(6), ARG(0));
		goto RETURN1;
	}
	else
	{
		if(CL_CHARP(ARG(8)))
		{
			COPY(ARG(8), ARG(10));
		}
		else
		{
			COPY(SYMVAL(Slisp, 58), ARG(10));	/* WRONG_TYPE */
			COPY(ARG(8), ARG(11));
			LOAD_SYMBOL(SYMBOL(Slisp, 18), ARG(12));	/* CHARACTER */
			Ferror(ARG(10), 3);
		}
		rt_char_upcase(ARG(10));
		rt_char_code(ARG(10));
		if(CL_CHARP(ARG(9)))
		{
			COPY(ARG(9), ARG(11));
		}
		else
		{
			COPY(SYMVAL(Slisp, 58), ARG(11));	/* WRONG_TYPE */
			COPY(ARG(9), ARG(12));
			LOAD_SYMBOL(SYMBOL(Slisp, 18), ARG(13));	/* CHARACTER */
			Ferror(ARG(11), 3);
		}
		rt_char_upcase(ARG(11));
		rt_char_code(ARG(11));
		Fnumeql(ARG(10), 2);
		if(CL_TRUEP(ARG(10)))
		{
		}
		else
		{
			LOAD_NIL(ARG(0));
			goto RETURN1;
		}
	}
	COPY(ARG(6), ARG(10));
	F1plus(ARG(10));
	F1plus(ARG(7));
	COPY(ARG(10), ARG(6));
	goto M1_1;
	RETURN2:;
	RETURN1:;
}