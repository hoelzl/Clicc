/* This file was generated by CLiCC 0.6.4 [obrep 1] */
#define __OBREP 1
#include <c_decl.h>
#include "sys.h"
#include "lisp.h"

void quick_sort(CL_FORM *base)
{
	LOAD_FIXNUM(ARG(5), 0, ARG(5));
	LOAD_FIXNUM(ARG(6), 0, ARG(6));
	COPY(ARG(2), ARG(7));
	COPY(ARG(1), ARG(8));
	F1plus(ARG(8));
	Fle(ARG(7), 2);
	if(CL_TRUEP(ARG(7)))
	{
		goto RETURN1;
	}
	COPY(ARG(1), ARG(5));
	COPY(ARG(2), ARG(6));
	F1minus(ARG(6));
	COPY(ARG(0), ARG(7));
	COPY(ARG(1), ARG(8));
	Felt(ARG(7));
	M1_1:;
	COPY(ARG(5), ARG(8));
	COPY(ARG(6), ARG(9));
	Fgt(ARG(8), 2);
	if(CL_TRUEP(ARG(8)))
	{
		goto RETURN2;
	}
	M2_1:;
	COPY(ARG(5), ARG(8));
	COPY(ARG(6), ARG(9));
	Fgt(ARG(8), 2);
	if(CL_TRUEP(ARG(8)))
	{
		goto THEN1;
	}
	else
	{
		COPY(ARG(3), ARG(9));
		COPY(ARG(4), ARG(10));
		COPY(ARG(0), ARG(11));
		COPY(ARG(6), ARG(12));
		Felt(ARG(11));
		Ffuncall(ARG(10), 2);
		mv_count = 1;
		COPY(ARG(4), ARG(11));
		COPY(ARG(7), ARG(12));
		Ffuncall(ARG(11), 2);
		mv_count = 1;
		Ffuncall(ARG(9), 3);
		mv_count = 1;
	}
	if(CL_TRUEP(ARG(9)))
	{
		THEN1:;
		goto RETURN3;
	}
	F1minus(ARG(6));
	goto M2_1;
	RETURN3:;
	COPY(ARG(6), ARG(8));
	COPY(ARG(1), ARG(9));
	Flt(ARG(8), 2);
	if(CL_TRUEP(ARG(8)))
	{
		COPY(ARG(0), ARG(8));
		COPY(ARG(1), ARG(9));
		F1plus(ARG(9));
		COPY(ARG(2), ARG(10));
		COPY(ARG(3), ARG(11));
		COPY(ARG(4), ARG(12));
		quick_sort(ARG(8));
		goto RETURN1;
	}
	M3_1:;
	COPY(ARG(5), ARG(8));
	COPY(ARG(6), ARG(9));
	Fgt(ARG(8), 2);
	if(CL_TRUEP(ARG(8)))
	{
		goto THEN2;
	}
	else
	{
		COPY(ARG(3), ARG(9));
		COPY(ARG(4), ARG(10));
		COPY(ARG(0), ARG(11));
		COPY(ARG(5), ARG(12));
		Felt(ARG(11));
		Ffuncall(ARG(10), 2);
		mv_count = 1;
		COPY(ARG(4), ARG(11));
		COPY(ARG(7), ARG(12));
		Ffuncall(ARG(11), 2);
		mv_count = 1;
		Ffuncall(ARG(9), 3);
		mv_count = 1;
		if(CL_TRUEP(ARG(9)))
		{
			goto ELSE3;
		}
		else
		{
			goto THEN2;
		}
	}
	{
		THEN2:;
		goto RETURN4;
	}
	ELSE3:;
	F1plus(ARG(5));
	goto M3_1;
	RETURN4:;
	COPY(ARG(5), ARG(8));
	COPY(ARG(6), ARG(9));
	Fgt(ARG(8), 2);
	if(CL_TRUEP(ARG(8)))
	{
		goto RETURN2;
	}
	COPY(ARG(0), ARG(8));
	COPY(ARG(5), ARG(9));
	Felt(ARG(8));
	COPY(ARG(0), ARG(9));
	COPY(ARG(6), ARG(10));
	Felt(ARG(9));
	COPY(ARG(0), ARG(10));
	COPY(ARG(5), ARG(11));
	Fset_elt(ARG(9));
	COPY(ARG(8), ARG(9));
	COPY(ARG(0), ARG(10));
	COPY(ARG(6), ARG(11));
	Fset_elt(ARG(9));
	F1plus(ARG(5));
	F1minus(ARG(6));
	goto M1_1;
	RETURN2:;
	COPY(ARG(0), ARG(7));
	COPY(ARG(1), ARG(8));
	COPY(ARG(5), ARG(9));
	COPY(ARG(3), ARG(10));
	COPY(ARG(4), ARG(11));
	quick_sort(ARG(7));
	COPY(ARG(0), ARG(7));
	COPY(ARG(5), ARG(8));
	COPY(ARG(2), ARG(9));
	COPY(ARG(3), ARG(10));
	COPY(ARG(4), ARG(11));
	quick_sort(ARG(7));
	RETURN1:;
}
