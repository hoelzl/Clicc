/* This file was generated by CLiCC 0.6.4 [obrep 1] */
#define __OBREP 1
#include <c_decl.h>
#include "sys.h"
#include "lisp.h"

void replace1(CL_FORM *base)
{
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
	COPY(ARG(3), ARG(6));
	COPY(ARG(2), ARG(7));
	Fminus(ARG(6), 2);
	COPY(ARG(5), ARG(7));
	COPY(ARG(4), ARG(8));
	Fminus(ARG(7), 2);
	Fmin(ARG(6), 2);
	if(EQ(ARG(0), ARG(1)))
	{
		COPY(ARG(2), ARG(7));
		COPY(ARG(4), ARG(8));
		Fgt(ARG(7), 2);
	}
	else
	{
		goto ELSE1;
	}
	if(CL_TRUEP(ARG(7)))
	{
		LOAD_FIXNUM(ARG(7), 0, ARG(7));
		COPY(ARG(2), ARG(8));
		COPY(ARG(6), ARG(9));
		F1minus(ARG(9));
		Fplus(ARG(8), 2);
		COPY(ARG(4), ARG(9));
		COPY(ARG(6), ARG(10));
		F1minus(ARG(10));
		Fplus(ARG(9), 2);
		M1_1:;
		COPY(ARG(7), ARG(10));
		COPY(ARG(6), ARG(11));
		Fge(ARG(10), 2);
		if(CL_TRUEP(ARG(10)))
		{
			goto RETURN1;
		}
		COPY(ARG(1), ARG(10));
		COPY(ARG(9), ARG(11));
		Felt(ARG(10));
		COPY(ARG(0), ARG(11));
		COPY(ARG(8), ARG(12));
		Fset_elt(ARG(10));
		COPY(ARG(7), ARG(10));
		F1plus(ARG(10));
		COPY(ARG(8), ARG(11));
		F1minus(ARG(11));
		F1minus(ARG(9));
		COPY(ARG(11), ARG(8));
		COPY(ARG(10), ARG(7));
		goto M1_1;
		RETURN1:;
	}
	else
	{
		ELSE1:;
		LOAD_FIXNUM(ARG(7), 0, ARG(7));
		COPY(ARG(2), ARG(8));
		COPY(ARG(4), ARG(9));
		M2_1:;
		COPY(ARG(7), ARG(10));
		COPY(ARG(6), ARG(11));
		Fge(ARG(10), 2);
		if(CL_TRUEP(ARG(10)))
		{
			goto RETURN2;
		}
		COPY(ARG(1), ARG(10));
		COPY(ARG(9), ARG(11));
		Felt(ARG(10));
		COPY(ARG(0), ARG(11));
		COPY(ARG(8), ARG(12));
		Fset_elt(ARG(10));
		COPY(ARG(7), ARG(10));
		F1plus(ARG(10));
		COPY(ARG(8), ARG(11));
		F1plus(ARG(11));
		F1plus(ARG(9));
		COPY(ARG(11), ARG(8));
		COPY(ARG(10), ARG(7));
		goto M2_1;
		RETURN2:;
	}
}
