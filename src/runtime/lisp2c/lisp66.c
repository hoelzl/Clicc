/* This file was generated by CLiCC 0.6.4 [obrep 1] */
#define __OBREP 1
#include <c_decl.h>
#include "sys.h"
#include "lisp.h"

static void Z33_lambda(CL_FORM *base, int nargs);

void remove_duplicates1(CL_FORM *base)
{
	GEN_HEAPVAR(ARG(3), ARG(7));
	COPY(ARG(0), ARG(7));
	Flength(ARG(7));
	LOAD_NIL(ARG(8));
	LOAD_NIL(ARG(9));
	LOAD_NIL(ARG(10));
	if(CL_TRUEP(ARG(2)))
	{
	}
	else
	{
		if(CL_TRUEP(INDIRECT(ARG(3))))
		{
			GEN_CLOSURE(array, ARG(11), 4, Z33_lambda, -1);
			COPY(ARG(3), &array[3]);
			LOAD_CLOSURE(array, ARG(11));
			COPY(ARG(11), ARG(2));
		}
		else
		{
			GEN_STATIC_GLOBAL_FUNARG(extern_closure, Feql, 2);
			LOAD_GLOBFUN(&extern_closure, ARG(2));
		}
	}
	COPY(ARG(4), ARG(11));
	COPY(ARG(5), ARG(12));
	COPY(ARG(7), ARG(13));
	check_seq_start_end(ARG(11));
	COPY(ARG(11), ARG(5));
	if(CL_TRUEP(ARG(1)))
	{
		COPY(ARG(5), ARG(11));
		F1minus(ARG(11));
		M1_1:;
		COPY(ARG(11), ARG(12));
		COPY(ARG(4), ARG(13));
		Flt(ARG(12), 2);
		if(CL_TRUEP(ARG(12)))
		{
			goto RETURN1;
		}
		COPY(ARG(11), ARG(12));
		F1minus(ARG(12));
		M2_1:;
		COPY(ARG(12), ARG(13));
		COPY(ARG(4), ARG(14));
		Flt(ARG(13), 2);
		if(CL_TRUEP(ARG(13)))
		{
			COPY(ARG(0), ARG(13));
			COPY(ARG(11), ARG(14));
			Felt(ARG(13));
			ALLOC_CONS(ARG(15), ARG(13), ARG(10), ARG(10));
			goto RETURN2;
		}
		COPY(ARG(2), ARG(13));
		COPY(ARG(6), ARG(14));
		COPY(ARG(0), ARG(15));
		COPY(ARG(11), ARG(16));
		Felt(ARG(15));
		Ffuncall(ARG(14), 2);
		mv_count = 1;
		COPY(ARG(6), ARG(15));
		COPY(ARG(0), ARG(16));
		COPY(ARG(12), ARG(17));
		Felt(ARG(16));
		Ffuncall(ARG(15), 2);
		mv_count = 1;
		Ffuncall(ARG(13), 3);
		mv_count = 1;
		if(CL_TRUEP(ARG(13)))
		{
			goto RETURN2;
		}
		F1minus(ARG(12));
		goto M2_1;
		RETURN2:;
		F1minus(ARG(11));
		goto M1_1;
		RETURN1:;
	}
	else
	{
		COPY(ARG(4), ARG(11));
		M3_1:;
		COPY(ARG(11), ARG(12));
		COPY(ARG(5), ARG(13));
		Fge(ARG(12), 2);
		if(CL_TRUEP(ARG(12)))
		{
			COPY(ARG(10), ARG(12));
			Freverse(ARG(12));
			COPY(ARG(12), ARG(10));
			goto RETURN3;
		}
		COPY(ARG(11), ARG(12));
		F1plus(ARG(12));
		M4_1:;
		COPY(ARG(12), ARG(13));
		COPY(ARG(5), ARG(14));
		Fge(ARG(13), 2);
		if(CL_TRUEP(ARG(13)))
		{
			COPY(ARG(0), ARG(13));
			COPY(ARG(11), ARG(14));
			Felt(ARG(13));
			ALLOC_CONS(ARG(15), ARG(13), ARG(10), ARG(10));
			goto RETURN4;
		}
		COPY(ARG(2), ARG(13));
		COPY(ARG(6), ARG(14));
		COPY(ARG(0), ARG(15));
		COPY(ARG(11), ARG(16));
		Felt(ARG(15));
		Ffuncall(ARG(14), 2);
		mv_count = 1;
		COPY(ARG(6), ARG(15));
		COPY(ARG(0), ARG(16));
		COPY(ARG(12), ARG(17));
		Felt(ARG(16));
		Ffuncall(ARG(15), 2);
		mv_count = 1;
		Ffuncall(ARG(13), 3);
		mv_count = 1;
		if(CL_TRUEP(ARG(13)))
		{
			goto RETURN4;
		}
		F1plus(ARG(12));
		goto M4_1;
		RETURN4:;
		F1plus(ARG(11));
		goto M3_1;
		RETURN3:;
	}
	COPY(ARG(4), ARG(11));
	COPY(ARG(10), ARG(12));
	Flength(ARG(12));
	COPY(ARG(7), ARG(13));
	COPY(ARG(5), ARG(14));
	Fminus(ARG(13), 2);
	Fplus(ARG(11), 3);
	COPY(ARG(11), ARG(9));
	COPY(ARG(0), ARG(11));
	sequence_type(ARG(11));
	COPY(ARG(9), ARG(12));
	COPY(ARG(11), ARG(13));
	COPY(ARG(12), ARG(14));
	LOAD_NIL(ARG(15));
	LOAD_NIL(ARG(16));
	make_sequence1(ARG(13));
	COPY(ARG(13), ARG(8));
	LOAD_FIXNUM(ARG(11), 0, ARG(11));
	COPY(ARG(5), ARG(12));
	M5_1:;
	COPY(ARG(11), ARG(13));
	COPY(ARG(4), ARG(14));
	Fge(ARG(13), 2);
	if(CL_TRUEP(ARG(13)))
	{
		goto RETURN5;
	}
	COPY(ARG(0), ARG(13));
	COPY(ARG(11), ARG(14));
	Felt(ARG(13));
	COPY(ARG(8), ARG(14));
	COPY(ARG(11), ARG(15));
	Fset_elt(ARG(13));
	F1plus(ARG(11));
	goto M5_1;
	RETURN5:;
	M6_1:;
	if(CL_TRUEP(ARG(10)))
	{
	}
	else
	{
		goto RETURN6;
	}
	if(CL_CONSP(ARG(10)))
	{
		COPY(GET_CAR(ARG(10)), ARG(13));
	}
	else
	{
		LOAD_SMSTR((CL_FORM *)&KClisp[264], ARG(13));	/* ~a is not a list */
		COPY(ARG(10), ARG(14));
		Ferror(ARG(13), 2);
	}
	COPY(ARG(10), ARG(14));
	COPY(ARG(14), ARG(15));
	if(CL_CONSP(ARG(15)))
	{
		COPY(GET_CDR(ARG(15)), ARG(10));
	}
	else
	{
		LOAD_SMSTR((CL_FORM *)&KClisp[262], ARG(16));	/* ~a is not a list */
		COPY(ARG(15), ARG(17));
		Ferror(ARG(16), 2);
	}
	COPY(ARG(13), ARG(14));
	COPY(ARG(8), ARG(15));
	COPY(ARG(11), ARG(16));
	Fset_elt(ARG(14));
	F1plus(ARG(11));
	goto M6_1;
	RETURN6:;
	M7_1:;
	COPY(ARG(11), ARG(13));
	COPY(ARG(9), ARG(14));
	Fge(ARG(13), 2);
	if(CL_TRUEP(ARG(13)))
	{
		goto RETURN7;
	}
	COPY(ARG(0), ARG(13));
	COPY(ARG(12), ARG(14));
	Felt(ARG(13));
	COPY(ARG(8), ARG(14));
	COPY(ARG(11), ARG(15));
	Fset_elt(ARG(13));
	F1plus(ARG(11));
	F1plus(ARG(12));
	goto M7_1;
	RETURN7:;
	COPY(ARG(8), ARG(0));
}

static void Z33_lambda(CL_FORM *base, int nargs)
{
	CL_FORM *rest_0;
	CL_FORM *local;
	rest_0 = ARG(1);
	local = ARG(nargs + 1);
	{
		COPY(INDIRECT(GET_FORM(ARG(0)) + 3), LOCAL(0));
		REST_APPLY(LOCAL(0), 1, rest_0);
		mv_count = 1;
		if(CL_TRUEP(LOCAL(0)))
		{
			LOAD_NIL(ARG(0));
		}
		else
		{
			LOAD_SYMBOL(SYMBOL(Slisp, 48), ARG(0));	/* T */
		}
	}
}