/* This file was generated by CLiCC 0.6.4 [obrep 1] */
#define __OBREP 1
#include <c_decl.h>
#include "sys.h"
#include "lisp.h"

static void Z26_lambda(CL_FORM *base, int nargs);

void search1(CL_FORM *base)
{
	GEN_HEAPVAR(ARG(4), ARG(10));
	if(CL_TRUEP(ARG(3)))
	{
	}
	else
	{
		if(CL_TRUEP(INDIRECT(ARG(4))))
		{
			GEN_CLOSURE(array, ARG(10), 4, Z26_lambda, -1);
			COPY(ARG(4), &array[3]);
			LOAD_CLOSURE(array, ARG(10));
			COPY(ARG(10), ARG(3));
		}
		else
		{
			GEN_STATIC_GLOBAL_FUNARG(extern_closure, Feql, 2);
			LOAD_GLOBFUN(&extern_closure, ARG(3));
		}
	}
	COPY(ARG(6), ARG(10));
	COPY(ARG(8), ARG(11));
	COPY(ARG(0), ARG(12));
	Flength(ARG(12));
	check_seq_start_end(ARG(10));
	COPY(ARG(10), ARG(8));
	COPY(ARG(7), ARG(10));
	COPY(ARG(9), ARG(11));
	COPY(ARG(1), ARG(12));
	Flength(ARG(12));
	check_seq_start_end(ARG(10));
	COPY(ARG(10), ARG(9));
	if(CL_TRUEP(ARG(2)))
	{
		COPY(ARG(5), ARG(10));
		COPY(ARG(0), ARG(11));
		COPY(ARG(8), ARG(12));
		F1minus(ARG(12));
		Felt(ARG(11));
		Ffuncall(ARG(10), 2);
		mv_count = 1;
	}
	else
	{
		COPY(ARG(5), ARG(10));
		COPY(ARG(0), ARG(11));
		COPY(ARG(6), ARG(12));
		Felt(ARG(11));
		Ffuncall(ARG(10), 2);
		mv_count = 1;
	}
	if(CL_TRUEP(ARG(2)))
	{
		COPY(ARG(9), ARG(11));
		F1minus(ARG(11));
		M1_1:;
		COPY(ARG(11), ARG(12));
		COPY(ARG(7), ARG(13));
		Fle(ARG(12), 2);
		if(CL_TRUEP(ARG(12)))
		{
			LOAD_NIL(ARG(0));
			goto RETURN2;
		}
		COPY(ARG(3), ARG(12));
		COPY(ARG(10), ARG(13));
		COPY(ARG(5), ARG(14));
		COPY(ARG(1), ARG(15));
		COPY(ARG(11), ARG(16));
		Felt(ARG(15));
		Ffuncall(ARG(14), 2);
		mv_count = 1;
		Ffuncall(ARG(12), 3);
		mv_count = 1;
		if(CL_TRUEP(ARG(12)))
		{
			LOAD_FIXNUM(ARG(12), 2, ARG(12));
			COPY(ARG(8), ARG(13));
			Fminus(ARG(12), 2);
			COPY(ARG(11), ARG(13));
			F1minus(ARG(13));
			M2_1:;
			COPY(ARG(12), ARG(14));
			COPY(ARG(6), ARG(15));
			Fle(ARG(14), 2);
			if(CL_TRUEP(ARG(14)))
			{
				goto THEN1;
			}
			else
			{
				COPY(ARG(13), ARG(15));
				COPY(ARG(7), ARG(16));
				Fle(ARG(15), 2);
			}
			if(CL_TRUEP(ARG(15)))
			{
				THEN1:;
				goto RETURN4;
			}
			COPY(ARG(3), ARG(14));
			COPY(ARG(5), ARG(15));
			COPY(ARG(0), ARG(16));
			COPY(ARG(12), ARG(17));
			Felt(ARG(16));
			Ffuncall(ARG(15), 2);
			mv_count = 1;
			COPY(ARG(5), ARG(16));
			COPY(ARG(1), ARG(17));
			COPY(ARG(13), ARG(18));
			Felt(ARG(17));
			Ffuncall(ARG(16), 2);
			mv_count = 1;
			Ffuncall(ARG(14), 3);
			mv_count = 1;
			if(CL_TRUEP(ARG(14)))
			{
			}
			else
			{
				COPY(ARG(13), ARG(11));
				F1plus(ARG(11));
				goto RETURN3;
			}
			COPY(ARG(12), ARG(14));
			F1minus(ARG(14));
			F1minus(ARG(13));
			COPY(ARG(14), ARG(12));
			goto M2_1;
			RETURN4:;
			COPY(ARG(11), ARG(12));
			F1plus(ARG(12));
			COPY(ARG(8), ARG(13));
			COPY(ARG(6), ARG(14));
			Fminus(ARG(13), 2);
			Fminus(ARG(12), 2);
			COPY(ARG(12), ARG(0));
			goto RETURN1;
		}
		RETURN3:;
		F1minus(ARG(11));
		goto M1_1;
		RETURN2:;
	}
	else
	{
		COPY(ARG(7), ARG(11));
		M3_1:;
		COPY(ARG(11), ARG(12));
		COPY(ARG(9), ARG(13));
		Fge(ARG(12), 2);
		if(CL_TRUEP(ARG(12)))
		{
			LOAD_NIL(ARG(0));
			goto RETURN5;
		}
		COPY(ARG(3), ARG(12));
		COPY(ARG(10), ARG(13));
		COPY(ARG(5), ARG(14));
		COPY(ARG(1), ARG(15));
		COPY(ARG(11), ARG(16));
		Felt(ARG(15));
		Ffuncall(ARG(14), 2);
		mv_count = 1;
		Ffuncall(ARG(12), 3);
		mv_count = 1;
		if(CL_TRUEP(ARG(12)))
		{
			COPY(ARG(6), ARG(12));
			F1plus(ARG(12));
			COPY(ARG(11), ARG(13));
			F1plus(ARG(13));
			M4_1:;
			COPY(ARG(12), ARG(14));
			COPY(ARG(8), ARG(15));
			Fge(ARG(14), 2);
			if(CL_TRUEP(ARG(14)))
			{
				goto THEN2;
			}
			else
			{
				COPY(ARG(13), ARG(15));
				COPY(ARG(9), ARG(16));
				Fge(ARG(15), 2);
			}
			if(CL_TRUEP(ARG(15)))
			{
				THEN2:;
				goto RETURN7;
			}
			COPY(ARG(3), ARG(14));
			COPY(ARG(5), ARG(15));
			COPY(ARG(0), ARG(16));
			COPY(ARG(12), ARG(17));
			Felt(ARG(16));
			Ffuncall(ARG(15), 2);
			mv_count = 1;
			COPY(ARG(5), ARG(16));
			COPY(ARG(1), ARG(17));
			COPY(ARG(13), ARG(18));
			Felt(ARG(17));
			Ffuncall(ARG(16), 2);
			mv_count = 1;
			Ffuncall(ARG(14), 3);
			mv_count = 1;
			if(CL_TRUEP(ARG(14)))
			{
			}
			else
			{
				COPY(ARG(13), ARG(11));
				F1minus(ARG(11));
				goto RETURN6;
			}
			COPY(ARG(12), ARG(14));
			F1plus(ARG(14));
			F1plus(ARG(13));
			COPY(ARG(14), ARG(12));
			goto M4_1;
			RETURN7:;
			COPY(ARG(11), ARG(0));
			goto RETURN1;
		}
		RETURN6:;
		F1plus(ARG(11));
		goto M3_1;
		RETURN5:;
	}
	RETURN1:;
}

static void Z26_lambda(CL_FORM *base, int nargs)
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
