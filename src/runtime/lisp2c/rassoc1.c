/* This file was generated by CLiCC 0.6.4 [obrep 1] */
#define __OBREP 1
#include <c_decl.h>
#include "sys.h"
#include "lisp.h"

static void Z1_lambda(CL_FORM *base, int nargs);

void rassoc1(CL_FORM *base)
{
	GEN_HEAPVAR(ARG(3), ARG(5));
	if(CL_TRUEP(ARG(2)))
	{
	}
	else
	{
		if(CL_TRUEP(INDIRECT(ARG(3))))
		{
			GEN_CLOSURE(array, ARG(5), 4, Z1_lambda, -1);
			COPY(ARG(3), &array[3]);
			LOAD_CLOSURE(array, ARG(5));
			COPY(ARG(5), ARG(2));
		}
		else
		{
			GEN_STATIC_GLOBAL_FUNARG(extern_closure, Feql, 2);
			LOAD_GLOBFUN(&extern_closure, ARG(2));
		}
	}
	{
		GEN_STATIC_GLOBAL_FUNARG(extern_closure, Feql, 2);
		LOAD_GLOBFUN(&extern_closure, ARG(6));
	}
	if(EQ(ARG(2), ARG(6)))
	{
		LOAD_NIL(ARG(5));
		COPY(ARG(1), ARG(6));
		M1_1:;
		if(CL_ATOMP(ARG(6)))
		{
			LOAD_NIL(ARG(5));
			COPY(ARG(5), ARG(0));
			goto RETURN2;
		}
		COPY(ARG(6), ARG(7));
		COPY(GET_CAR(ARG(7)), ARG(5));
		if(CL_TRUEP(ARG(4)))
		{
			COPY(ARG(4), ARG(8));
			if(CL_CONSP(ARG(5)))
			{
				COPY(GET_CAR(ARG(5)), ARG(9));
			}
			else
			{
				if(CL_TRUEP(ARG(5)))
				{
					LOAD_SMSTR((CL_FORM *)&KClisp[264], ARG(9));	/* ~a is not a list */
					COPY(ARG(5), ARG(10));
					Ferror(ARG(9), 2);
				}
				else
				{
					COPY(ARG(5), ARG(9));
				}
			}
			Ffuncall(ARG(8), 2);
			mv_count = 1;
		}
		else
		{
			if(CL_CONSP(ARG(5)))
			{
				COPY(GET_CAR(ARG(5)), ARG(8));
			}
			else
			{
				if(CL_TRUEP(ARG(5)))
				{
					LOAD_SMSTR((CL_FORM *)&KClisp[264], ARG(8));	/* ~a is not a list */
					COPY(ARG(5), ARG(9));
					Ferror(ARG(8), 2);
				}
				else
				{
					COPY(ARG(5), ARG(8));
				}
			}
		}
		if(EQL(ARG(0), ARG(8)))
		{
			COPY(ARG(5), ARG(0));
			goto RETURN1;
		}
		COPY(ARG(6), ARG(7));
		COPY(GET_CDR(ARG(7)), ARG(6));
		goto M1_1;
		RETURN2:;
	}
	else
	{
		LOAD_NIL(ARG(5));
		COPY(ARG(1), ARG(6));
		M2_1:;
		if(CL_ATOMP(ARG(6)))
		{
			LOAD_NIL(ARG(5));
			COPY(ARG(5), ARG(0));
			goto RETURN3;
		}
		COPY(ARG(6), ARG(7));
		COPY(GET_CAR(ARG(7)), ARG(5));
		COPY(ARG(2), ARG(7));
		COPY(ARG(0), ARG(8));
		if(CL_TRUEP(ARG(4)))
		{
			COPY(ARG(4), ARG(9));
			if(CL_CONSP(ARG(5)))
			{
				COPY(GET_CDR(ARG(5)), ARG(10));
			}
			else
			{
				if(CL_TRUEP(ARG(5)))
				{
					LOAD_SMSTR((CL_FORM *)&KClisp[262], ARG(10));	/* ~a is not a list */
					COPY(ARG(5), ARG(11));
					Ferror(ARG(10), 2);
				}
				else
				{
					COPY(ARG(5), ARG(10));
				}
			}
			Ffuncall(ARG(9), 2);
			mv_count = 1;
		}
		else
		{
			if(CL_CONSP(ARG(5)))
			{
				COPY(GET_CDR(ARG(5)), ARG(9));
			}
			else
			{
				if(CL_TRUEP(ARG(5)))
				{
					LOAD_SMSTR((CL_FORM *)&KClisp[262], ARG(9));	/* ~a is not a list */
					COPY(ARG(5), ARG(10));
					Ferror(ARG(9), 2);
				}
				else
				{
					COPY(ARG(5), ARG(9));
				}
			}
		}
		Ffuncall(ARG(7), 3);
		mv_count = 1;
		if(CL_TRUEP(ARG(7)))
		{
			COPY(ARG(5), ARG(0));
			goto RETURN1;
		}
		COPY(ARG(6), ARG(7));
		if(CL_CONSP(ARG(7)))
		{
			COPY(GET_CDR(ARG(7)), ARG(6));
		}
		else
		{
			if(CL_TRUEP(ARG(7)))
			{
				LOAD_SMSTR((CL_FORM *)&KClisp[262], ARG(6));	/* ~a is not a list */
				Ferror(ARG(6), 2);
			}
			else
			{
				COPY(ARG(7), ARG(6));
			}
		}
		goto M2_1;
		RETURN3:;
	}
	RETURN1:;
}

static void Z1_lambda(CL_FORM *base, int nargs)
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
