/* This file was generated by CLiCC 0.6.4 [obrep 1] */
#define __OBREP 1
#include <c_decl.h>
#include "sys.h"
#include "lisp.h"

static void Z31_lambda(CL_FORM *base, int nargs);

void list_find1(CL_FORM *base)
{
	GEN_HEAPVAR(ARG(4), ARG(8));
	COPY(ARG(5), ARG(8));
	LOAD_NIL(ARG(9));
	if(CL_TRUEP(ARG(3)))
	{
	}
	else
	{
		if(CL_TRUEP(INDIRECT(ARG(4))))
		{
			GEN_CLOSURE(array, ARG(10), 4, Z31_lambda, -1);
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
	COPY(ARG(5), ARG(10));
	COPY(ARG(6), ARG(11));
	COPY(ARG(1), ARG(12));
	Flength(ARG(12));
	check_seq_start_end(ARG(10));
	COPY(ARG(10), ARG(6));
	if(CL_TRUEP(ARG(2)))
	{
		F1minus(ARG(6));
	}
	LOAD_NIL(ARG(10));
	COPY(ARG(5), ARG(11));
	COPY(ARG(1), ARG(12));
	Fnthcdr(ARG(11));
	M1_1:;
	if(CL_ATOMP(ARG(11)))
	{
		LOAD_NIL(ARG(10));
		COPY(ARG(10), ARG(0));
		goto RETURN1;
	}
	COPY(ARG(11), ARG(12));
	COPY(GET_CAR(ARG(12)), ARG(10));
	COPY(ARG(3), ARG(12));
	COPY(ARG(0), ARG(13));
	COPY(ARG(7), ARG(14));
	COPY(ARG(10), ARG(15));
	Ffuncall(ARG(14), 2);
	mv_count = 1;
	Ffuncall(ARG(12), 3);
	mv_count = 1;
	if(CL_TRUEP(ARG(12)))
	{
		if(CL_TRUEP(ARG(2)))
		{
			COPY(ARG(10), ARG(9));
		}
		else
		{
			COPY(ARG(10), ARG(0));
			goto RETURN1;
		}
	}
	COPY(ARG(8), ARG(12));
	COPY(ARG(6), ARG(13));
	Fnumeql(ARG(12), 2);
	if(CL_TRUEP(ARG(12)))
	{
		COPY(ARG(9), ARG(0));
		goto RETURN1;
	}
	F1plus(ARG(8));
	COPY(ARG(11), ARG(12));
	if(CL_CONSP(ARG(12)))
	{
		COPY(GET_CDR(ARG(12)), ARG(11));
	}
	else
	{
		if(CL_TRUEP(ARG(12)))
		{
			LOAD_SMSTR((CL_FORM *)&KClisp[262], ARG(11));	/* ~a is not a list */
			Ferror(ARG(11), 2);
		}
		else
		{
			COPY(ARG(12), ARG(11));
		}
	}
	goto M1_1;
	RETURN1:;
}

static void Z31_lambda(CL_FORM *base, int nargs)
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
