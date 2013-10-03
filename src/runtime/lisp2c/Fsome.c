/* This file was generated by CLiCC 0.6.4 [obrep 1] */
#define __OBREP 1
#include <c_decl.h>
#include "sys.h"
#include "lisp.h"

GEN_GLOBAL_FUNARG(CFsome, Fsome, -3);

static void Z147_get_elem(CL_FORM *base);

void Fsome(CL_FORM *base, int nargs)
{
	Flist(ARG(2), nargs - 2);
	ALLOC_CONS(ARG(5), ARG(1), ARG(2), ARG(2));
	LOAD_GLOBFUN(&CFmin, ARG(3));
	LOAD_GLOBFUN(&CFlength, ARG(4));
	COPY(ARG(2), ARG(5));
	Fmapcar(ARG(4), 2);
	Fapply(ARG(3), 2);
	mv_count = 1;
	LOAD_FIXNUM(ARG(4), 0, ARG(4));
	GEN_HEAPVAR(ARG(4), ARG(5));
	{
		GEN_CLOSURE(array, ARG(5), 4, Z147_get_elem, 1);
		COPY(ARG(4), &array[3]);
		LOAD_CLOSURE(array, ARG(5));
	}
	M1_1:;
	COPY(INDIRECT(ARG(4)), ARG(6));
	COPY(ARG(3), ARG(7));
	Fge(ARG(6), 2);
	if(CL_TRUEP(ARG(6)))
	{
		LOAD_NIL(ARG(0));
		goto RETURN1;
	}
	COPY(ARG(0), ARG(6));
	COPY(ARG(5), ARG(7));
	COPY(ARG(2), ARG(8));
	Fmaplist(ARG(7), 2);
	Fapply(ARG(6), 2);
	mv_count = 1;
	if(CL_TRUEP(ARG(6)))
	{
		COPY(ARG(6), ARG(0));
		goto RETURN1;
	}
	COPY(INDIRECT(ARG(4)), ARG(6));
	F1plus(ARG(6));
	COPY(ARG(6), INDIRECT(ARG(4)));
	goto M1_1;
	RETURN1:;
}

static void Z147_get_elem(CL_FORM *base)
{
	if(CL_CONSP(ARG(1)))
	{
		COPY(GET_CAR(ARG(1)), ARG(2));
	}
	else
	{
		if(CL_TRUEP(ARG(1)))
		{
			LOAD_SMSTR((CL_FORM *)&KClisp[264], ARG(2));	/* ~a is not a list */
			COPY(ARG(1), ARG(3));
			Ferror(ARG(2), 2);
		}
		else
		{
			COPY(ARG(1), ARG(2));
		}
	}
	if(CL_LISTP(ARG(2)))
	{
		if(CL_CONSP(ARG(1)))
		{
			COPY(GET_CAR(ARG(1)), ARG(2));
		}
		else
		{
			if(CL_TRUEP(ARG(1)))
			{
				LOAD_SMSTR((CL_FORM *)&KClisp[264], ARG(2));	/* ~a is not a list */
				COPY(ARG(1), ARG(3));
				Ferror(ARG(2), 2);
			}
			else
			{
				COPY(ARG(1), ARG(2));
			}
		}
		if(CL_CONSP(ARG(2)))
		{
			COPY(GET_CAR(ARG(2)), ARG(3));
		}
		else
		{
			if(CL_TRUEP(ARG(2)))
			{
				LOAD_SMSTR((CL_FORM *)&KClisp[264], ARG(3));	/* ~a is not a list */
				COPY(ARG(2), ARG(4));
				Ferror(ARG(3), 2);
			}
			else
			{
				COPY(ARG(2), ARG(3));
			}
		}
		COPY(ARG(2), ARG(4));
		COPY(ARG(4), ARG(5));
		if(CL_CONSP(ARG(5)))
		{
			COPY(GET_CDR(ARG(5)), ARG(2));
		}
		else
		{
			if(CL_TRUEP(ARG(5)))
			{
				LOAD_SMSTR((CL_FORM *)&KClisp[262], ARG(6));	/* ~a is not a list */
				COPY(ARG(5), ARG(7));
				Ferror(ARG(6), 2);
			}
			else
			{
				COPY(ARG(5), ARG(2));
			}
		}
		if(CL_CONSP(ARG(1)))
		{
			COPY(ARG(2), GET_CAR(ARG(1)));
		}
		else
		{
			LOAD_SMSTR((CL_FORM *)&KClisp[252], ARG(4));	/* ~a is not a cons */
			COPY(ARG(1), ARG(5));
			Ferror(ARG(4), 2);
		}
		COPY(ARG(3), ARG(0));
	}
	else
	{
		if(CL_CONSP(ARG(1)))
		{
			COPY(GET_CAR(ARG(1)), ARG(2));
		}
		else
		{
			if(CL_TRUEP(ARG(1)))
			{
				LOAD_SMSTR((CL_FORM *)&KClisp[264], ARG(2));	/* ~a is not a list */
				COPY(ARG(1), ARG(3));
				Ferror(ARG(2), 2);
			}
			else
			{
				COPY(ARG(1), ARG(2));
			}
		}
		COPY(INDIRECT(GET_FORM(ARG(0)) + 3), ARG(3));
		Felt(ARG(2));
		COPY(ARG(2), ARG(0));
	}
}