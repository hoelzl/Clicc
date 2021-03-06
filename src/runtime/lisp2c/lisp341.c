/* This file was generated by CLiCC 0.6.4 [obrep 1] */
#define __OBREP 1
#include <c_decl.h>
#include "sys.h"
#include "lisp.h"

CL_INIT Kmaybe_diddle_case[] =
{
	MAKE_STRING(44, "etypecase: the value ~a is not a legal value"),	/* 0 */
	MAKE_STRING(44, "etypecase: the value ~a is not a legal value"),	/* 2 */
};

static void Z115_check_for(CL_FORM *base);
static void Z116_diddle_with(CL_FORM *base);

void maybe_diddle_case(CL_FORM *base)
{
	if(CL_TRUEP(ARG(1)))
	{
		LOAD_GLOBFUN(&CFupper_case_p, ARG(2));
		COPY(ARG(0), ARG(3));
		Z115_check_for(ARG(2));
		LOAD_GLOBFUN(&CFlower_case_p, ARG(3));
		COPY(ARG(0), ARG(4));
		Z115_check_for(ARG(3));
		if(CL_TRUEP(ARG(2)))
		{
		}
		else
		{
			goto ELSE1;
		}
		if(CL_TRUEP(ARG(3)))
		{
		}
		else
		{
			ELSE1:;
			if(CL_TRUEP(ARG(2)))
			{
				LOAD_GLOBFUN(&CFstring_downcase, ARG(4));
				COPY(ARG(0), ARG(5));
				Z116_diddle_with(ARG(4));
				COPY(ARG(4), ARG(0));
			}
			else
			{
				if(CL_TRUEP(ARG(3)))
				{
					LOAD_GLOBFUN(&CFstring_upcase, ARG(4));
					COPY(ARG(0), ARG(5));
					Z116_diddle_with(ARG(4));
					COPY(ARG(4), ARG(0));
				}
				else
				{
				}
			}
		}
	}
	else
	{
	}
}

static void Z115_check_for(CL_FORM *base)
{
	COPY(ARG(1), ARG(2));
	LOAD_SYMBOL(SYMBOL(Slisp, 250), ARG(3));	/* PATTERN */
	rt_struct_typep(ARG(2));
	if(CL_TRUEP(ARG(2)))
	{
		LOAD_NIL(ARG(2));
		COPY(ARG(1), ARG(3));
		pattern_pieces(ARG(3));
		M1_1:;
		if(CL_ATOMP(ARG(3)))
		{
			LOAD_NIL(ARG(2));
			COPY(ARG(2), ARG(0));
			goto RETURN1;
		}
		COPY(ARG(3), ARG(4));
		COPY(GET_CAR(ARG(4)), ARG(2));
		if(CL_SMSTRP(ARG(2)))
		{
			COPY(ARG(0), ARG(4));
			COPY(ARG(2), ARG(5));
			Z115_check_for(ARG(4));
			bool_result = CL_TRUEP(ARG(4));
		}
		else
		{
			if(CL_CONSP(ARG(2)))
			{
				if(CL_CONSP(ARG(1)))
				{
					COPY(GET_CAR(ARG(1)), ARG(4));
				}
				else
				{
					if(CL_TRUEP(ARG(1)))
					{
						LOAD_SMSTR((CL_FORM *)&KClisp[264], ARG(4));	/* ~a is not a list */
						COPY(ARG(1), ARG(5));
						Ferror(ARG(4), 2);
					}
					else
					{
						COPY(ARG(1), ARG(4));
					}
				}
				if(CL_SYMBOLP(ARG(4)) && GET_SYMBOL(ARG(4)) == SYMBOL(Slisp, 265))	/* CHARACTER-SET */
				{
					COPY(ARG(0), ARG(5));
					if(CL_CONSP(ARG(1)))
					{
						COPY(GET_CDR(ARG(1)), ARG(6));
					}
					else
					{
						if(CL_TRUEP(ARG(1)))
						{
							LOAD_SMSTR((CL_FORM *)&KClisp[262], ARG(6));	/* ~a is not a list */
							COPY(ARG(1), ARG(7));
							Ferror(ARG(6), 2);
						}
						else
						{
							COPY(ARG(1), ARG(6));
						}
					}
					Z115_check_for(ARG(5));
				}
				else
				{
					goto ELSE1;
				}
			}
			else
			{
				goto ELSE1;
			}
			bool_result = CL_TRUEP(ARG(5));
		}
		if(bool_result)
		{
			LOAD_SYMBOL(SYMBOL(Slisp, 48), ARG(0));	/* T */
			goto RETURN1;
		}
		ELSE1:;
		COPY(ARG(3), ARG(4));
		COPY(GET_CDR(ARG(4)), ARG(3));
		goto M1_1;
		RETURN1:;
	}
	else
	{
		if(CL_LISTP(ARG(1)))
		{
			LOAD_NIL(ARG(2));
			COPY(ARG(1), ARG(3));
			M2_1:;
			if(CL_ATOMP(ARG(3)))
			{
				LOAD_NIL(ARG(2));
				COPY(ARG(2), ARG(0));
				goto RETURN2;
			}
			COPY(ARG(3), ARG(4));
			COPY(GET_CAR(ARG(4)), ARG(2));
			COPY(ARG(0), ARG(4));
			COPY(ARG(2), ARG(5));
			Z115_check_for(ARG(4));
			if(CL_TRUEP(ARG(4)))
			{
				LOAD_SYMBOL(SYMBOL(Slisp, 48), ARG(0));	/* T */
				goto RETURN2;
			}
			COPY(ARG(3), ARG(4));
			COPY(GET_CDR(ARG(4)), ARG(3));
			goto M2_1;
			RETURN2:;
		}
		else
		{
			if(CL_SMSTRP(ARG(1)))
			{
				LOAD_FIXNUM(ARG(2), 0, ARG(2));
				M3_1:;
				COPY(ARG(2), ARG(3));
				COPY(ARG(1), ARG(4));
				Flength(ARG(4));
				Fge(ARG(3), 2);
				if(CL_TRUEP(ARG(3)))
				{
					LOAD_NIL(ARG(0));
					goto RETURN3;
				}
				COPY(ARG(0), ARG(3));
				if(CL_SMSTRP(ARG(1)))
				{
				}
				else
				{
					COPY(SYMVAL(Slisp, 58), ARG(4));	/* WRONG_TYPE */
					COPY(ARG(1), ARG(5));
					LOAD_SYMBOL(SYMBOL(Slisp, 40), ARG(6));	/* SIMPLE-STRING */
					Ferror(ARG(4), 3);
				}
				COPY(ARG(1), ARG(4));
				COPY(ARG(2), ARG(5));
				pvref(ARG(4));
				Ffuncall(ARG(3), 2);
				mv_count = 1;
				if(CL_TRUEP(ARG(3)))
				{
					LOAD_SYMBOL(SYMBOL(Slisp, 48), ARG(0));	/* T */
					goto RETURN3;
				}
				F1plus(ARG(2));
				goto M3_1;
				RETURN3:;
			}
			else
			{
				if(CL_SYMBOLP(ARG(1)) && GET_SYMBOL(ARG(1)) == SYMBOL(Slisp, 266))	/* UNSPECIFIC */
				{
					LOAD_NIL(ARG(0));
				}
				else
				{
					LOAD_SMSTR((CL_FORM *)&Kmaybe_diddle_case[0], ARG(0));	/* etypecase: the value ~a is not a legal value */
					Ferror(ARG(0), 2);
				}
			}
		}
	}
}

static void Z117_lambda(CL_FORM *base);

static void Z116_diddle_with(CL_FORM *base)
{
	GEN_HEAPVAR(ARG(0), ARG(2));
	GEN_HEAPVAR(ARG(1), ARG(2));
	COPY(INDIRECT(ARG(1)), ARG(2));
	LOAD_SYMBOL(SYMBOL(Slisp, 250), ARG(3));	/* PATTERN */
	rt_struct_typep(ARG(2));
	if(CL_TRUEP(ARG(2)))
	{
		GEN_CLOSURE(array, ARG(2), 5, Z117_lambda, 1);
		COPY(ARG(1), &array[3]);
		COPY(ARG(0), &array[4]);
		LOAD_CLOSURE(array, ARG(2));
		COPY(ARG(2), ARG(2));
		COPY(INDIRECT(ARG(1)), ARG(3));
		pattern_pieces(ARG(3));
		Fmapcar(ARG(2), 2);
		LOAD_SYMBOL(SYMBOL(Slisp, 250), ARG(0));	/* PATTERN */
		COPY(ARG(2), ARG(1));
		rt_make_struct(ARG(0), 2);
	}
	else
	{
		if(CL_LISTP(INDIRECT(ARG(1))))
		{
			COPY(INDIRECT(ARG(0)), ARG(0));
			COPY(INDIRECT(ARG(1)), ARG(1));
			Fmapcar(ARG(0), 2);
		}
		else
		{
			if(CL_SMSTRP(INDIRECT(ARG(1))))
			{
				COPY(INDIRECT(ARG(0)), ARG(0));
				COPY(INDIRECT(ARG(1)), ARG(1));
				Ffuncall(ARG(0), 2);
			}
			else
			{
				LOAD_SMSTR((CL_FORM *)&Kmaybe_diddle_case[2], ARG(0));	/* etypecase: the value ~a is not a legal value */
				COPY(INDIRECT(ARG(1)), ARG(1));
				Ferror(ARG(0), 2);
			}
		}
	}
}

static void Z117_lambda(CL_FORM *base)
{
	if(CL_SMSTRP(ARG(1)))
	{
		COPY(INDIRECT(GET_FORM(ARG(0)) + 4), ARG(2));
		COPY(INDIRECT(GET_FORM(ARG(0)) + 3), ARG(3));
		Ffuncall(ARG(2), 2);
		COPY(ARG(2), ARG(0));
	}
	else
	{
		if(CL_CONSP(ARG(1)))
		{
			COPY(GET_CAR(ARG(1)), ARG(2));
			if(CL_SYMBOLP(ARG(2)) && GET_SYMBOL(ARG(2)) == SYMBOL(Slisp, 265))	/* CHARACTER-SET */
			{
				LOAD_SYMBOL(SYMBOL(Slisp, 265), ARG(3));	/* CHARACTER-SET */
				COPY(INDIRECT(GET_FORM(ARG(0)) + 4), ARG(4));
				COPY(GET_CDR(ARG(1)), ARG(5));
				Ffuncall(ARG(4), 2);
				mv_count = 1;
				ALLOC_CONS(ARG(5), ARG(3), ARG(4), ARG(0));
			}
			else
			{
				COPY(ARG(1), ARG(0));
			}
		}
		else
		{
			COPY(ARG(1), ARG(0));
		}
	}
}
