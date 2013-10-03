/* This file was generated by CLiCC 0.6.4 [obrep 1] */
#define __OBREP 1
#include <c_decl.h>
#include "sys.h"
#include "lisp.h"

CL_INIT Kmake_hash_table1[] =
{
	MAKE_STRING(41, "test ~a is not a valid test-function name"),	/* 0 */
	MAKE_STRING(57, "test ~a in make-hash-table should be a function or symbol"),	/* 2 */
	MAKE_STRING(51, "rehash-size ~A is not a floating-point number > 1.0"),	/* 4 */
	MAKE_STRING(56, "rehash-size ~A is not a floating-point number or integer"),	/* 6 */
	MAKE_STRING(80, "rehash-threshold ~A is not a floating-point ~\n            number between 0 and 1"),	/* 8 */
};

void make_hash_table1(CL_FORM *base)
{
	if(CL_SYMBOLP(ARG(0)) || CL_NILP(ARG(0)))
	{
		if(CL_SYMBOLP(ARG(0)) && GET_SYMBOL(ARG(0)) == SYMBOL(Slisp, 411))	/* EQ */
		{
			GEN_STATIC_GLOBAL_FUNARG(extern_closure, Feq, 2);
			LOAD_GLOBFUN(&extern_closure, ARG(0));
		}
		else
		{
			if(CL_SYMBOLP(ARG(0)) && GET_SYMBOL(ARG(0)) == SYMBOL(Slisp, 412))	/* EQL */
			{
				GEN_STATIC_GLOBAL_FUNARG(extern_closure, Feql, 2);
				LOAD_GLOBFUN(&extern_closure, ARG(0));
			}
			else
			{
				if(CL_SYMBOLP(ARG(0)) && GET_SYMBOL(ARG(0)) == SYMBOL(Slisp, 413))	/* EQUAL */
				{
					LOAD_GLOBFUN(&CFequal, ARG(0));
				}
				else
				{
					if(CL_SYMBOLP(ARG(0)) && GET_SYMBOL(ARG(0)) == SYMBOL(Slisp, 414))	/* EQUALP */
					{
						LOAD_GLOBFUN(&CFequalp, ARG(0));
					}
					else
					{
						LOAD_SMSTR((CL_FORM *)&Kmake_hash_table1[0], ARG(4));	/* test ~a is not a valid test-function name */
						COPY(ARG(0), ARG(5));
						Ferror(ARG(4), 2);
					}
				}
			}
		}
	}
	else
	{
		if(CL_FUNCTIONP(ARG(0)))
		{
		}
		else
		{
			LOAD_SMSTR((CL_FORM *)&Kmake_hash_table1[2], ARG(4));	/* test ~a in make-hash-table should be a function or symbol */
			COPY(ARG(0), ARG(5));
			Ferror(ARG(4), 2);
		}
	}
	if(CL_FIXNUMP(ARG(2)))
	{
		COPY(ARG(1), ARG(4));
		COPY(ARG(2), ARG(5));
		Fdiv(ARG(4), 2);
		GEN_FLOAT(ARG(5), 1.0, ARG(5));
		Fplus(ARG(4), 2);
		COPY(ARG(4), ARG(2));
	}
	else
	{
		if(CL_FLOATP(ARG(2)))
		{
			GEN_FLOAT(ARG(4), 1.0, ARG(4));
			COPY(ARG(2), ARG(5));
			Fge(ARG(4), 2);
			if(CL_TRUEP(ARG(4)))
			{
				LOAD_SMSTR((CL_FORM *)&Kmake_hash_table1[4], ARG(4));	/* rehash-size ~A is not a floating-point number > 1.0 */
				COPY(ARG(2), ARG(5));
				Ferror(ARG(4), 2);
			}
		}
		else
		{
			LOAD_SMSTR((CL_FORM *)&Kmake_hash_table1[6], ARG(4));	/* rehash-size ~A is not a floating-point number or integer */
			COPY(ARG(2), ARG(5));
			Ferror(ARG(4), 2);
		}
	}
	if(CL_FLOATP(ARG(3)))
	{
		COPY(ARG(3), ARG(4));
		GEN_FLOAT(ARG(5), 0.0, ARG(5));
		Fge(ARG(4), 2);
		if(CL_TRUEP(ARG(4)))
		{
			COPY(ARG(3), ARG(4));
			GEN_FLOAT(ARG(5), 1.0, ARG(5));
			Fle(ARG(4), 2);
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
	if(CL_TRUEP(ARG(4)))
	{
	}
	else
	{
		ELSE1:;
		LOAD_SMSTR((CL_FORM *)&Kmake_hash_table1[8], ARG(4));	/* rehash-threshold ~A is not a floating-point ~
            number between 0 and 1 */
		COPY(ARG(3), ARG(5));
		Ferror(ARG(4), 2);
	}
	COPY(ARG(1), ARG(4));
	LOAD_FIXNUM(ARG(5), 2, ARG(5));
	COPY(ARG(4), ARG(6));
	LOAD_FIXNUM(ARG(7), 2, ARG(7));
	rt_truncate(ARG(6));
	COPY(&mv_buf[0], ARG(7));
	mv_count = 1;
	{
		COPY(ARG(7), ARG(5));
	}
	Fzerop(ARG(5));
	if(CL_TRUEP(ARG(5)))
	{
		F1plus(ARG(4));
	}
	LOAD_FIXNUM(ARG(5), 3, ARG(5));
	COPY(ARG(4), ARG(6));
	LOAD_FIXNUM(ARG(7), 3, ARG(7));
	rt_truncate(ARG(6));
	COPY(&mv_buf[0], ARG(7));
	mv_count = 1;
	{
		COPY(ARG(7), ARG(5));
	}
	Fzerop(ARG(5));
	if(CL_TRUEP(ARG(5)))
	{
		LOAD_FIXNUM(ARG(5), 2, ARG(5));
		Fplus(ARG(4), 2);
	}
	LOAD_FIXNUM(ARG(5), 5, ARG(5));
	COPY(ARG(4), ARG(6));
	LOAD_FIXNUM(ARG(7), 5, ARG(7));
	rt_truncate(ARG(6));
	COPY(&mv_buf[0], ARG(7));
	mv_count = 1;
	{
		COPY(ARG(7), ARG(5));
	}
	Fzerop(ARG(5));
	if(CL_TRUEP(ARG(5)))
	{
		LOAD_FIXNUM(ARG(5), 4, ARG(5));
		Fplus(ARG(4), 2);
	}
	LOAD_FIXNUM(ARG(5), 0, ARG(5));
	COPY(ARG(4), ARG(6));
	LOAD_SYMBOL(SYMBOL(Slisp, 48), ARG(7));	/* T */
	LOAD_NIL(ARG(8));
	LOAD_NIL(ARG(9));
	LOAD_NIL(ARG(10));
	LOAD_NIL(ARG(11));
	LOAD_NIL(ARG(12));
	LOAD_FIXNUM(ARG(13), 0, ARG(13));
	LOAD_SYMBOL(SYMBOL(Slisp, 48), ARG(14));	/* T */
	LOAD_NIL(ARG(15));
	LOAD_NIL(ARG(16));
	make_array1(ARG(6));
	LOAD_SYMBOL(SYMBOL(Slisp, 399), ARG(7));	/* HASH-TABLE */
	COPY(ARG(4), ARG(8));
	LOAD_FIXNUM(ARG(9), 0, ARG(9));
	COPY(ARG(2), ARG(10));
	COPY(ARG(3), ARG(11));
	COPY(ARG(0), ARG(12));
	COPY(ARG(6), ARG(13));
	rt_make_struct(ARG(7), 7);
	COPY(ARG(7), ARG(0));
}