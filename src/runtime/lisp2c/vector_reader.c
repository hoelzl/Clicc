/* This file was generated by CLiCC 0.6.4 [obrep 1] */
#define __OBREP 1
#include <c_decl.h>
#include "sys.h"
#include "lisp.h"

GEN_GLOBAL_FUNARG(Cvector_reader, vector_reader, 3);

CL_INIT Kvector_reader[] =
{
	MAKE_STRING(42, "Vector longer than specified length: #~S~S"),	/* 0 */
	MAKE_STRING(52, "#~s( syntax is not allowed in backquoted expressions"),	/* 2 */
};

void vector_reader(CL_FORM *base)
{
	COPY(ARG(0), ARG(3));
	COPY(ARG(1), ARG(4));
	cons_reader(ARG(3));
	if(CL_TRUEP(SYMVAL(Slisp, 418)))	/* *READ-SUPPRESS* */
	{
		LOAD_NIL(ARG(0));
	}
	else
	{
		COPY(SYMVAL(Slisp, 447), ARG(4));	/* *BQ-LEVEL* */
		Fzerop(ARG(4));
		if(CL_TRUEP(ARG(4)))
		{
			if(CL_TRUEP(ARG(2)))
			{
				COPY(ARG(3), ARG(4));
				Flength(ARG(4));
				COPY(ARG(4), ARG(5));
				COPY(ARG(2), ARG(6));
				Fgt(ARG(5), 2);
				if(CL_TRUEP(ARG(5)))
				{
					LOAD_SMSTR((CL_FORM *)&Kvector_reader[0], ARG(5));	/* Vector longer than specified length: #~S~S */
					COPY(ARG(2), ARG(6));
					COPY(ARG(3), ARG(7));
					Ferror(ARG(5), 3);
				}
				COPY(ARG(2), ARG(5));
				Fmake_array(ARG(5), 1);
				COPY(ARG(3), ARG(6));
				LOAD_FIXNUM(ARG(7), 0, ARG(7));
				LOAD_NIL(ARG(8));
				LOAD_FIXNUM(ARG(9), 0, ARG(9));
				LOAD_NIL(ARG(10));
				replace1(ARG(5));
				COPY(ARG(3), ARG(6));
				LOAD_FIXNUM(ARG(7), 1, ARG(7));
				last1(ARG(6));
				if(CL_CONSP(ARG(6)))
				{
					COPY(GET_CAR(ARG(6)), ARG(6));
				}
				else
				{
					if(CL_TRUEP(ARG(6)))
					{
						LOAD_SMSTR((CL_FORM *)&KClisp[264], ARG(7));	/* ~a is not a list */
						COPY(ARG(6), ARG(8));
						Ferror(ARG(7), 2);
					}
					else
					{
					}
				}
				COPY(ARG(5), ARG(0));
				COPY(ARG(6), ARG(1));
				COPY(ARG(4), ARG(2));
				LOAD_NIL(ARG(3));
				fill1(ARG(0));
			}
			else
			{
				LOAD_GLOBFUN(&CFvector, ARG(0));
				COPY(ARG(3), ARG(1));
				Fapply(ARG(0), 2);
			}
		}
		else
		{
			if(CL_TRUEP(ARG(2)))
			{
				LOAD_SMSTR((CL_FORM *)&Kvector_reader[2], ARG(0));	/* #~s( syntax is not allowed in backquoted expressions */
				COPY(ARG(2), ARG(1));
				Ferror(ARG(0), 2);
			}
			else
			{
				ALLOC_CONS(ARG(5), SYMVAL(Slisp, 446), ARG(3), ARG(0));	/* *BQ-VECTOR* */
			}
		}
	}
}
