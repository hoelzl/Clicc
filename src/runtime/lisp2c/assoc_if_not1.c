/* This file was generated by CLiCC 0.6.4 [obrep 1] */
#define __OBREP 1
#include <c_decl.h>
#include "sys.h"
#include "lisp.h"

void assoc_if_not1(CL_FORM *base)
{
	LOAD_NIL(ARG(3));
	COPY(ARG(1), ARG(4));
	M1_1:;
	if(CL_ATOMP(ARG(4)))
	{
		LOAD_NIL(ARG(3));
		COPY(ARG(3), ARG(0));
		goto RETURN2;
	}
	COPY(ARG(4), ARG(5));
	COPY(GET_CAR(ARG(5)), ARG(3));
	COPY(ARG(0), ARG(5));
	if(CL_TRUEP(ARG(2)))
	{
		COPY(ARG(2), ARG(6));
		if(CL_CONSP(ARG(3)))
		{
			COPY(GET_CAR(ARG(3)), ARG(7));
		}
		else
		{
			if(CL_TRUEP(ARG(3)))
			{
				LOAD_SMSTR((CL_FORM *)&KClisp[264], ARG(7));	/* ~a is not a list */
				COPY(ARG(3), ARG(8));
				Ferror(ARG(7), 2);
			}
			else
			{
				COPY(ARG(3), ARG(7));
			}
		}
		Ffuncall(ARG(6), 2);
		mv_count = 1;
	}
	else
	{
		if(CL_CONSP(ARG(3)))
		{
			COPY(GET_CAR(ARG(3)), ARG(6));
		}
		else
		{
			if(CL_TRUEP(ARG(3)))
			{
				LOAD_SMSTR((CL_FORM *)&KClisp[264], ARG(6));	/* ~a is not a list */
				COPY(ARG(3), ARG(7));
				Ferror(ARG(6), 2);
			}
			else
			{
				COPY(ARG(3), ARG(6));
			}
		}
	}
	Ffuncall(ARG(5), 2);
	mv_count = 1;
	if(CL_TRUEP(ARG(5)))
	{
	}
	else
	{
		COPY(ARG(3), ARG(0));
		goto RETURN1;
	}
	COPY(ARG(4), ARG(5));
	COPY(GET_CDR(ARG(5)), ARG(4));
	goto M1_1;
	RETURN2:;
	RETURN1:;
}
