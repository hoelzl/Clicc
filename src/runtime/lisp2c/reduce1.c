/* This file was generated by CLiCC 0.6.4 [obrep 1] */
#define __OBREP 1
#include <c_decl.h>
#include "sys.h"
#include "lisp.h"

void reduce1(CL_FORM *base)
{
	COPY(ARG(1), ARG(7));
	Flength(ARG(7));
	COPY(ARG(3), ARG(8));
	COPY(ARG(4), ARG(9));
	COPY(ARG(7), ARG(10));
	check_seq_start_end(ARG(8));
	COPY(ARG(8), ARG(4));
	COPY(ARG(7), ARG(8));
	Fzerop(ARG(8));
	if(CL_TRUEP(ARG(8)))
	{
		if(CL_TRUEP(ARG(6)))
		{
			COPY(ARG(5), ARG(0));
		}
		else
		{
			Ffuncall(ARG(0), 1);
		}
	}
	else
	{
		COPY(ARG(7), ARG(8));
		LOAD_FIXNUM(ARG(9), 1, ARG(9));
		Fnumeql(ARG(8), 2);
		if(CL_TRUEP(ARG(8)))
		{
			if(CL_TRUEP(ARG(6)))
			{
				goto ELSE1;
			}
			else
			{
				goto THEN2;
			}
		}
		else
		{
			goto ELSE1;
		}
		{
			THEN2:;
			COPY(ARG(1), ARG(0));
			LOAD_FIXNUM(ARG(1), 0, ARG(1));
			Felt(ARG(0));
		}
		goto ENDIF3;
		{
			ELSE1:;
			if(CL_TRUEP(ARG(2)))
			{
				F1minus(ARG(4));
				if(CL_TRUEP(ARG(6)))
				{
				}
				else
				{
					COPY(ARG(1), ARG(8));
					COPY(ARG(4), ARG(9));
					Felt(ARG(8));
					COPY(ARG(8), ARG(5));
					F1minus(ARG(4));
				}
				COPY(ARG(5), ARG(8));
				M1_1:;
				COPY(ARG(3), ARG(9));
				COPY(ARG(4), ARG(10));
				Fge(ARG(9), 2);
				if(CL_TRUEP(ARG(9)))
				{
					COPY(ARG(8), ARG(0));
					goto RETURN1;
				}
				COPY(ARG(0), ARG(9));
				COPY(ARG(1), ARG(10));
				COPY(ARG(4), ARG(11));
				Felt(ARG(10));
				COPY(ARG(8), ARG(11));
				Ffuncall(ARG(9), 3);
				mv_count = 1;
				COPY(ARG(9), ARG(8));
				F1minus(ARG(4));
				goto M1_1;
				RETURN1:;
			}
			else
			{
				if(CL_TRUEP(ARG(6)))
				{
				}
				else
				{
					COPY(ARG(1), ARG(8));
					COPY(ARG(3), ARG(9));
					Felt(ARG(8));
					COPY(ARG(8), ARG(5));
					F1plus(ARG(3));
				}
				COPY(ARG(5), ARG(8));
				M2_1:;
				COPY(ARG(3), ARG(9));
				COPY(ARG(4), ARG(10));
				Fge(ARG(9), 2);
				if(CL_TRUEP(ARG(9)))
				{
					COPY(ARG(8), ARG(0));
					goto RETURN2;
				}
				COPY(ARG(0), ARG(9));
				COPY(ARG(8), ARG(10));
				COPY(ARG(1), ARG(11));
				COPY(ARG(3), ARG(12));
				Felt(ARG(11));
				Ffuncall(ARG(9), 3);
				mv_count = 1;
				COPY(ARG(9), ARG(8));
				F1plus(ARG(3));
				goto M2_1;
				RETURN2:;
			}
		}
		ENDIF3:;
	}
}
