/* This file was generated by CLiCC 0.6.4 [obrep 1] */
#define __OBREP 1
#include <c_decl.h>
#include "sys.h"
#include "lisp.h"

void compare_component(CL_FORM *base)
{
	M1_1:;
	LOAD_BOOL(EQL(ARG(0), ARG(1)), ARG(2));
	if(CL_TRUEP(ARG(2)))
	{
		COPY(ARG(2), ARG(0));
	}
	else
	{
		if(CL_SMSTRP(ARG(0)))
		{
			if(CL_SMSTRP(ARG(1)))
			{
				LOAD_FIXNUM(ARG(2), 0, ARG(2));
				LOAD_NIL(ARG(3));
				LOAD_FIXNUM(ARG(4), 0, ARG(4));
				LOAD_NIL(ARG(5));
				stringE1(ARG(0));
			}
			else
			{
				LOAD_NIL(ARG(0));
			}
		}
		else
		{
			COPY(ARG(0), ARG(3));
			LOAD_SYMBOL(SYMBOL(Slisp, 250), ARG(4));	/* PATTERN */
			rt_struct_typep(ARG(3));
			if(CL_TRUEP(ARG(3)))
			{
				COPY(ARG(1), ARG(3));
				LOAD_SYMBOL(SYMBOL(Slisp, 250), ARG(4));	/* PATTERN */
				rt_struct_typep(ARG(3));
				if(CL_TRUEP(ARG(3)))
				{
					patternE(ARG(0));
				}
				else
				{
					LOAD_NIL(ARG(0));
				}
			}
			else
			{
				if(CL_CONSP(ARG(0)))
				{
					if(CL_CONSP(ARG(1)))
					{
						COPY(GET_CAR(ARG(0)), ARG(3));
						COPY(GET_CAR(ARG(1)), ARG(4));
						compare_component(ARG(3));
						if(CL_TRUEP(ARG(3)))
						{
							COPY(GET_CDR(ARG(0)), ARG(3));
							COPY(GET_CDR(ARG(1)), ARG(4));
							COPY(ARG(3), ARG(0));
							COPY(ARG(4), ARG(1));
							goto M1_1;
						}
						else
						{
							LOAD_NIL(ARG(0));
						}
					}
					else
					{
						LOAD_NIL(ARG(0));
					}
				}
				else
				{
					LOAD_NIL(ARG(0));
				}
			}
		}
	}
	goto RETURN1;
	RETURN1:;
}
