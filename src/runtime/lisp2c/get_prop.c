/* This file was generated by CLiCC 0.6.4 [obrep 1] */
#define __OBREP 1
#include <c_decl.h>
#include "sys.h"
#include "lisp.h"

void get_prop(CL_FORM *base)
{
	M1_1:;
	if(CL_ATOMP(ARG(0)))
	{
		LOAD_NIL(ARG(0));
	}
	else
	{
		COPY(GET_CAR(ARG(0)), ARG(2));
		if(EQ(ARG(2), ARG(1)))
		{
		}
		else
		{
			COPY(ARG(0), ARG(2));
			COPY(ARG(2), ARG(3));
			COPY(GET_CDR(ARG(3)), ARG(3));
			if(CL_CONSP(ARG(3)))
			{
				COPY(GET_CDR(ARG(3)), ARG(0));
			}
			else
			{
				if(CL_TRUEP(ARG(3)))
				{
					LOAD_SMSTR((CL_FORM *)&KClisp[262], ARG(4));	/* ~a is not a list */
					COPY(ARG(3), ARG(5));
					Ferror(ARG(4), 2);
				}
				else
				{
					COPY(ARG(3), ARG(0));
				}
			}
			goto M1_1;
		}
	}
	goto RETURN1;
	RETURN1:;
}
