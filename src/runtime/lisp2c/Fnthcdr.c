/* This file was generated by CLiCC 0.6.4 [obrep 1] */
#define __OBREP 1
#include <c_decl.h>
#include "sys.h"
#include "lisp.h"

void Fnthcdr(CL_FORM *base)
{
	M1_1:;
	COPY(ARG(0), ARG(2));
	Fzerop(ARG(2));
	if(CL_TRUEP(ARG(2)))
	{
		COPY(ARG(1), ARG(0));
		goto RETURN1;
	}
	else
	{
		if(CL_TRUEP(ARG(1)))
		{
			F1minus(ARG(0));
			if(CL_CONSP(ARG(1)))
			{
				COPY(GET_CAR(ARG(1)), ARG(2));
			}
			else
			{
				LOAD_SMSTR((CL_FORM *)&KClisp[264], ARG(2));	/* ~a is not a list */
				COPY(ARG(1), ARG(3));
				Ferror(ARG(2), 2);
			}
			COPY(ARG(1), ARG(3));
			COPY(ARG(3), ARG(4));
			if(CL_CONSP(ARG(4)))
			{
				COPY(GET_CDR(ARG(4)), ARG(1));
			}
			else
			{
				LOAD_SMSTR((CL_FORM *)&KClisp[262], ARG(5));	/* ~a is not a list */
				COPY(ARG(4), ARG(6));
				Ferror(ARG(5), 2);
			}
		}
		else
		{
			LOAD_NIL(ARG(0));
			goto RETURN1;
		}
	}
	goto M1_1;
	RETURN1:;
}
