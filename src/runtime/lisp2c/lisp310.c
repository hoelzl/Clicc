/* This file was generated by CLiCC 0.6.4 [obrep 1] */
#define __OBREP 1
#include <c_decl.h>
#include "sys.h"
#include "lisp.h"

void Fset_slot_value(CL_FORM *base)
{
	if(CL_INSTANCEP(ARG(1)))
	{
		COPY(OFFSET(AR_BASE(GET_FORM(ARG(1))), -1 + 1), ARG(3));
	}
	else
	{
		LOAD_SMSTR((CL_FORM *)&KClisp[238], ARG(3));	/* ~S ist not a valid argument for CLASS-OF, ~
              these have been restricted to instances of user-defined-classes. */
		COPY(ARG(1), ARG(4));
		Ferror(ARG(3), 2);
	}
	COPY(OFFSET(AR_BASE(GET_FORM(ARG(3))), 3 + 1), ARG(3));
	COPY(ARG(2), ARG(4));
	COPY(ARG(3), ARG(5));
	LOAD_SYMBOL(SYMBOL(Slisp, 209), ARG(6));	/* KEY */
	LOAD_GLOBFUN(&CFthird, ARG(7));
	Fposition(ARG(4), 4);
	if(CL_TRUEP(ARG(4)))
	{
		COPY(ARG(0), OFFSET(AR_BASE(GET_FORM(ARG(1))), GET_FIXNUM(ARG(4)) + 1));
	}
	if(CL_INSTANCEP(ARG(1)))
	{
		COPY(OFFSET(AR_BASE(GET_FORM(ARG(1))), -1 + 1), ARG(5));
	}
	else
	{
		LOAD_SMSTR((CL_FORM *)&KClisp[238], ARG(5));	/* ~S ist not a valid argument for CLASS-OF, ~
              these have been restricted to instances of user-defined-classes. */
		COPY(ARG(1), ARG(6));
		Ferror(ARG(5), 2);
	}
	LOAD_CONS((CL_FORM *)&KClisp[140], ARG(6));
	LOAD_SMSTR((CL_FORM *)&KClisp[234], ARG(7));	/* ~S: The slot ~s is missing from the object ~s of class ~s. */
	COPY(ARG(6), ARG(8));
	COPY(ARG(2), ARG(9));
	COPY(ARG(1), ARG(10));
	COPY(ARG(5), ARG(11));
	Ferror(ARG(7), 5);
}
