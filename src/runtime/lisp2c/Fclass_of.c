/* This file was generated by CLiCC 0.6.4 [obrep 1] */
#define __OBREP 1
#include <c_decl.h>
#include "sys.h"
#include "lisp.h"

void Fclass_of(CL_FORM *base)
{
	if(CL_INSTANCEP(ARG(0)))
	{
		COPY(OFFSET(AR_BASE(GET_FORM(ARG(0))), -1 + 1), ARG(0));
	}
	else
	{
		LOAD_SMSTR((CL_FORM *)&KClisp[238], ARG(1));	/* ~S ist not a valid argument for CLASS-OF, ~
              these have been restricted to instances of user-defined-classes. */
		COPY(ARG(0), ARG(2));
		Ferror(ARG(1), 2);
	}
}
