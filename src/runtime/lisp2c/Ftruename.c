/* This file was generated by CLiCC 0.6.4 [obrep 1] */
#define __OBREP 1
#include <c_decl.h>
#include "sys.h"
#include "lisp.h"

void Ftruename(CL_FORM *base)
{
	COPY(ARG(0), ARG(1));
	Fprobe_file(ARG(1));
	if(CL_TRUEP(ARG(1)))
	{
	}
	else
	{
		LOAD_SMSTR((CL_FORM *)&KClisp[242], ARG(2));	/* The file ~S does not exist. */
		COPY(ARG(0), ARG(3));
		Fnamestring(ARG(3));
		mv_count = 1;
		Ferror(ARG(2), 2);
	}
	COPY(ARG(1), ARG(0));
}
