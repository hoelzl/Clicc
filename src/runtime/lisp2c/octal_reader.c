/* This file was generated by CLiCC 0.6.4 [obrep 1] */
#define __OBREP 1
#include <c_decl.h>
#include "sys.h"
#include "lisp.h"

GEN_GLOBAL_FUNARG(Coctal_reader, octal_reader, 3);

void octal_reader(CL_FORM *base)
{
	if(CL_TRUEP(ARG(2)))
	{
		LOAD_SMSTR((CL_FORM *)&KClisp[230], ARG(3));	/* extra argument for #~S */
		COPY(ARG(1), ARG(4));
		Ferror(ARG(3), 2);
	}
	LOAD_FIXNUM(ARG(2), 8, ARG(2));
	radix_reader(ARG(0));
}
