/* This file was generated by CLiCC 0.6.4 [obrep 1] */
#define __OBREP 1
#include <c_decl.h>
#include "sys.h"
#include "lisp.h"

void Fprin1_to_string(CL_FORM *base)
{
	Fmake_string_output_stream(ARG(1), 0);
	COPY(ARG(0), ARG(2));
	COPY(ARG(1), ARG(3));
	Fprin1(ARG(2), 2);
	COPY(ARG(1), ARG(0));
	Fget_output_stream_string(ARG(0));
}
