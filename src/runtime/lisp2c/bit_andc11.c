/* This file was generated by CLiCC 0.6.4 [obrep 1] */
#define __OBREP 1
#include <c_decl.h>
#include "sys.h"
#include "lisp.h"

void bit_andc11(CL_FORM *base)
{
	LOAD_FIXNUM(ARG(3), 6, ARG(3));
	COPY(ARG(0), ARG(4));
	COPY(ARG(1), ARG(5));
	COPY(ARG(2), ARG(6));
	bit_op(ARG(3));
	COPY(ARG(3), ARG(0));
}
