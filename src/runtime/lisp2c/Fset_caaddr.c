/* This file was generated by CLiCC 0.6.4 [obrep 1] */
#define __OBREP 1
#include <c_decl.h>
#include "sys.h"
#include "lisp.h"

void Fset_caaddr(CL_FORM *base)
{
	Fcdr(ARG(1));
	Fcdr(ARG(1));
	Fcar(ARG(1));
	Fset_car(ARG(0));
}