/* This file was generated by CLiCC 0.6.4 [obrep 1] */
#define __OBREP 1
#include <c_decl.h>
#include "sys.h"
#include "lisp.h"

void Fhash_table_rehash_size(CL_FORM *base)
{
	LOAD_FIXNUM(ARG(1), 2, ARG(1));
	LOAD_SYMBOL(SYMBOL(Slisp, 399), ARG(2));	/* HASH-TABLE */
	rt_struct_ref(ARG(0));
}