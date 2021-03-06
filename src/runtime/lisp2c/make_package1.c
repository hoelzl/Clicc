/* This file was generated by CLiCC 0.6.4 [obrep 1] */
#define __OBREP 1
#include <c_decl.h>
#include "sys.h"
#include "lisp.h"

CL_INIT Kmake_package1[] =
{
	MAKE_STRING(35, "package with name ~S already exists"),	/* 0 */
};

void make_package1(CL_FORM *base)
{
	LOAD_NIL(ARG(3));
	if(CL_SYMBOLP(ARG(0)) || CL_NILP(ARG(0)))
	{
		COPY(ARG(0), ARG(4));
		if(CL_SYMBOLP(ARG(4)))
		{
			LOAD_SMSTR(SYM_NAME(ARG(4)), ARG(0));
		}
		else
		{
			LOAD_SMSTR((CL_FORM *)&KClisp[266], ARG(0));	/* NIL */
		}
	}
	COPY(ARG(0), ARG(4));
	Ffind_package(ARG(4));
	if(CL_TRUEP(ARG(4)))
	{
		LOAD_SMSTR((CL_FORM *)&Kmake_package1[0], ARG(4));	/* package with name ~S already exists */
		COPY(ARG(0), ARG(5));
		Ferror(ARG(4), 2);
	}
	COPY(ARG(0), ARG(4));
	LOAD_FIXNUM(ARG(5), 101, ARG(5));
	LOAD_SYMBOL(SYMBOL(Slisp, 28), ARG(6));	/* LIST */
	LOAD_NIL(ARG(7));
	LOAD_NIL(ARG(8));
	LOAD_NIL(ARG(9));
	LOAD_NIL(ARG(10));
	LOAD_NIL(ARG(11));
	LOAD_FIXNUM(ARG(12), 0, ARG(12));
	LOAD_SYMBOL(SYMBOL(Slisp, 48), ARG(13));	/* T */
	LOAD_NIL(ARG(14));
	LOAD_NIL(ARG(15));
	make_array1(ARG(5));
	LOAD_FIXNUM(ARG(6), 101, ARG(6));
	LOAD_SYMBOL(SYMBOL(Slisp, 28), ARG(7));	/* LIST */
	LOAD_NIL(ARG(8));
	LOAD_NIL(ARG(9));
	LOAD_NIL(ARG(10));
	LOAD_NIL(ARG(11));
	LOAD_NIL(ARG(12));
	LOAD_FIXNUM(ARG(13), 0, ARG(13));
	LOAD_SYMBOL(SYMBOL(Slisp, 48), ARG(14));	/* T */
	LOAD_NIL(ARG(15));
	LOAD_NIL(ARG(16));
	make_array1(ARG(6));
	COPY(ARG(4), ARG(7));
	COPY(ARG(5), ARG(8));
	COPY(ARG(6), ARG(9));
	LOAD_SYMBOL(SYMBOL(Slisp, 354), ARG(3));	/* PACKAGE */
	COPY(ARG(7), ARG(4));
	LOAD_NIL(ARG(5));
	COPY(ARG(8), ARG(6));
	COPY(ARG(9), ARG(7));
	LOAD_NIL(ARG(8));
	LOAD_NIL(ARG(9));
	LOAD_NIL(ARG(10));
	rt_make_struct(ARG(3), 8);
	COPY(ARG(3), ARG(4));
	COPY(SYMVAL(Slisp, 389), ARG(5));	/* *PACKAGE-ARRAY* */
	LOAD_NIL(ARG(6));
	vector_push_extend1(ARG(4));
	COPY(ARG(2), ARG(4));
	COPY(ARG(3), ARG(5));
	use_package1(ARG(4));
	COPY(ARG(1), ARG(4));
	COPY(ARG(3), ARG(5));
	add_nicknames(ARG(4));
	COPY(ARG(3), ARG(0));
}
