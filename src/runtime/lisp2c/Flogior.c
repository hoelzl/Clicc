/* This file was generated by CLiCC 0.6.4 [obrep 1] */
#define __OBREP 1
#include <c_decl.h>
#include "sys.h"
#include "lisp.h"

void Flogior(CL_FORM *base, int nargs)
{
	Flist(ARG(0), nargs - 0);
	LOAD_FIXNUM(ARG(1), 0, ARG(1));
	M1_1:;
	if(CL_TRUEP(ARG(0)))
	{
	}
	else
	{
		COPY(ARG(1), ARG(0));
		goto RETURN1;
	}
	COPY(ARG(0), ARG(3));
	COPY(ARG(3), ARG(4));
	if(CL_CONSP(ARG(4)))
	{
		COPY(GET_CAR(ARG(4)), ARG(3));
	}
	else
	{
		LOAD_SMSTR((CL_FORM *)&KClisp[264], ARG(3));	/* ~a is not a list */
		Ferror(ARG(3), 2);
	}
	COPY(ARG(0), ARG(4));
	COPY(ARG(4), ARG(5));
	if(CL_CONSP(ARG(5)))
	{
		COPY(GET_CDR(ARG(5)), ARG(0));
	}
	else
	{
		LOAD_SMSTR((CL_FORM *)&KClisp[262], ARG(6));	/* ~a is not a list */
		COPY(ARG(5), ARG(7));
		Ferror(ARG(6), 2);
	}
	if(CL_FIXNUMP(ARG(3)))
	{
	}
	else
	{
		COPY(ARG(3), ARG(4));
		LOAD_SMSTR((CL_FORM *)&KClisp[244], ARG(3));	/* type error: ~S is not of type ~S */
		LOAD_SYMBOL(SYMBOL(Slisp, 23), ARG(5));	/* FIXNUM */
		Ferror(ARG(3), 3);
	}
	LOAD_FIXNUM(ARG(4), GET_FIXNUM(ARG(1)) | GET_FIXNUM(ARG(3)), ARG(1));
	goto M1_1;
	RETURN1:;
}
