/* This file was generated by CLiCC 0.6.4 [obrep 1] */
#define __OBREP 1
#include <c_decl.h>
#include "sys.h"
#include "lisp.h"

GEN_GLOBAL_FUNARG(Cclose_err, close_err, -1);

CL_INIT Kclose_err[] =
{
	MAKE_STRING(16, "stream is closed"),	/* 0 */
};

void close_err(CL_FORM *base, int nargs)
{
	LOAD_SMSTR((CL_FORM *)&Kclose_err[0], ARG(0));	/* stream is closed */
	Ferror(ARG(0), 1);
}
