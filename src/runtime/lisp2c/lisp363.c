/* This file was generated by CLiCC 0.6.4 [obrep 1] */
#define __OBREP 1
#include <c_decl.h>
#include "sys.h"
#include "lisp.h"

GEN_GLOBAL_FUNARG(Cunparse_unix_host, unparse_unix_host, 1);

CL_INIT Kunparse_unix_host[] =
{
	MAKE_STRING(4, "Unix"),	/* 0 */
};

void unparse_unix_host(CL_FORM *base)
{
	LOAD_SMSTR((CL_FORM *)&Kunparse_unix_host[0], ARG(0));	/* Unix */
}
