/* This file was generated by CLiCC 0.6.4 [obrep 1] */
#define __OBREP 1
#include <c_decl.h>
#include "sys.h"
#include "lisp.h"

GEN_GLOBAL_FUNARG(Cunparse_unix_namestring, unparse_unix_namestring, 1);

void unparse_unix_namestring(CL_FORM *base)
{
	LOAD_SYMBOL(SYMBOL(Slisp, 40), ARG(1));	/* SIMPLE-STRING */
	COPY(ARG(0), ARG(2));
	Ppathname_directory(ARG(2));
	unparse_unix_directory_list(ARG(2));
	COPY(ARG(0), ARG(3));
	unparse_unix_file(ARG(3));
	Fconcatenate(ARG(1), 3);
	COPY(ARG(1), ARG(0));
}
