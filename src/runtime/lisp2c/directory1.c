/* This file was generated by CLiCC 0.6.4 [obrep 1] */
#define __OBREP 1
#include <c_decl.h>
#include "sys.h"
#include "lisp.h"

CL_INIT Kdirectory1[] =
{
	MAKE_STRING(1, "/"),	/* 0 */
};

static void Z52_lambda(CL_FORM *base);
static void Z53_g663(CL_FORM *base);

void directory1(CL_FORM *base)
{
	GEN_HEAPVAR(ARG(1), ARG(4));
	GEN_HEAPVAR(ARG(2), ARG(4));
	GEN_HEAPVAR(ARG(3), ARG(4));
	LOAD_NIL(ARG(4));
	GEN_HEAPVAR(ARG(4), ARG(5));
	LOAD_SYMBOL(SYMBOL(Slisp, 254), ARG(5));	/* NAME */
	LOAD_SYMBOL(SYMBOL(Slisp, 271), ARG(6));	/* WILD */
	LOAD_SYMBOL(SYMBOL(Slisp, 80), ARG(7));	/* TYPE */
	LOAD_SYMBOL(SYMBOL(Slisp, 271), ARG(8));	/* WILD */
	LOAD_SYMBOL(SYMBOL(Slisp, 255), ARG(9));	/* VERSION */
	LOAD_SYMBOL(SYMBOL(Slisp, 271), ARG(10));	/* WILD */
	Fmake_pathname(ARG(5), 6);
	COPY(ARG(0), ARG(6));
	COPY(ARG(5), ARG(7));
	LOAD_SYMBOL(SYMBOL(Slisp, 269), ARG(8));	/* NEWEST */
	merge_pathnames1(ARG(6));
	COPY(ARG(6), ARG(5));
	{
		GEN_CLOSURE(array, ARG(6), 5, Z53_g663, 1);
		COPY(ARG(4), &array[3]);
		COPY(ARG(1), &array[4]);
		LOAD_CLOSURE(array, ARG(6));
	}
	COPY(ARG(5), ARG(7));
	Fpathname(ARG(7));
	LOAD_SYMBOL(SYMBOL(Slisp, 48), ARG(8));	/* T */
	COPY(ARG(6), ARG(9));
	Penumerate_matches(ARG(7));
	mv_count = 1;
	LOAD_SYMBOL(SYMBOL(Slisp, 48), ARG(5));	/* T */
	BIND_SPECIAL(SYMBOL(Slisp, 287), ARG(5));	/* *IGNORE-WILDCARDS* */
	{
		GEN_CLOSURE(array, ARG(6), 5, Z52_lambda, 1);
		COPY(ARG(3), &array[3]);
		COPY(ARG(2), &array[4]);
		LOAD_CLOSURE(array, ARG(6));
	}
	COPY(ARG(6), ARG(6));
	LOAD_SYMBOL(SYMBOL(Slisp, 282), ARG(7));	/* TEST */
	LOAD_GLOBFUN(&CFstringE, ARG(8));
	Flist(ARG(7), 2);
	LOAD_GLOBFUN(&CFremove_duplicates, ARG(8));
	COPY(INDIRECT(ARG(4)), ARG(9));
	COPY(ARG(7), ARG(10));
	Fapply(ARG(8), 3);
	mv_count = 1;
	COPY(ARG(8), ARG(7));
	if(CL_LISTP(ARG(8)))
	{
		COPY(ARG(8), ARG(7));
		LOAD_GLOBFUN(&CFstringL, ARG(8));
		LOAD_GLOBFUN(&CFidentity, ARG(9));
		list_merge_sort(ARG(7));
	}
	else
	{
		COPY(ARG(8), ARG(9));
		LOAD_FIXNUM(ARG(10), 0, ARG(10));
		COPY(ARG(8), ARG(11));
		Flength(ARG(11));
		LOAD_GLOBFUN(&CFstringL, ARG(12));
		LOAD_GLOBFUN(&CFidentity, ARG(13));
		quick_sort(ARG(9));
		COPY(ARG(9), ARG(7));
	}
	Fmapcar(ARG(6), 2);
	COPY(ARG(6), ARG(0));
	RESTORE_SPECIAL;
}

static void Z52_lambda(CL_FORM *base)
{
	if(CL_TRUEP(INDIRECT(GET_FORM(ARG(0)) + 4)))
	{
		COPY(ARG(1), ARG(2));
		LOAD_NIL(ARG(3));
		unix_file_kind1(ARG(2));
	}	/* DIRECTORY */
	else
	{
		goto ELSE1;
	}
	if(CL_SYMBOLP(ARG(2)) && GET_SYMBOL(ARG(2)) == SYMBOL(Slisp, 253))
	{
		LOAD_SYMBOL(SYMBOL(Slisp, 44), ARG(2));	/* STRING */
		COPY(ARG(1), ARG(3));
		LOAD_SMSTR((CL_FORM *)&Kdirectory1[0], ARG(4));	/* / */
		Fconcatenate(ARG(2), 3);
	}
	else
	{
		ELSE1:;
		COPY(ARG(1), ARG(2));
	}
	if(CL_TRUEP(INDIRECT(GET_FORM(ARG(0)) + 3)))
	{
		COPY(ARG(2), ARG(3));
		Fprobe_file(ARG(3));
		if(CL_TRUEP(ARG(3)))
		{
		}
		else
		{
			LOAD_SMSTR((CL_FORM *)&KClisp[242], ARG(4));	/* The file ~S does not exist. */
			COPY(ARG(2), ARG(5));
			Fnamestring(ARG(5));
			mv_count = 1;
			Ferror(ARG(4), 2);
		}
		COPY(ARG(3), ARG(0));
	}
	else
	{
		COPY(ARG(2), ARG(3));
		Fpathname(ARG(3));
		COPY(ARG(3), ARG(0));
	}
}

static void Z53_g663(CL_FORM *base)
{
	if(CL_TRUEP(INDIRECT(GET_FORM(ARG(0)) + 4)))
	{
		goto THEN1;
	}
	else
	{
		LOAD_CHAR(ARG(2), '/', ARG(2));
		COPY(ARG(1), ARG(3));
		LOAD_SYMBOL(SYMBOL(Slisp, 294), ARG(4));	/* FROM-END */
		LOAD_SYMBOL(SYMBOL(Slisp, 48), ARG(5));	/* T */
		Fposition(ARG(2), 4);
		if(CL_TRUEP(ARG(2)))
		{
			LOAD_NIL(ARG(3));
		}
		else
		{
			LOAD_SYMBOL(SYMBOL(Slisp, 48), ARG(3));	/* T */
		}
		if(CL_TRUEP(ARG(3)))
		{
			goto THEN1;
		}
		else
		{
			COPY(ARG(2), ARG(4));
			F1plus(ARG(4));
			COPY(ARG(1), ARG(5));
			Flength(ARG(5));
			Fnumeql(ARG(4), 2);
			if(CL_TRUEP(ARG(4)))
			{
				goto THEN1;
			}
			else
			{
				COPY(ARG(2), ARG(5));
				F1plus(ARG(5));
				if(CL_SMSTRP(ARG(1)))
				{
				}
				else
				{
					COPY(SYMVAL(Slisp, 58), ARG(6));	/* WRONG_TYPE */
					COPY(ARG(1), ARG(7));
					LOAD_SYMBOL(SYMBOL(Slisp, 40), ARG(8));	/* SIMPLE-STRING */
					Ferror(ARG(6), 3);
				}
				if(CL_FIXNUMP(ARG(5)))
				{
					LOAD_FIXNUM(ARG(6), 0, ARG(6));
					COPY(ARG(5), ARG(7));
					LOAD_FIXNUM(ARG(9), AR_SIZE(GET_FORM(ARG(1))), ARG(8));
					F1minus(ARG(8));
					Fle(ARG(6), 3);
				}
				else
				{
					goto ELSE2;
				}
				if(CL_TRUEP(ARG(6)))
				{
				}
				else
				{
					ELSE2:;
					COPY(SYMVAL(Slisp, 153), ARG(6));	/* OUT_OF_RANGE */
					COPY(ARG(5), ARG(7));
					LOAD_FIXNUM(ARG(9), AR_SIZE(GET_FORM(ARG(1))), ARG(8));
					Ferror(ARG(6), 3);
				}
				COPY(ARG(1), ARG(6));
				COPY(ARG(5), ARG(7));
				rt_pvref(ARG(6));
				COPY(ARG(6), ARG(5));
				LOAD_CHAR(ARG(6), '.', ARG(6));
				rt_charNE(ARG(5));
			}
		}
	}
	if(CL_TRUEP(ARG(5)))
	{
		THEN1:;
		ALLOC_CONS(ARG(3), ARG(1), INDIRECT(GET_FORM(ARG(0)) + 3), INDIRECT(GET_FORM(ARG(0)) + 3));
		COPY(INDIRECT(GET_FORM(ARG(0)) + 3), ARG(0));
	}
	else
	{
		LOAD_NIL(ARG(0));
	}
}
