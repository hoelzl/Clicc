/* This file was generated by CLiCC 0.6.4 [obrep 1] */
#define __OBREP 1
#include <c_decl.h>
#include "sys.h"
#include "lisp.h"

void Fmismatch(CL_FORM *base, int nargs)
{
	BOOL supl_flags[8];
	static CL_FORM * keylist[] =
	{
		SYMBOL(Slisp, 294),	/* FROM-END */
		SYMBOL(Slisp, 282),	/* TEST */
		SYMBOL(Slisp, 550),	/* TEST-NOT */
		SYMBOL(Slisp, 209),	/* KEY */
		SYMBOL(Slisp, 302),	/* START1 */
		SYMBOL(Slisp, 280),	/* START2 */
		SYMBOL(Slisp, 303),	/* END1 */
		SYMBOL(Slisp, 281),	/* END2 */
	};
	keysort(ARG(2), nargs - 2, 8, keylist, supl_flags, FALSE);
	if(NOT(supl_flags[0]))
	{
		LOAD_NIL(ARG(2));
	}
	if(NOT(supl_flags[1]))
	{
		LOAD_NIL(ARG(3));
	}
	if(NOT(supl_flags[2]))
	{
		LOAD_NIL(ARG(4));
	}
	if(NOT(supl_flags[3]))
	{
		LOAD_GLOBFUN(&CFidentity, ARG(5));
	}
	if(NOT(supl_flags[4]))
	{
		LOAD_FIXNUM(ARG(10), 0, ARG(6));
	}
	if(NOT(supl_flags[5]))
	{
		LOAD_FIXNUM(ARG(10), 0, ARG(7));
	}
	if(NOT(supl_flags[6]))
	{
		LOAD_NIL(ARG(8));
	}
	if(NOT(supl_flags[7]))
	{
		LOAD_NIL(ARG(9));
	}
	mismatch1(ARG(0));
}
