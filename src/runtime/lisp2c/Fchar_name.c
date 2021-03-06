/* This file was generated by CLiCC 0.6.4 [obrep 1] */
#define __OBREP 1
#include <c_decl.h>
#include "sys.h"
#include "lisp.h"

void Fchar_name(CL_FORM *base)
{
	if(CL_CHARP(ARG(0)))
	{
	}
	else
	{
		COPY(SYMVAL(Slisp, 58), ARG(1));	/* WRONG_TYPE */
		COPY(ARG(0), ARG(2));
		LOAD_SYMBOL(SYMBOL(Slisp, 18), ARG(3));	/* CHARACTER */
		Ferror(ARG(1), 3);
	}
	COPY(ARG(0), ARG(1));
	LOAD_CHAR(ARG(2), ' ', ARG(2));
	rt_charE(ARG(1));
	if(CL_TRUEP(ARG(1)))
	{
		LOAD_SMSTR((CL_FORM *)&KClisp[180], ARG(0));	/* Space */
	}
	else
	{
		COPY(ARG(0), ARG(1));
		LOAD_CHAR(ARG(2), '\n', ARG(2));
		rt_charE(ARG(1));
		if(CL_TRUEP(ARG(1)))
		{
			LOAD_SMSTR((CL_FORM *)&KClisp[178], ARG(0));	/* Newline */
		}
		else
		{
			COPY(ARG(0), ARG(1));
			LOAD_CHAR(ARG(2), '\b', ARG(2));
			rt_charE(ARG(1));
			if(CL_TRUEP(ARG(1)))
			{
				LOAD_SMSTR((CL_FORM *)&KClisp[176], ARG(0));	/* Backspace */
			}
			else
			{
				COPY(ARG(0), ARG(1));
				LOAD_CHAR(ARG(2), '\t', ARG(2));
				rt_charE(ARG(1));
				if(CL_TRUEP(ARG(1)))
				{
					LOAD_SMSTR((CL_FORM *)&KClisp[174], ARG(0));	/* Tab */
				}
				else
				{
					COPY(ARG(0), ARG(1));
					LOAD_CHAR(ARG(2), '\n', ARG(2));
					rt_charE(ARG(1));
					if(CL_TRUEP(ARG(1)))
					{
						LOAD_SMSTR((CL_FORM *)&KClisp[172], ARG(0));	/* Linefeed */
					}
					else
					{
						COPY(ARG(0), ARG(1));
						LOAD_CHAR(ARG(2), '\f', ARG(2));
						rt_charE(ARG(1));
						if(CL_TRUEP(ARG(1)))
						{
							LOAD_SMSTR((CL_FORM *)&KClisp[170], ARG(0));	/* Page */
						}
						else
						{
							COPY(ARG(0), ARG(1));
							LOAD_CHAR(ARG(2), '\r', ARG(2));
							rt_charE(ARG(1));
							if(CL_TRUEP(ARG(1)))
							{
								LOAD_SMSTR((CL_FORM *)&KClisp[168], ARG(0));	/* Return */
							}
							else
							{
								COPY(ARG(0), ARG(1));
								LOAD_CHAR(ARG(2), '\177', ARG(2));
								rt_charE(ARG(1));
								if(CL_TRUEP(ARG(1)))
								{
									LOAD_SMSTR((CL_FORM *)&KClisp[166], ARG(0));	/* Rubout */
								}
								else
								{
									LOAD_NIL(ARG(0));
								}
							}
						}
					}
				}
			}
		}
	}
}
