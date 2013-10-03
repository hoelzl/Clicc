/* This file was generated by CLiCC 0.6.4 [obrep 1] */
#define __OBREP 1
#include <c_decl.h>
#include "sys.h"
#include "lisp.h"

CL_INIT Kmake_array1[] =
{
	MAKE_STRING(140, "A dimension argument to MAKE-ARRAY, ~A, is unusable ~\n              since it is not a positive fixnum ~\n              less or equal than ~A."),	/* 0 */
	MAKE_STRING(94, "The :initial-element, ~A, is of the wrong type; ~\n                     it should be of type ~A"),	/* 2 */
	MAKE_STRING(57, ":initial-element and :initial-contents are both supplied."),	/* 4 */
	MAKE_STRING(168, "The :initial-contents argument, ~A,\n                             isn\'t the correct shape or size\n                             for an array of rank ~A with dimensions ~A"),	/* 6 */
	MAKE_STRING(43, "The :displaced-to argument is not an array."),	/* 8 */
	MAKE_STRING(59, "Cannot displace the array, because it has a different type."),	/* 10 */
	MAKE_STRING(97, "Cannot displace the array, ~\n                because the total size of the to-array is too small."),	/* 12 */
	MAKE_STRING(120, "A :displaced-index-offset argument is given to make-array, ~\n                        but there is no :displaced-to array"),	/* 14 */
	MAKE_STRING(58, "The displaced-to array is smaller than the displaced array"),	/* 16 */
	MAKE_STRING(147, "The :displaced-index-offset argument, ~A, ~\n                  is not in the linearized range [0, ~A) ~\n                  of the :displaced-to array"),	/* 18 */
	MAKE_STRING(59, "The fill-pointer ~A must be an integer in the range [0, ~A]"),	/* 20 */
	MAKE_STRING(48, "Fill-pointers are only for 1-dimensional arrays."),	/* 22 */
};

static void Z58_set_initial_contents(CL_FORM *base, CL_FORM *display[]);
static void Z59_chk_shape_of_init_cont(CL_FORM *base);

void make_array1(CL_FORM *base)
{
	CL_FORM *display[1];
	GEN_HEAPVAR(ARG(0), ARG(11));
	GEN_HEAPVAR(ARG(3), ARG(11));
	if(CL_ATOMP(INDIRECT(ARG(0))))
	{
		LOAD_NIL(ARG(12));
		ALLOC_CONS(ARG(13), INDIRECT(ARG(0)), ARG(12), INDIRECT(ARG(0)));
	}
	COPY(INDIRECT(ARG(0)), ARG(11));
	Flength(ARG(11));
	GEN_HEAPVAR(ARG(11), ARG(12));
	COPY(INDIRECT(ARG(0)), ARG(12));
	LOAD_NIL(ARG(13));
	LOAD_FIXNUM(ARG(14), 1, ARG(14));
	M1_1:;
	if(CL_TRUEP(ARG(12)))
	{
	}
	else
	{
		COPY(ARG(14), ARG(12));
		goto RETURN1;
	}
	COPY(ARG(12), ARG(15));
	COPY(ARG(15), ARG(16));
	if(CL_CONSP(ARG(16)))
	{
		COPY(GET_CAR(ARG(16)), ARG(15));
	}
	else
	{
		LOAD_SMSTR((CL_FORM *)&KClisp[264], ARG(15));	/* ~a is not a list */
		Ferror(ARG(15), 2);
	}
	COPY(ARG(12), ARG(16));
	COPY(ARG(16), ARG(17));
	if(CL_CONSP(ARG(17)))
	{
		COPY(GET_CDR(ARG(17)), ARG(12));
	}
	else
	{
		LOAD_SMSTR((CL_FORM *)&KClisp[262], ARG(18));	/* ~a is not a list */
		COPY(ARG(17), ARG(19));
		Ferror(ARG(18), 2);
	}
	COPY(ARG(15), ARG(13));
	if(CL_FIXNUMP(ARG(13)))
	{
		LOAD_FIXNUM(ARG(15), 0, ARG(15));
		COPY(ARG(13), ARG(16));
		COPY(SYMVAL(Slisp, 5), ARG(17));	/* ARRAY-DIMENSION-LIMIT */
		Fle(ARG(15), 3);
	}
	else
	{
		goto ELSE1;
	}
	if(CL_TRUEP(ARG(15)))
	{
	}
	else
	{
		ELSE1:;
		LOAD_SMSTR((CL_FORM *)&Kmake_array1[0], ARG(15));	/* A dimension argument to MAKE-ARRAY, ~A, is unusable ~
              since it is not a positive fixnum ~
              less or equal than ~A. */
		COPY(ARG(13), ARG(16));
		COPY(SYMVAL(Slisp, 5), ARG(17));	/* ARRAY-DIMENSION-LIMIT */
		Ferror(ARG(15), 3);
	}
	COPY(ARG(13), ARG(15));
	Fmult(ARG(14), 2);
	goto M1_1;
	RETURN1:;
	if(CL_SYMBOLP(ARG(1)) && GET_SYMBOL(ARG(1)) == SYMBOL(Slisp, 23))	/* FIXNUM */
	{
		LOAD_FIXNUM(ARG(13), 1, ARG(13));
	}
	else
	{
		LOAD_BOOL(CL_SYMBOLP(ARG(1)) && GET_SYMBOL(ARG(1)) == SYMBOL(Slisp, 24), ARG(13));	/* FLOAT */
		if(CL_TRUEP(ARG(13)))
		{
			goto THEN2;
		}
		else
		{
			LOAD_BOOL(CL_SYMBOLP(ARG(1)) && GET_SYMBOL(ARG(1)) == SYMBOL(Slisp, 37), ARG(14));	/* SHORT-FLOAT */
			if(CL_TRUEP(ARG(14)))
			{
				goto THEN2;
			}
			else
			{
				LOAD_BOOL(CL_SYMBOLP(ARG(1)) && GET_SYMBOL(ARG(1)) == SYMBOL(Slisp, 42), ARG(15));	/* SINGLE-FLOAT */
				if(CL_TRUEP(ARG(15)))
				{
					goto THEN2;
				}
				else
				{
					LOAD_BOOL(CL_SYMBOLP(ARG(1)) && GET_SYMBOL(ARG(1)) == SYMBOL(Slisp, 22), ARG(16));	/* DOUBLE-FLOAT */
					if(CL_TRUEP(ARG(16)))
					{
						goto THEN2;
					}
					else
					{
					}	/* LONG-FLOAT */
				}
			}
		}
		if(CL_SYMBOLP(ARG(1)) && GET_SYMBOL(ARG(1)) == SYMBOL(Slisp, 29))
		{
			THEN2:;
			LOAD_FIXNUM(ARG(13), 2, ARG(13));
		}
		else
		{
			LOAD_BOOL(CL_SYMBOLP(ARG(1)) && GET_SYMBOL(ARG(1)) == SYMBOL(Slisp, 43), ARG(13));	/* STANDARD-CHAR */
			if(CL_TRUEP(ARG(13)))
			{
				goto THEN3;
			}
			else
			{
			}	/* CHARACTER */
			if(CL_SYMBOLP(ARG(1)) && GET_SYMBOL(ARG(1)) == SYMBOL(Slisp, 18))
			{
				THEN3:;
				LOAD_FIXNUM(ARG(13), 3, ARG(13));
			}
			else
			{
				if(CL_SYMBOLP(ARG(1)) && GET_SYMBOL(ARG(1)) == SYMBOL(Slisp, 16))	/* BIT */
				{
					LOAD_FIXNUM(ARG(13), 4, ARG(13));
				}
				else
				{
					LOAD_FIXNUM(ARG(13), 0, ARG(13));
				}
			}
		}
	}
	if(CL_TRUEP(ARG(8)))
	{
		COPY(ARG(2), ARG(14));
		COPY(ARG(13), ARG(15));
		type_code_p(ARG(14));
		if(CL_TRUEP(ARG(14)))
		{
			goto ELSE4;
		}
		else
		{
			goto THEN5;
		}
	}
	else
	{
		goto ELSE4;
	}
	{
		THEN5:;
		LOAD_SMSTR((CL_FORM *)&Kmake_array1[2], ARG(14));	/* The :initial-element, ~A, is of the wrong type; ~
                     it should be of type ~A */
		COPY(ARG(2), ARG(15));
		COPY(ARG(13), ARG(16));
		to_element_type(ARG(16));
		Ferror(ARG(14), 3);
	}
	ELSE4:;
	if(CL_TRUEP(ARG(9)))
	{
		if(CL_TRUEP(ARG(8)))
		{
			LOAD_SMSTR((CL_FORM *)&Kmake_array1[4], ARG(14));	/* :initial-element and :initial-contents are both supplied. */
			Ferror(ARG(14), 1);
		}
		{
			GEN_CLOSURE(array, ARG(14), 6, Z59_chk_shape_of_init_cont, 2);
			COPY(ARG(0), &array[3]);
			COPY(ARG(11), &array[4]);
			COPY(ARG(3), &array[5]);
			LOAD_CLOSURE(array, ARG(14));
		}
		COPY(ARG(14), ARG(15));
		COPY(INDIRECT(ARG(0)), ARG(16));
		COPY(INDIRECT(ARG(3)), ARG(17));
		Z59_chk_shape_of_init_cont(ARG(15));
	}
	if(CL_TRUEP(ARG(6)))
	{
		if(CL_TRUEP(ARG(8)))
		{
			COPY(SYMVAL(Slisp, 151), ARG(14));	/* DISPLACED_NOT_WITH */
			LOAD_SYMBOL(SYMBOL(Slisp, 155), ARG(15));	/* INITIAL-ELEMENT */
			Ferror(ARG(14), 2);
		}
		if(CL_TRUEP(ARG(9)))
		{
			COPY(SYMVAL(Slisp, 151), ARG(14));	/* DISPLACED_NOT_WITH */
			LOAD_SYMBOL(SYMBOL(Slisp, 103), ARG(15));	/* INITIAL-CONTENTS */
			Ferror(ARG(14), 2);
		}
		LOAD_BOOL(CL_SMVECP(ARG(6)), ARG(14));
		if(CL_TRUEP(ARG(14)))
		{
			goto THEN6;
		}
		else
		{
			COPY(ARG(6), ARG(15));
			LOAD_SYMBOL(SYMBOL(Slisp, 145), ARG(16));	/* COMPLEX-BASE-ARRAY */
			rt_struct_typep(ARG(15));
		}
		if(CL_TRUEP(ARG(15)))
		{
			THEN6:;
		}
		else
		{
			LOAD_SMSTR((CL_FORM *)&Kmake_array1[8], ARG(14));	/* The :displaced-to argument is not an array. */
			Ferror(ARG(14), 1);
		}
		COPY(ARG(13), ARG(14));
		COPY(ARG(6), ARG(15));
		if(CL_SMVECP(ARG(15)))
		{
			rt_plain_vector_element_code(ARG(15));
		}
		else
		{
			COPY(ARG(15), ARG(16));
			LOAD_SYMBOL(SYMBOL(Slisp, 145), ARG(17));	/* COMPLEX-BASE-ARRAY */
			rt_struct_typep(ARG(16));
			if(CL_TRUEP(ARG(16)))
			{
				COPY(ARG(15), ARG(16));
				M2_1:;
				COPY(ARG(16), ARG(17));
				COPY(ARG(17), ARG(18));
				COPY(ARG(18), ARG(19));
				LOAD_SYMBOL(SYMBOL(Slisp, 145), ARG(20));	/* COMPLEX-BASE-ARRAY */
				rt_struct_typep(ARG(19));
				if(CL_TRUEP(ARG(19)))
				{
					COPY(OFFSET(AR_BASE(GET_FORM(ARG(18))), 0 + 1), ARG(16));
				}
				else
				{
					COPY(SYMVAL(Slisp, 352), ARG(16));	/* NO_STRUCT */
					COPY(ARG(18), ARG(17));
					LOAD_SYMBOL(SYMBOL(Slisp, 145), ARG(18));	/* COMPLEX-BASE-ARRAY */
					Ferror(ARG(16), 3);
				}
				if(CL_SMVECP(ARG(16)))
				{
				}
				else
				{
					goto M2_1;
				}
				goto RETURN2;
				RETURN2:;
				rt_plain_vector_element_code(ARG(16));
				COPY(ARG(16), ARG(15));
			}
			else
			{
				COPY(SYMVAL(Slisp, 152), ARG(16));	/* NO_ARRAY */
				COPY(ARG(15), ARG(17));
				Ferror(ARG(16), 2);
			}
		}
		COPY(ARG(15), ARG(16));
		Fzerop(ARG(16));
		if(CL_TRUEP(ARG(16)))
		{
			goto THEN7;
		}
		else
		{
			COPY(ARG(14), ARG(17));
			COPY(ARG(15), ARG(18));
			Fnumeql(ARG(17), 2);
		}
		if(CL_TRUEP(ARG(17)))
		{
			THEN7:;
		}
		else
		{
			LOAD_SMSTR((CL_FORM *)&Kmake_array1[10], ARG(14));	/* Cannot displace the array, because it has a different type. */
			Ferror(ARG(14), 1);
		}
		COPY(ARG(12), ARG(14));
		COPY(ARG(6), ARG(15));
		Farray_total_size(ARG(15));
		Fgt(ARG(14), 2);
		if(CL_TRUEP(ARG(14)))
		{
			LOAD_SMSTR((CL_FORM *)&Kmake_array1[12], ARG(14));	/* Cannot displace the array, ~
                because the total size of the to-array is too small. */
			Ferror(ARG(14), 1);
		}
	}
	if(CL_TRUEP(ARG(10)))
	{
		if(CL_TRUEP(ARG(6)))
		{
		}
		else
		{
			LOAD_SMSTR((CL_FORM *)&Kmake_array1[14], ARG(14));	/* A :displaced-index-offset argument is given to make-array, ~
                        but there is no :displaced-to array */
			Ferror(ARG(14), 1);
		}
		COPY(ARG(6), ARG(14));
		Farray_total_size(ARG(14));
		COPY(ARG(12), ARG(15));
		COPY(ARG(14), ARG(16));
		Fgt(ARG(15), 2);
		if(CL_TRUEP(ARG(15)))
		{
			LOAD_SMSTR((CL_FORM *)&Kmake_array1[16], ARG(15));	/* The displaced-to array is smaller than the displaced array */
			Ferror(ARG(15), 1);
		}
		if(CL_FIXNUMP(ARG(7)))
		{
			LOAD_FIXNUM(ARG(15), 0, ARG(15));
			COPY(ARG(7), ARG(16));
			COPY(ARG(14), ARG(17));
			COPY(ARG(12), ARG(18));
			Fminus(ARG(17), 2);
			Fle(ARG(15), 3);
		}
		else
		{
			goto ELSE8;
		}
		if(CL_TRUEP(ARG(15)))
		{
		}
		else
		{
			ELSE8:;
			LOAD_SMSTR((CL_FORM *)&Kmake_array1[18], ARG(15));	/* The :displaced-index-offset argument, ~A, ~
                  is not in the linearized range [0, ~A) ~
                  of the :displaced-to array */
			COPY(ARG(7), ARG(16));
			COPY(ARG(6), ARG(17));
			Farray_total_size(ARG(17));
			COPY(ARG(12), ARG(18));
			Fminus(ARG(17), 2);
			Ferror(ARG(15), 3);
		}
	}
	COPY(INDIRECT(ARG(11)), ARG(14));
	LOAD_FIXNUM(ARG(15), 1, ARG(15));
	Fnumeql(ARG(14), 2);
	if(CL_TRUEP(ARG(14)))
	{
		LOAD_NIL(ARG(14));
		if(CL_TRUEP(ARG(5)))
		{
			if(CL_FIXNUMP(ARG(5)))
			{
				LOAD_FIXNUM(ARG(15), 0, ARG(15));
				COPY(ARG(5), ARG(16));
				COPY(ARG(12), ARG(17));
				Fle(ARG(15), 3);
			}
			else
			{
				goto ELSE9;
			}
			if(CL_TRUEP(ARG(15)))
			{
				goto ELSE10;
			}
			else
			{
				ELSE9:;
				goto THEN11;
			}
		}
		else
		{
			goto ELSE10;
		}
		{
			THEN11:;
			LOAD_SMSTR((CL_FORM *)&Kmake_array1[20], ARG(15));	/* The fill-pointer ~A must be an integer in the range [0, ~A] */
			COPY(ARG(5), ARG(16));
			COPY(ARG(12), ARG(17));
			Ferror(ARG(15), 3);
		}
		ELSE10:;
		if(CL_TRUEP(ARG(6)))
		{
			COPY(ARG(12), ARG(15));
			COPY(ARG(7), ARG(16));
			COPY(ARG(6), ARG(17));
			if(CL_TRUEP(ARG(5)))
			{
				COPY(ARG(5), ARG(18));
			}
			else
			{
				LOAD_FIXNUM(ARG(18), -1, ARG(18));
			}
			COPY(ARG(17), ARG(19));
			COPY(ARG(16), ARG(20));
			COPY(ARG(15), ARG(21));
			COPY(ARG(18), ARG(22));
			LOAD_SYMBOL(SYMBOL(Slisp, 150), ARG(14));	/* COMPLEX-VECTOR */
			COPY(ARG(19), ARG(15));
			COPY(ARG(20), ARG(16));
			COPY(ARG(21), ARG(17));
			COPY(ARG(22), ARG(18));
			rt_make_struct(ARG(14), 5);
		}
		else
		{
			if(CL_TRUEP(ARG(5)))
			{
				goto ELSE12;
			}
			else
			{
				COPY(ARG(4), ARG(15));
				if(CL_TRUEP(ARG(15)))
				{
					goto ELSE12;
				}
				else
				{
					goto THEN13;
				}
			}
			{
				THEN13:;
				COPY(ARG(12), ARG(14));
				COPY(ARG(13), ARG(15));
				make_plain_vector(ARG(14));
			}
			goto ENDIF14;
			{
				ELSE12:;
				COPY(ARG(12), ARG(15));
				COPY(ARG(12), ARG(16));
				COPY(ARG(13), ARG(17));
				make_plain_vector(ARG(16));
				if(CL_TRUEP(ARG(5)))
				{
					COPY(ARG(5), ARG(17));
				}
				else
				{
					LOAD_FIXNUM(ARG(17), -1, ARG(17));
				}
				COPY(ARG(16), ARG(18));
				COPY(ARG(15), ARG(19));
				COPY(ARG(17), ARG(20));
				LOAD_SYMBOL(SYMBOL(Slisp, 150), ARG(14));	/* COMPLEX-VECTOR */
				COPY(ARG(18), ARG(15));
				LOAD_FIXNUM(ARG(16), -1, ARG(16));
				COPY(ARG(19), ARG(17));
				COPY(ARG(20), ARG(18));
				rt_make_struct(ARG(14), 5);
			}
			ENDIF14:;
		}
		if(CL_TRUEP(ARG(8)))
		{
			COPY(ARG(14), ARG(15));
			COPY(ARG(2), ARG(16));
			LOAD_FIXNUM(ARG(17), 0, ARG(17));
			LOAD_NIL(ARG(18));
			fill1(ARG(15));
		}
		if(CL_TRUEP(ARG(9)))
		{
			COPY(ARG(14), ARG(15));
			COPY(INDIRECT(ARG(3)), ARG(16));
			LOAD_FIXNUM(ARG(17), 0, ARG(17));
			LOAD_NIL(ARG(18));
			LOAD_FIXNUM(ARG(19), 0, ARG(19));
			LOAD_NIL(ARG(20));
			replace1(ARG(15));
		}
		COPY(ARG(14), ARG(0));
	}
	else
	{
		LOAD_NIL(ARG(14));
		if(CL_TRUEP(ARG(5)))
		{
			LOAD_SMSTR((CL_FORM *)&Kmake_array1[22], ARG(15));	/* Fill-pointers are only for 1-dimensional arrays. */
			Ferror(ARG(15), 1);
		}
		if(CL_TRUEP(ARG(6)))
		{
			COPY(INDIRECT(ARG(0)), ARG(15));
			COPY(ARG(7), ARG(16));
			COPY(ARG(6), ARG(17));
			COPY(ARG(17), ARG(18));
			COPY(ARG(16), ARG(19));
			COPY(ARG(15), ARG(20));
			LOAD_SYMBOL(SYMBOL(Slisp, 147), ARG(14));	/* COMPLEX-ARRAY */
			COPY(ARG(18), ARG(15));
			COPY(ARG(19), ARG(16));
			COPY(ARG(20), ARG(17));
			rt_make_struct(ARG(14), 4);
		}
		else
		{
			COPY(INDIRECT(ARG(0)), ARG(15));
			COPY(ARG(12), ARG(16));
			COPY(ARG(13), ARG(17));
			make_plain_vector(ARG(16));
			COPY(ARG(16), ARG(17));
			COPY(ARG(15), ARG(18));
			LOAD_SYMBOL(SYMBOL(Slisp, 147), ARG(14));	/* COMPLEX-ARRAY */
			COPY(ARG(17), ARG(15));
			LOAD_FIXNUM(ARG(16), -1, ARG(16));
			COPY(ARG(18), ARG(17));
			rt_make_struct(ARG(14), 4);
		}
		if(CL_TRUEP(ARG(8)))
		{
			LOAD_FIXNUM(ARG(15), 0, ARG(15));
			M3_1:;
			COPY(ARG(15), ARG(16));
			COPY(ARG(14), ARG(17));
			Farray_total_size(ARG(17));
			Fge(ARG(16), 2);
			if(CL_TRUEP(ARG(16)))
			{
				goto RETURN3;
			}
			COPY(ARG(2), ARG(16));
			COPY(ARG(14), ARG(17));
			COPY(ARG(15), ARG(18));
			Fset_row_major_aref(ARG(16));
			F1plus(ARG(15));
			goto M3_1;
			RETURN3:;
		}
		if(CL_TRUEP(ARG(9)))
		{
			LOAD_FIXNUM(ARG(15), 0, ARG(15));
			COPY(INDIRECT(ARG(0)), ARG(16));
			COPY(INDIRECT(ARG(3)), ARG(17));
			display[0] = ARG(0);
			Z58_set_initial_contents(ARG(15), display);
		}
		COPY(ARG(14), ARG(0));
	}
}

static void Z58_set_initial_contents(CL_FORM *base, CL_FORM *display[])
{
	if(CL_TRUEP(ARG(1)))
	{
		if(CL_CONSP(ARG(1)))
		{
			COPY(GET_CAR(ARG(1)), ARG(3));
		}
		else
		{
			LOAD_SMSTR((CL_FORM *)&KClisp[264], ARG(3));	/* ~a is not a list */
			COPY(ARG(1), ARG(4));
			Ferror(ARG(3), 2);
		}
		if(CL_CONSP(ARG(1)))
		{
			COPY(GET_CDR(ARG(1)), ARG(4));
		}
		else
		{
			LOAD_SMSTR((CL_FORM *)&KClisp[262], ARG(4));	/* ~a is not a list */
			COPY(ARG(1), ARG(5));
			Ferror(ARG(4), 2);
		}
		LOAD_FIXNUM(ARG(5), 0, ARG(5));
		LOAD_FIXNUM(ARG(6), 0, ARG(6));
		M1_1:;
		COPY(ARG(5), ARG(7));
		COPY(ARG(3), ARG(8));
		Fge(ARG(7), 2);
		if(CL_TRUEP(ARG(7)))
		{
			COPY(ARG(6), ARG(0));
			goto RETURN1;
		}
		COPY(ARG(6), ARG(7));
		COPY(ARG(0), ARG(8));
		COPY(ARG(6), ARG(9));
		Fplus(ARG(8), 2);
		COPY(ARG(4), ARG(9));
		COPY(ARG(2), ARG(10));
		COPY(ARG(5), ARG(11));
		Felt(ARG(10));
		Z58_set_initial_contents(ARG(8), display);
		Fplus(ARG(7), 2);
		COPY(ARG(7), ARG(6));
		F1plus(ARG(5));
		goto M1_1;
		RETURN1:;
	}
	else
	{
		COPY(ARG(2), ARG(3));
		COPY(&display[0][14], ARG(4));
		COPY(ARG(0), ARG(5));
		Fset_row_major_aref(ARG(3));
		LOAD_FIXNUM(ARG(3), 1, ARG(0));
	}
}

static void Z60_lambda(CL_FORM *base);

static void Z59_chk_shape_of_init_cont(CL_FORM *base)
{
	GEN_HEAPVAR(ARG(1), ARG(3));
	if(CL_TRUEP(INDIRECT(ARG(1))))
	{
		LOAD_NIL(ARG(3));
	}
	else
	{
		LOAD_SYMBOL(SYMBOL(Slisp, 48), ARG(3));	/* T */
	}
	if(CL_TRUEP(ARG(3)))
	{
		COPY(ARG(3), ARG(0));
	}
	else
	{
		if(CL_CONSP(INDIRECT(ARG(1))))
		{
			COPY(GET_CAR(INDIRECT(ARG(1))), ARG(4));
		}
		else
		{
			if(CL_TRUEP(INDIRECT(ARG(1))))
			{
				LOAD_SMSTR((CL_FORM *)&KClisp[264], ARG(4));	/* ~a is not a list */
				COPY(INDIRECT(ARG(1)), ARG(5));
				Ferror(ARG(4), 2);
			}
			else
			{
				COPY(INDIRECT(ARG(1)), ARG(4));
			}
		}
		COPY(ARG(2), ARG(5));
		Flength(ARG(5));
		Fnumneql(ARG(4), 2);
		if(CL_TRUEP(ARG(4)))
		{
			LOAD_SMSTR((CL_FORM *)&Kmake_array1[6], ARG(4));	/* The :initial-contents argument, ~A,
                             isn't the correct shape or size
                             for an array of rank ~A with dimensions ~A */
			COPY(INDIRECT(GET_FORM(ARG(0)) + 5), ARG(5));
			COPY(INDIRECT(GET_FORM(ARG(0)) + 4), ARG(6));
			COPY(INDIRECT(GET_FORM(ARG(0)) + 3), ARG(7));
			Ferror(ARG(4), 4);
		}
		else
		{
			GEN_CLOSURE(array, ARG(4), 5, Z60_lambda, 1);
			COPY(ARG(1), &array[3]);
			COPY(ARG(0), &array[4]);
			LOAD_CLOSURE(array, ARG(4));
			COPY(ARG(4), ARG(4));
			COPY(ARG(2), ARG(5));
			Fmapc(ARG(4), 2);
			COPY(ARG(4), ARG(0));
		}
	}
}

static void Z60_lambda(CL_FORM *base)
{
	COPY(GET_FORM(ARG(0)) + 4, ARG(2));
	if(CL_CONSP(INDIRECT(GET_FORM(ARG(0)) + 3)))
	{
		COPY(GET_CDR(INDIRECT(GET_FORM(ARG(0)) + 3)), ARG(3));
	}
	else
	{
		if(CL_TRUEP(INDIRECT(GET_FORM(ARG(0)) + 3)))
		{
			LOAD_SMSTR((CL_FORM *)&KClisp[262], ARG(3));	/* ~a is not a list */
			COPY(INDIRECT(GET_FORM(ARG(0)) + 3), ARG(4));
			Ferror(ARG(3), 2);
		}
		else
		{
			COPY(INDIRECT(GET_FORM(ARG(0)) + 3), ARG(3));
		}
	}
	COPY(ARG(1), ARG(4));
	Z59_chk_shape_of_init_cont(ARG(2));
	COPY(ARG(2), ARG(0));
}
