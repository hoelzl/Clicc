/* This file was generated by CLiCC 0.6.4 [obrep 1] */
#define __OBREP 1
#include <c_decl.h>
#include "sys.h"
#include "lisp.h"

GEN_GLOBAL_FUNARG(Cbq_simplify, bq_simplify, 1);

void bq_simplify(CL_FORM *base)
{
	if(CL_ATOMP(ARG(0)))
	{
	}
	else
	{
		COPY(GET_CAR(ARG(0)), ARG(1));
		if(EQ(ARG(1), SYMVAL(Slisp, 444)))	/* *BQ-QUOTE* */
		{
			COPY(ARG(0), ARG(1));
		}
		else
		{
			LOAD_GLOBFUN(&Cbq_simplify, ARG(1));
			COPY(ARG(0), ARG(2));
			maptree(ARG(1));
			mv_count = 1;
		}
		if(CL_CONSP(ARG(1)))
		{
			COPY(GET_CAR(ARG(1)), ARG(2));
		}
		else
		{
			if(CL_TRUEP(ARG(1)))
			{
				LOAD_SMSTR((CL_FORM *)&KClisp[264], ARG(2));	/* ~a is not a list */
				COPY(ARG(1), ARG(3));
				Ferror(ARG(2), 2);
			}
			else
			{
				COPY(ARG(1), ARG(2));
			}
		}
		if(EQ(ARG(2), SYMVAL(Slisp, 440)))	/* *BQ-APPEND* */
		{
			if(CL_CONSP(ARG(1)))
			{
				COPY(GET_CDR(ARG(1)), ARG(2));
			}
			else
			{
				if(CL_TRUEP(ARG(1)))
				{
					LOAD_SMSTR((CL_FORM *)&KClisp[262], ARG(2));	/* ~a is not a list */
					COPY(ARG(1), ARG(3));
					Ferror(ARG(2), 2);
				}
				else
				{
					COPY(ARG(1), ARG(2));
				}
			}
			Freverse(ARG(2));
			LOAD_NIL(ARG(3));
			M1_1:;
			if(CL_TRUEP(ARG(2)))
			{
			}
			else
			{
				COPY(ARG(3), ARG(0));
				goto RETURN1;
			}
			COPY(ARG(2), ARG(4));
			if(CL_CONSP(ARG(4)))
			{
				COPY(GET_CDR(ARG(4)), ARG(4));
			}
			else
			{
				LOAD_SMSTR((CL_FORM *)&KClisp[262], ARG(5));	/* ~a is not a list */
				COPY(ARG(4), ARG(6));
				Ferror(ARG(5), 2);
			}
			COPY(ARG(2), ARG(5));
			if(CL_CONSP(ARG(5)))
			{
				COPY(GET_CAR(ARG(5)), ARG(5));
			}
			else
			{
				LOAD_SMSTR((CL_FORM *)&KClisp[264], ARG(6));	/* ~a is not a list */
				COPY(ARG(5), ARG(7));
				Ferror(ARG(6), 2);
			}
			if(CL_ATOMP(ARG(5)))
			{
				COPY(SYMVAL(Slisp, 440), ARG(5));	/* *BQ-APPEND* */
				COPY(ARG(2), ARG(6));
				if(CL_CONSP(ARG(6)))
				{
					COPY(GET_CAR(ARG(6)), ARG(6));
				}
				else
				{
					LOAD_SMSTR((CL_FORM *)&KClisp[264], ARG(7));	/* ~a is not a list */
					COPY(ARG(6), ARG(8));
					Ferror(ARG(7), 2);
				}
				COPY(ARG(3), ARG(7));
				bq_attach_append(ARG(5));
				COPY(ARG(5), ARG(3));
			}
			else
			{
				COPY(ARG(2), ARG(5));
				COPY(ARG(5), ARG(6));
				if(CL_CONSP(ARG(6)))
				{
					COPY(GET_CAR(ARG(6)), ARG(6));
				}
				else
				{
					LOAD_SMSTR((CL_FORM *)&KClisp[264], ARG(7));	/* ~a is not a list */
					COPY(ARG(6), ARG(8));
					Ferror(ARG(7), 2);
				}
				if(CL_CONSP(ARG(6)))
				{
					COPY(GET_CAR(ARG(6)), ARG(5));
				}
				else
				{
					if(CL_TRUEP(ARG(6)))
					{
						LOAD_SMSTR((CL_FORM *)&KClisp[264], ARG(5));	/* ~a is not a list */
						Ferror(ARG(5), 2);
					}
					else
					{
						COPY(ARG(6), ARG(5));
					}
				}
				if(EQ(ARG(5), SYMVAL(Slisp, 439)))	/* *BQ-LIST* */
				{
					COPY(ARG(2), ARG(5));
					COPY(ARG(5), ARG(6));
					if(CL_CONSP(ARG(6)))
					{
						COPY(GET_CAR(ARG(6)), ARG(6));
					}
					else
					{
						LOAD_SMSTR((CL_FORM *)&KClisp[264], ARG(7));	/* ~a is not a list */
						COPY(ARG(6), ARG(8));
						Ferror(ARG(7), 2);
					}
					if(CL_CONSP(ARG(6)))
					{
						COPY(GET_CDR(ARG(6)), ARG(5));
					}
					else
					{
						if(CL_TRUEP(ARG(6)))
						{
							LOAD_SMSTR((CL_FORM *)&KClisp[262], ARG(5));	/* ~a is not a list */
							Ferror(ARG(5), 2);
						}
						else
						{
							COPY(ARG(6), ARG(5));
						}
					}
					LOAD_GLOBFUN(&Cbq_splicing_frob, ARG(6));
					COPY(ARG(5), ARG(7));
					Fsome(ARG(6), 2);
					if(CL_TRUEP(ARG(6)))
					{
						goto ELSE1;
					}
					else
					{
						goto THEN2;
					}
				}
				else
				{
					goto ELSE1;
				}
				{
					THEN2:;
					COPY(ARG(2), ARG(5));
					COPY(ARG(5), ARG(6));
					if(CL_CONSP(ARG(6)))
					{
						COPY(GET_CAR(ARG(6)), ARG(6));
					}
					else
					{
						LOAD_SMSTR((CL_FORM *)&KClisp[264], ARG(7));	/* ~a is not a list */
						COPY(ARG(6), ARG(8));
						Ferror(ARG(7), 2);
					}
					if(CL_CONSP(ARG(6)))
					{
						COPY(GET_CDR(ARG(6)), ARG(5));
					}
					else
					{
						if(CL_TRUEP(ARG(6)))
						{
							LOAD_SMSTR((CL_FORM *)&KClisp[262], ARG(5));	/* ~a is not a list */
							Ferror(ARG(5), 2);
						}
						else
						{
							COPY(ARG(6), ARG(5));
						}
					}
					COPY(ARG(3), ARG(6));
					bq_attach_conses(ARG(5));
					COPY(ARG(5), ARG(3));
				}
				goto ENDIF3;
				{
					ELSE1:;
					COPY(ARG(2), ARG(5));
					COPY(ARG(5), ARG(6));
					if(CL_CONSP(ARG(6)))
					{
						COPY(GET_CAR(ARG(6)), ARG(6));
					}
					else
					{
						LOAD_SMSTR((CL_FORM *)&KClisp[264], ARG(7));	/* ~a is not a list */
						COPY(ARG(6), ARG(8));
						Ferror(ARG(7), 2);
					}
					if(CL_CONSP(ARG(6)))
					{
						COPY(GET_CAR(ARG(6)), ARG(5));
					}
					else
					{
						if(CL_TRUEP(ARG(6)))
						{
							LOAD_SMSTR((CL_FORM *)&KClisp[264], ARG(5));	/* ~a is not a list */
							Ferror(ARG(5), 2);
						}
						else
						{
							COPY(ARG(6), ARG(5));
						}
					}
					if(EQ(ARG(5), SYMVAL(Slisp, 441)))	/* *BQ-LIST** */
					{
						COPY(ARG(2), ARG(5));
						COPY(ARG(5), ARG(6));
						if(CL_CONSP(ARG(6)))
						{
							COPY(GET_CAR(ARG(6)), ARG(6));
						}
						else
						{
							LOAD_SMSTR((CL_FORM *)&KClisp[264], ARG(7));	/* ~a is not a list */
							COPY(ARG(6), ARG(8));
							Ferror(ARG(7), 2);
						}
						if(CL_CONSP(ARG(6)))
						{
							COPY(GET_CDR(ARG(6)), ARG(5));
						}
						else
						{
							if(CL_TRUEP(ARG(6)))
							{
								LOAD_SMSTR((CL_FORM *)&KClisp[262], ARG(5));	/* ~a is not a list */
								Ferror(ARG(5), 2);
							}
							else
							{
								COPY(ARG(6), ARG(5));
							}
						}
						LOAD_GLOBFUN(&Cbq_splicing_frob, ARG(6));
						COPY(ARG(5), ARG(7));
						Fsome(ARG(6), 2);
						if(CL_TRUEP(ARG(6)))
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
						COPY(ARG(2), ARG(5));
						COPY(ARG(5), ARG(6));
						if(CL_CONSP(ARG(6)))
						{
							COPY(GET_CAR(ARG(6)), ARG(6));
						}
						else
						{
							LOAD_SMSTR((CL_FORM *)&KClisp[264], ARG(7));	/* ~a is not a list */
							COPY(ARG(6), ARG(8));
							Ferror(ARG(7), 2);
						}
						if(CL_CONSP(ARG(6)))
						{
							COPY(GET_CDR(ARG(6)), ARG(5));
						}
						else
						{
							if(CL_TRUEP(ARG(6)))
							{
								LOAD_SMSTR((CL_FORM *)&KClisp[262], ARG(5));	/* ~a is not a list */
								Ferror(ARG(5), 2);
							}
							else
							{
								COPY(ARG(6), ARG(5));
							}
						}
						Freverse(ARG(5));
						if(CL_CONSP(ARG(5)))
						{
							COPY(GET_CDR(ARG(5)), ARG(5));
						}
						else
						{
							if(CL_TRUEP(ARG(5)))
							{
								LOAD_SMSTR((CL_FORM *)&KClisp[262], ARG(6));	/* ~a is not a list */
								COPY(ARG(5), ARG(7));
								Ferror(ARG(6), 2);
							}
							else
							{
							}
						}
						Freverse(ARG(5));
						COPY(SYMVAL(Slisp, 440), ARG(6));	/* *BQ-APPEND* */
						COPY(ARG(2), ARG(7));
						if(CL_CONSP(ARG(7)))
						{
							COPY(GET_CAR(ARG(7)), ARG(7));
						}
						else
						{
							LOAD_SMSTR((CL_FORM *)&KClisp[264], ARG(8));	/* ~a is not a list */
							COPY(ARG(7), ARG(9));
							Ferror(ARG(8), 2);
						}
						LOAD_FIXNUM(ARG(8), 1, ARG(8));
						last1(ARG(7));
						if(CL_CONSP(ARG(7)))
						{
							COPY(GET_CAR(ARG(7)), ARG(7));
						}
						else
						{
							if(CL_TRUEP(ARG(7)))
							{
								LOAD_SMSTR((CL_FORM *)&KClisp[264], ARG(8));	/* ~a is not a list */
								COPY(ARG(7), ARG(9));
								Ferror(ARG(8), 2);
							}
							else
							{
							}
						}
						COPY(ARG(3), ARG(8));
						bq_attach_append(ARG(6));
						bq_attach_conses(ARG(5));
						COPY(ARG(5), ARG(3));
					}
					goto ENDIF6;
					{
						ELSE4:;
						COPY(ARG(2), ARG(5));
						COPY(ARG(5), ARG(6));
						if(CL_CONSP(ARG(6)))
						{
							COPY(GET_CAR(ARG(6)), ARG(6));
						}
						else
						{
							LOAD_SMSTR((CL_FORM *)&KClisp[264], ARG(7));	/* ~a is not a list */
							COPY(ARG(6), ARG(8));
							Ferror(ARG(7), 2);
						}
						if(CL_CONSP(ARG(6)))
						{
							COPY(GET_CAR(ARG(6)), ARG(5));
						}
						else
						{
							if(CL_TRUEP(ARG(6)))
							{
								LOAD_SMSTR((CL_FORM *)&KClisp[264], ARG(5));	/* ~a is not a list */
								Ferror(ARG(5), 2);
							}
							else
							{
								COPY(ARG(6), ARG(5));
							}
						}
						if(EQ(ARG(5), SYMVAL(Slisp, 444)))	/* *BQ-QUOTE* */
						{
							COPY(ARG(2), ARG(5));
							COPY(ARG(5), ARG(6));
							if(CL_CONSP(ARG(6)))
							{
								COPY(GET_CAR(ARG(6)), ARG(6));
							}
							else
							{
								LOAD_SMSTR((CL_FORM *)&KClisp[264], ARG(7));	/* ~a is not a list */
								COPY(ARG(6), ARG(8));
								Ferror(ARG(7), 2);
							}
							if(CL_CONSP(ARG(6)))
							{
								COPY(GET_CDR(ARG(6)), ARG(6));
							}
							else
							{
								if(CL_TRUEP(ARG(6)))
								{
									LOAD_SMSTR((CL_FORM *)&KClisp[262], ARG(7));	/* ~a is not a list */
									COPY(ARG(6), ARG(8));
									Ferror(ARG(7), 2);
								}
								else
								{
								}
							}
							if(CL_CONSP(ARG(6)))
							{
								COPY(GET_CAR(ARG(6)), ARG(5));
							}
							else
							{
								if(CL_TRUEP(ARG(6)))
								{
									LOAD_SMSTR((CL_FORM *)&KClisp[264], ARG(5));	/* ~a is not a list */
									Ferror(ARG(5), 2);
								}
								else
								{
									COPY(ARG(6), ARG(5));
								}
							}
							if(CL_CONSP(ARG(5)))
							{
								COPY(ARG(2), ARG(5));
								COPY(ARG(5), ARG(6));
								if(CL_CONSP(ARG(6)))
								{
									COPY(GET_CAR(ARG(6)), ARG(6));
								}
								else
								{
									LOAD_SMSTR((CL_FORM *)&KClisp[264], ARG(7));	/* ~a is not a list */
									COPY(ARG(6), ARG(8));
									Ferror(ARG(7), 2);
								}
								if(CL_CONSP(ARG(6)))
								{
									COPY(GET_CDR(ARG(6)), ARG(6));
								}
								else
								{
									if(CL_TRUEP(ARG(6)))
									{
										LOAD_SMSTR((CL_FORM *)&KClisp[262], ARG(7));	/* ~a is not a list */
										COPY(ARG(6), ARG(8));
										Ferror(ARG(7), 2);
									}
									else
									{
									}
								}
								if(CL_CONSP(ARG(6)))
								{
									COPY(GET_CAR(ARG(6)), ARG(5));
								}
								else
								{
									if(CL_TRUEP(ARG(6)))
									{
										LOAD_SMSTR((CL_FORM *)&KClisp[264], ARG(5));	/* ~a is not a list */
										Ferror(ARG(5), 2);
									}
									else
									{
										COPY(ARG(6), ARG(5));
									}
								}
								if(CL_CONSP(ARG(5)))
								{
									COPY(ARG(5), ARG(6));
									COPY(GET_CAR(ARG(6)), ARG(6));
									LOAD_BOOL(EQ(ARG(6), SYMVAL(Slisp, 436)), ARG(6));	/* *COMMA* */
									if(CL_TRUEP(ARG(6)))
									{
										goto THEN7;
									}
									else
									{
										COPY(ARG(5), ARG(7));
										COPY(GET_CAR(ARG(7)), ARG(7));
										LOAD_BOOL(EQ(ARG(7), SYMVAL(Slisp, 437)), ARG(7));	/* *COMMA-ATSIGN* */
										if(CL_TRUEP(ARG(7)))
										{
											goto THEN7;
										}
										else
										{
											COPY(ARG(5), ARG(8));
											COPY(GET_CAR(ARG(8)), ARG(8));
										}	/* *COMMA-DOT* */
									}
								}
								else
								{
									goto ELSE8;
								}
								if(EQ(ARG(8), SYMVAL(Slisp, 438)))
								{
									THEN7:;
									goto ELSE9;
								}
								else
								{
									ELSE8:;
									COPY(ARG(2), ARG(5));
									COPY(ARG(5), ARG(6));
									if(CL_CONSP(ARG(6)))
									{
										COPY(GET_CAR(ARG(6)), ARG(6));
									}
									else
									{
										LOAD_SMSTR((CL_FORM *)&KClisp[264], ARG(7));	/* ~a is not a list */
										COPY(ARG(6), ARG(8));
										Ferror(ARG(7), 2);
									}
									if(CL_CONSP(ARG(6)))
									{
										COPY(GET_CDR(ARG(6)), ARG(6));
									}
									else
									{
										if(CL_TRUEP(ARG(6)))
										{
											LOAD_SMSTR((CL_FORM *)&KClisp[262], ARG(7));	/* ~a is not a list */
											COPY(ARG(6), ARG(8));
											Ferror(ARG(7), 2);
										}
										else
										{
										}
									}
									if(CL_CONSP(ARG(6)))
									{
										COPY(GET_CDR(ARG(6)), ARG(5));
									}
									else
									{
										if(CL_TRUEP(ARG(6)))
										{
											LOAD_SMSTR((CL_FORM *)&KClisp[262], ARG(5));	/* ~a is not a list */
											Ferror(ARG(5), 2);
										}
										else
										{
											COPY(ARG(6), ARG(5));
										}
									}
									if(CL_TRUEP(ARG(5)))
									{
										goto ELSE9;
									}
									else
									{
										goto THEN10;
									}
								}
							}
							else
							{
								goto ELSE9;
							}
						}
						else
						{
							goto ELSE9;
						}
						{
							THEN10:;
							COPY(SYMVAL(Slisp, 444), ARG(5));	/* *BQ-QUOTE* */
							COPY(ARG(2), ARG(6));
							COPY(ARG(6), ARG(7));
							if(CL_CONSP(ARG(7)))
							{
								COPY(GET_CAR(ARG(7)), ARG(7));
							}
							else
							{
								LOAD_SMSTR((CL_FORM *)&KClisp[264], ARG(8));	/* ~a is not a list */
								COPY(ARG(7), ARG(9));
								Ferror(ARG(8), 2);
							}
							if(CL_CONSP(ARG(7)))
							{
								COPY(GET_CDR(ARG(7)), ARG(7));
							}
							else
							{
								if(CL_TRUEP(ARG(7)))
								{
									LOAD_SMSTR((CL_FORM *)&KClisp[262], ARG(8));	/* ~a is not a list */
									COPY(ARG(7), ARG(9));
									Ferror(ARG(8), 2);
								}
								else
								{
								}
							}
							if(CL_CONSP(ARG(7)))
							{
								COPY(GET_CAR(ARG(7)), ARG(7));
							}
							else
							{
								if(CL_TRUEP(ARG(7)))
								{
									LOAD_SMSTR((CL_FORM *)&KClisp[264], ARG(8));	/* ~a is not a list */
									COPY(ARG(7), ARG(9));
									Ferror(ARG(8), 2);
								}
								else
								{
								}
							}
							if(CL_CONSP(ARG(7)))
							{
								COPY(GET_CAR(ARG(7)), ARG(6));
							}
							else
							{
								if(CL_TRUEP(ARG(7)))
								{
									LOAD_SMSTR((CL_FORM *)&KClisp[264], ARG(6));	/* ~a is not a list */
									Ferror(ARG(6), 2);
								}
								else
								{
									COPY(ARG(7), ARG(6));
								}
							}
							Flist(ARG(5), 2);
							Flist(ARG(5), 1);
							COPY(ARG(3), ARG(6));
							bq_attach_conses(ARG(5));
							COPY(ARG(5), ARG(3));
						}
						goto ENDIF11;
						{
							ELSE9:;
							COPY(ARG(2), ARG(5));
							COPY(ARG(5), ARG(6));
							if(CL_CONSP(ARG(6)))
							{
								COPY(GET_CAR(ARG(6)), ARG(6));
							}
							else
							{
								LOAD_SMSTR((CL_FORM *)&KClisp[264], ARG(7));	/* ~a is not a list */
								COPY(ARG(6), ARG(8));
								Ferror(ARG(7), 2);
							}
							if(CL_CONSP(ARG(6)))
							{
								COPY(GET_CAR(ARG(6)), ARG(5));
							}
							else
							{
								if(CL_TRUEP(ARG(6)))
								{
									LOAD_SMSTR((CL_FORM *)&KClisp[264], ARG(5));	/* ~a is not a list */
									Ferror(ARG(5), 2);
								}
								else
								{
									COPY(ARG(6), ARG(5));
								}
							}
							if(EQ(ARG(5), SYMVAL(Slisp, 443)))	/* *BQ-CLOBBERABLE* */
							{
								COPY(SYMVAL(Slisp, 442), ARG(5));	/* *BQ-NCONC* */
								COPY(ARG(2), ARG(6));
								COPY(ARG(6), ARG(7));
								if(CL_CONSP(ARG(7)))
								{
									COPY(GET_CAR(ARG(7)), ARG(7));
								}
								else
								{
									LOAD_SMSTR((CL_FORM *)&KClisp[264], ARG(8));	/* ~a is not a list */
									COPY(ARG(7), ARG(9));
									Ferror(ARG(8), 2);
								}
								if(CL_CONSP(ARG(7)))
								{
									COPY(GET_CDR(ARG(7)), ARG(7));
								}
								else
								{
									if(CL_TRUEP(ARG(7)))
									{
										LOAD_SMSTR((CL_FORM *)&KClisp[262], ARG(8));	/* ~a is not a list */
										COPY(ARG(7), ARG(9));
										Ferror(ARG(8), 2);
									}
									else
									{
									}
								}
								if(CL_CONSP(ARG(7)))
								{
									COPY(GET_CAR(ARG(7)), ARG(6));
								}
								else
								{
									if(CL_TRUEP(ARG(7)))
									{
										LOAD_SMSTR((CL_FORM *)&KClisp[264], ARG(6));	/* ~a is not a list */
										Ferror(ARG(6), 2);
									}
									else
									{
										COPY(ARG(7), ARG(6));
									}
								}
								COPY(ARG(3), ARG(7));
								bq_attach_append(ARG(5));
								COPY(ARG(5), ARG(3));
							}
							else
							{
								COPY(SYMVAL(Slisp, 440), ARG(5));	/* *BQ-APPEND* */
								COPY(ARG(2), ARG(6));
								if(CL_CONSP(ARG(6)))
								{
									COPY(GET_CAR(ARG(6)), ARG(6));
								}
								else
								{
									LOAD_SMSTR((CL_FORM *)&KClisp[264], ARG(7));	/* ~a is not a list */
									COPY(ARG(6), ARG(8));
									Ferror(ARG(7), 2);
								}
								COPY(ARG(3), ARG(7));
								bq_attach_append(ARG(5));
								COPY(ARG(5), ARG(3));
							}
						}
						ENDIF11:;
					}
					ENDIF6:;
				}
				ENDIF3:;
			}
			COPY(ARG(4), ARG(2));
			goto M1_1;
			RETURN1:;
		}
		else
		{
			COPY(ARG(1), ARG(0));
		}
	}
}
