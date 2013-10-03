/*------------------------------------------------------------------------------
 * Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel
 *------------------------------------------------------------------------------
 * Projekt  : APPLY - A Practicable And Portable Lisp Implementation
 *            ------------------------------------------------------
 * Funktion : System-Funktionen (Arrays)                                
 *
 * $Revision: 1.18 $
 * $Log: array.c,v $
 * Revision 1.18  1994/04/28  09:42:57  sma
 * LOAD_FIXNUM, LOAD_CHAR und LOAD_FLOAT um 3. Argument ergänzt.
 *
 * Revision 1.17  1994/01/05  12:45:38  sma
 * Alle Laufzeitfunktionen mit dem Präfix rt_ versehen. make-plain-vector
 * in fünf Funktionen make-vector-t, make-vector-fixnum,
 * make-vector-float und make-vector-bit zerteilt.
 *
 * Revision 1.16  1993/12/14  12:30:37  sma
 * Namensänderungen durch Einführung von plain-vector-Typ.
 * simple-vector-element-code und make-simple-vector heißen jetzt
 * plain-vector-element-code und make-plain-vector. (set-)svref wurde in
 * (set-)pvref-internal umbenannt.
 *
 * Revision 1.15  1993/12/10  11:31:19  sma
 * Neue array-Repräsentation. Weniger C, mehr Lisp. Neue Version, die
 * meisten C-Funktionen wurden ersetzt/gelöscht. Neue Funktion shrink_smstr.
 *
 * Revision 1.13  1993/10/29  15:16:57  sma
 * Methode, den Array-Typ (inkorrekterweise) nochmals im TAG-Feld der
 * Arraygröße zu speichern entfernt. Aus diesem Grund
 * (set_)row_major_aref_internal geändert.
 *
 * Revision 1.12  1993/09/13  11:52:05  sma
 * Fehler in Längenangaben von Arrays, Vectoren und Instanzen beseitigt
 * durch Einführen des SET_AR_SIZE-Makros.
 *
 * Revision 1.11  1993/08/26  15:20:57  hk
 * Typ von set_array_header von (CL_FORM *) nach void.
 * Unbenutzte Variable entfernt.
 *
 * Revision 1.10  1993/08/20  10:13:41  hk
 * array_element_type_internal prüft nicht mehr auf array Typ.
 *
 * Revision 1.9  1993/07/08  13:12:05  sma
 * OFFSET-Marko eingeführt.
 *
 * Revision 1.8  1993/06/16  14:43:22  hk
 * Copyright Notiz eingefuegt.
 *
 * Revision 1.7  1993/05/08  18:19:20  hk
 * Argumentreihenfolge von set-row-major-aref-internal, set-svref-internal
 * und set-fill-pointer-internal geaendert.
 *
 * Revision 1.6  1993/04/22  10:29:34  hk
 * fun_decl.h -> sys.h.
 *
 * Revision 1.5  1993/02/17  15:25:58  hk
 * CLICC -> APPLY, Revison Keyword.
 *
 * Revision 1.4  1993/01/05  10:25:12  hk
 * Neue Funktion shrink-vector
 *
 * Revision 1.3  1992/11/16  11:41:10  hk
 * Typ von set_row_major_aref_internal von (void *) nach void, scheint ein
 * Schreibfehler gewesen zu sein.
 *
 * Revision 1.2  1992/09/28  17:20:28  hk
 * Lerror -> Labort, neues Lerror mit Lisp-Parameter
 *
 * Revision 1.1  1992/03/24  17:03:37  hk
 * Initial revision
 *----------------------------------------------------------------------------*/

#include <c_decl.h>
#include "sys.h"


/* Zusicherung: alle Parameter sind vom korrekten Typ! (ausser bei *_p) */
/*----------------------------------------------------------------------*/

/* lokales define */
#define HEADER_SIZE  1


/*------------------------------------------------------------------------------
 * RT::PLAIN-VECTOR-ELEMENT-CODE vector
 *----------------------------------------------------------------------------*/
void rt_plain_vector_element_code(base)
CL_FORM *base;
{
   /* 2 Zeilen notwendig wegen Seiteneffekte in den LOAD-Makros */
   int code = GET_VECTOR_CODE(ARG(0)); 
   LOAD_FIXNUM(ARG(0), code, ARG(0));
}

/*------------------------------------------------------------------------------
 * RT::MAKE-VECTOR-T size initvalue
 *----------------------------------------------------------------------------*/
void rt_make_vector_t(base)
CL_FORM *base;
{
   long i, size = GET_FIXNUM(ARG(0));
   CL_FORM *vector;
   
   vector = form_alloc(ARG(2), size + HEADER_SIZE);
   for (i = 1; i <= size; i++)
      COPY(ARG(1), OFFSET(vector, i));
   
   INIT_VEC_T(vector, size);
   LOAD_VEC_T(vector, ARG(0));
}

/*------------------------------------------------------------------------------
 * RT::MAKE_VECTOR_FIXNUM size
 *----------------------------------------------------------------------------*/
void rt_make_vector_fixnum(base)
CL_FORM *base;
{
   long i, size = GET_FIXNUM(ARG(0));
   long *data = fixnum_alloc(ARG(0), size);
   CL_FORM *vector;

   vector = form_alloc(ARG(0), 1 + HEADER_SIZE);
   for (i = 0; i < size; i++)
      data[i] = 0;
   LOAD_FIXNUM_PTR(data, AR_BASE(vector));
   INIT_VEC_FIXNUM(vector, size);
   LOAD_VEC_FIXNUM(vector, ARG(0));
}

/*------------------------------------------------------------------------------
 * RT::MAKE_VECTOR_FLOAT size
 *----------------------------------------------------------------------------*/
void rt_make_vector_float(base)
CL_FORM *base;
{
   long i, size = GET_FIXNUM(ARG(0));
   double *data = float_alloc(ARG(0), size);
   CL_FORM *vector;

   vector = form_alloc(ARG(0), 1 + HEADER_SIZE);
   for (i = 0; i < size; i++)
      data[i] = 0.0;
   LOAD_FLOAT_PTR(data, AR_BASE(vector));
   INIT_VEC_FLOAT(vector, size);
   LOAD_VEC_FLOAT(vector, ARG(0));
}

/*------------------------------------------------------------------------------
 * RT::MAKE_VECTOR_CHAR size initvalue
 *----------------------------------------------------------------------------*/
void rt_make_vector_char(base)
CL_FORM *base;
{
   long i, size = GET_FIXNUM(ARG(0));
   char initvalue = GET_CHAR(ARG(1));
   char *data = char_alloc(ARG(0), size);
   CL_FORM *vector;

   vector = form_alloc(ARG(0), 1 + HEADER_SIZE);
   for (i = 0; i < size; i++)
      data[i] = initvalue;
   LOAD_CHAR_PTR(data, AR_BASE(vector));
   INIT_VEC_CHAR(vector, size);
   LOAD_VEC_CHAR(vector, ARG(0));
}

/*------------------------------------------------------------------------------
 * RT::MAKE_VECTOR_BIT size
 *----------------------------------------------------------------------------*/
void rt_make_vector_bit(base)
CL_FORM *base;
{
   long size = GET_FIXNUM(ARG(0));
   CL_FORM *vector;

   vector = form_alloc(ARG(0), 1 + HEADER_SIZE);
   /* bits müssen nicht initialisiert werden,0/1 sind alle möglichen Werte */
   LOAD_BITS_PTR(bits_alloc(ARG(0), size), AR_BASE(vector));
   INIT_VEC_BIT(vector, size);
   LOAD_VEC_BIT(vector, ARG(0));
}


/*------------------------------------------------------------------------------
 * RT::PVREF vector index
 *----------------------------------------------------------------------------*/
void rt_pvref(base)
CL_FORM *base;
{
   CL_FORM *vector = GET_FORM(ARG(0));
   long index = GET_FIXNUM(ARG(1));
   
   switch (GET_VECTOR_CODE(ARG(0)))
   {
   case VT_T:
      COPY(OFFSET(AR_BASE(vector), index), ARG(0));
      break;
   case VT_FIXNUM:
      LOAD_FIXNUM(ARG(0), FIXNUM_AR(vector)[index], ARG(0));
      break;
   case VT_FLOAT:
      LOAD_FLOAT(ARG(0), &FLOAT_AR(vector)[index], ARG(0));
      break;
   case VT_CHARACTER:
      LOAD_CHAR(ARG(0), CHAR_AR(vector)[index], ARG(0));
      break;
   case VT_BIT:
      rt_sbvref(base);
      break;
   default:
      Labort("unknown array type");
   }
}
   
/*------------------------------------------------------------------------------
 * RT::SET-PVREF value vector index
 *----------------------------------------------------------------------------*/
void rt_set_pvref(base)
CL_FORM *base;
{
   CL_FORM *vector = GET_FORM(ARG(1));
   long index = GET_FIXNUM(ARG(2));
   
   switch (GET_VECTOR_CODE(ARG(1)))
   {
   case VT_T:
      COPY(ARG(0), OFFSET(AR_BASE(vector), index));
      break;
   case VT_FIXNUM:
      FIXNUM_AR(vector)[index] = GET_FIXNUM(ARG(0));
      break;
   case VT_FLOAT:
      FLOAT_AR(vector)[index] = GET_FLOAT(ARG(0));
      break;
   case VT_CHARACTER:
      CHAR_AR(vector)[index] = GET_CHAR(ARG(0));
      break;
   case VT_BIT:
      rt_set_sbvref(base);
      break;
   default:
      Labort("unknown array type");
   }
}

/*------------------------------------------------------------------------------
 * RT::SBVREF bit-vector index
 *----------------------------------------------------------------------------*/
void rt_sbvref(base)
CL_FORM *base;
{
   long *bit_vector = BIT_AR(GET_FORM(ARG(0)));
   long index = GET_FIXNUM(ARG(1));
   
   if (bit_vector[index / BITS_PER_FIXNUM] & (1L << (index % BITS_PER_FIXNUM)))
      LOAD_FIXNUM(ARG(0), 1, ARG(0));
   else
      LOAD_FIXNUM(ARG(0), 0, ARG(0));
}

/*------------------------------------------------------------------------------
 * RT::SET-SBVREF value bit-vector index
 *----------------------------------------------------------------------------*/
void rt_set_sbvref(base)
CL_FORM *base;
{
   long *bit_vector = BIT_AR(GET_FORM(ARG(1)));
   long index = GET_FIXNUM(ARG(2));
   
   if (GET_FIXNUM(ARG(0)))
      bit_vector[index / BITS_PER_FIXNUM] |= (1L << (index % BITS_PER_FIXNUM));
   else
      bit_vector[index / BITS_PER_FIXNUM] &=~(1L << (index % BITS_PER_FIXNUM));
}

/*------------------------------------------------------------------------------
 * RT::BITOP opcode bit-array1 bit-array2 result-bit-array
 *----------------------------------------------------------------------------*/
void rt_bitop(base)
CL_FORM *base;
{
   long *a1 = BIT_AR(GET_FORM(ARG(1)));
   long *a2 = BIT_AR(GET_FORM(ARG(2)));
   long *a3 = BIT_AR(GET_FORM(ARG(3)));
   long i, size = AR_SIZE(GET_FORM(ARG(1)));
   
   size = (size + BITS_PER_FIXNUM - 1) / BITS_PER_FIXNUM;
   
   switch (GET_FIXNUM(ARG(0)))
   {
   case 0:                      /* and */
      for (i = 0; i < size; i++)
         a3[i] = a1[i] & a2[i];
      break;
   case 1:                      /* ior */
      for (i = 0; i < size; i++)
         a3[i] = a1[i] | a2[i];
      break;
   case 2:                      /* xor */
      for (i = 0; i < size; i++)
         a3[i] = a1[i] ^ a2[i];
      break;
   case 3:                      /* eqv */  
      for (i = 0; i < size; i++)
         a3[i] = ~(a1[i] ^ a2[i]);
      break;
   case 4:                      /* nand */
      for (i = 0; i < size; i++)
         a3[i] = ~(a1[i] & a2[i]);
      break;
   case 5:                      /* nor */
      for (i = 0; i < size; i++)
         a3[i] = ~(a1[i] | a2[i]);
      break;
   case 6:                      /* andc1 */
      for (i = 0; i < size; i++)
         a3[i] = ~a1[i] & a2[i];
      break;
   case 7:                      /* andc2 */
      for (i = 0; i < size; i++)
         a3[i] = a1[i] & ~a2[i];
      break;
   case 8:                      /* orc1 */
      for (i = 0; i < size; i++)
         a3[i] = ~a1[i] | a2[i];
      break;
   case 9:                      /* orc2 */
      for (i = 0; i < size; i++)
         a3[i] = a1[i] | ~a2[i];
      break;
   case 10:                     /* not */
      for (i = 0; i < size; i++)
         a3[i] = ~a1[i];
      break;
   default:
      Labort("unknown opcode for bit-op");
   }
   COPY(ARG(3), ARG(0));
}

/*------------------------------------------------------------------------------
 * RT::SHRINK-SMSTR simple-strign new-size
 *----------------------------------------------------------------------------*/
void rt_shrink_smstr(base)
CL_FORM *base;
{
   long new_size = GET_FIXNUM(ARG(1));
   /* Neue Endnull, falls String später in C-String verwandelt werden soll */
   CHAR_AR(AR_BASE(GET_FORM(ARG(0))))[new_size] = '\0';
   SET_AR_SIZE(new_size, GET_FORM(ARG(0)));
}
