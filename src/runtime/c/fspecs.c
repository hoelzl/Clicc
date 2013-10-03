/*------------------------------------------------------------------------------
 * Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel
 *------------------------------------------------------------------------------
 * Projekt  : APPLY - A Practicable And Portable Lisp Implementation
 *            ------------------------------------------------------
 * Funktion : Funktionen zur Berechnung der Kenndaten der Floatingpointrepr.
 *
 * $Revision: 1.3 $
 * $Log: fspecs.c,v $
 * Revision 1.3  1994/05/24  14:05:46  sma
 * rt::most-positive-fixnum, rt::most-negaitiv-fixnum berechnen selbige
 * Konstanten.
 *
 * Revision 1.2  1994/04/28  09:46:23  sma
 * LOAD_FIXNUM, LOAD_CHAR und LOAD_FLOAT um 3. Argument ergänzt.
 *
 * Revision 1.1  1994/01/21  15:10:38  ft
 * Initial revision
 *
 *----------------------------------------------------------------------------*/

#include <c_decl.h>
#include "sys.h"

#ifdef __STDC__

#include <float.h>

LISP_FUN(rt_calc_radix)
{
   LOAD_FIXNUM(ARG(0), FLT_RADIX, ARG(0));
}

LISP_FUN(rt_calc_mant_dig)
{
   LOAD_FIXNUM(ARG(0), FLT_MANT_DIG, ARG(0));
}

#else

LISP_FUN(rt_calc_radix)
{
   double a, b;

   for (a = 1.0; ((a + 1.0) - a) - 1.0 == 0.0; a += a)
      ;
   for (b = 1.0; (a + b) - a == 0.0; b += b)
      ;
   LOAD_FIXNUM(ARG(0), (a + b) - a, ARG(0));
}

LISP_FUN(rt_calc_mant_dig)
{
   double a, b, beta;
   int it;
   
   for (a = 1.0; ((a + 1.0) - a) - 1.0 == 0.0; a += a)
      ;
   for (b = 1.0; (a + b) - a == 0.0; b += b)
      ;
   beta = (a + b) - a;
   for (b = 1.0, it = 0; (((b + 1.0) - b) - 1.0) == 0.0; it++, b *= beta)
      ;
   LOAD_FIXNUM(ARG(0), it, ARG(0));
}

#endif

LISP_FUN(rt_most_positive_fixnum)
{
   long i1 = 1, i2 = 0;
   
   while (i1 > i2)
   {
      i2 = i1;
      i1 = i1 * 2 + 1;
   }
   i2 >>= TAG_BITS;
   LOAD_FIXNUM(ARG(0), i2, ARG(0));
}

LISP_FUN(rt_most_negative_fixnum)
{
   long i1 = -1, i2 = 0;

   while (i1 < i2)
   {
      i2 = i1;
      i1 = i1 * 2 - 1;
   }
   i2 >>= TAG_BITS;
   LOAD_FIXNUM(ARG(0), i2, ARG(0));
}


