/*------------------------------------------------------------------------------
 * Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel
 *------------------------------------------------------------------------------
 * Projekt  : APPLY - A Practicable And Portable Lisp Implementation
 *            ------------------------------------------------------
 * Funktion : Laufzeitsystem
 *            - VALUES
 *            - VALUES-LIST
 *            - save_values
 *
 * $Revision: 1.8 $
 * $Log: values.c,v $
 * Revision 1.8  1994/04/28  09:54:41  sma
 * Fehler korrigiert, der durch das entfernen von OFFSET entstanden ist.
 *
 * Revision 1.7  1994/04/22  14:13:07  sma
 * STACK(base, x) -> ARG(x) und "int mv_args" entfernt.
 *
 * Revision 1.6  1993/07/06  11:12:48  sma
 * OFFSET-Makro eingeführt
 *
 * Revision 1.5  1993/06/16  14:43:22  hk
 * Copyright Notiz eingefuegt.
 *
 * Revision 1.4  1993/04/22  10:29:34  hk
 * fun_decl.h -> sys.h.
 *
 * Revision 1.3  1993/02/17  15:51:17  hk
 * CLICC -> APPLY, Revison Keyword.
 *
 * Revision 1.2  1992/09/28  17:20:28  hk
 * Lerror -> Labort, neues Lerror mit Lisp-Parameter
 *
 * Revision 1.1  1992/03/24  17:03:37  hk
 * Initial revision
 *----------------------------------------------------------------------------*/

#include <c_decl.h>
#include "sys.h"

char TOO_MANY_VALUES[] = "Too many values";
char LIST_EXPECTED[] = "~A is not a list";

#define MV_LIMIT 20
#define MV_BUFSIZE (MV_LIMIT - 1)

int mv_count;
CL_FORM mv_buf[MV_BUFSIZE];

/*------------------------------------------------------------------------------
 * die Systemfunktion VALUES                                               
 *----------------------------------------------------------------------------*/
void Fvalues(base, nargs)
CL_FORM *base;
int nargs;
{
   if(nargs == 0)
   {
      LOAD_NIL(ARG(0));
   }
   else if(nargs > MV_LIMIT)
      Labort(TOO_MANY_VALUES);
   else
   {
      int i;

      for(i = 1; i < nargs; i++)
      {
         COPY(ARG(i), &mv_buf[i-1]);
      }
   }
   mv_count = nargs;
}

/*------------------------------------------------------------------------------
 * die Systemfunktion VALUES-LIST                                         
 *----------------------------------------------------------------------------*/
void Fvalues_list (base)
CL_FORM *base;
{
   CL_FORM *values = ARG(0);

   mv_count = 0;

   switch(TYPE_OF(values))
   {
   case CL_NIL:
      /* NIL auf dem Stack */
      return;

   case CL_CONS:
      /* Zeiger auf das 1. Element der Liste */
      values = GET_CAR(values);
      /* 1. Element auf den Stack */
      COPY(values, ARG(0));
      values++;
      break;

   default:
      Lerror(ARG(0), LIST_EXPECTED);
      break;
   }

   while(CL_CONSP(values))
   {
      /* mv_count zunaechst als Index in mv_buf benutzen */
      if(mv_count >= MV_BUFSIZE)
         Labort(TOO_MANY_VALUES);
      values = GET_CAR(values);
      COPY(values, &mv_buf[mv_count]);
      mv_count++;
      values++;
   }

   /* den Eintrag auf dem Stack mitzaehlen */
   mv_count++;
}

/*------------------------------------------------------------------------------
 * base zeigt auf einen Wert im LISP-Stack; dieser Wert wird ersetzt durch
 * eine Liste, bestehend aus dem Wert und dem aktuellen Inhalt von mv_buf
 *----------------------------------------------------------------------------*/
void save_values (base)
CL_FORM *base;
{
   int i;

   for(i = 1; i < mv_count; i++)
      COPY(&mv_buf[i-1], ARG(i));
   Flist(ARG(0), mv_count);
   mv_count = 1;
}
