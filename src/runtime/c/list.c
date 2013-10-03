/*------------------------------------------------------------------------------
 * Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel
 *------------------------------------------------------------------------------
 * Projekt  : APPLY - A Practicable And Portable Lisp Implementation
 *            ------------------------------------------------------
 * Funktion : System-Funktionen: Lists
 *
 * $Revision: 1.11 $
 * $Log: list.c,v $
 * Revision 1.11  1994/04/28  09:49:49  sma
 * Umgeschrieben, damit der Aufbau einer CONS-Zelle mehr abstrahiert
 * wird. Außerdem Fappend optimiert.
 *
 * Revision 1.10  1994/01/05  12:51:03  sma
 * STACK(base, x) -> ARG(x). raw-list-length und simple-assoc gelöscht.
 * raw-list-length wird nicht mehr benötigt, simple-assoc ist (endgültig)
 * in Lisp implementiert.
 *
 * Revision 1.9  1993/09/19  18:13:22  sma
 * raw-list-length ist jetzt eine C-Funktion, und so deutlich schneller.
 * simple-assoc ist jetzt eine C-Funktion für (assoc item alist :test #'eq)
 * und deutlich schneller als die (allgemeine) Lisp-Variante. Wenn assoc
 * nur auf Symbolen arbeitet und so ein eq-Vergleich ausreicht, bitte
 * diesen explizit angeben. Dann wird diese Funktion aufgerufen.
 *
 * Revision 1.8  1993/09/06  16:39:28  sma
 * Laufzeitverhalten optimiert.
 *
 * Revision 1.7  1993/06/16  14:43:22  hk
 * Copyright Notiz eingefuegt.
 *
 * Revision 1.6  1993/04/22  10:29:34  hk
 * fun_decl.h -> sys.h.
 *
 * Revision 1.5  1993/02/17  15:51:57  hk
 * CLICC -> APPLY, Revison Keyword.
 *
 * Revision 1.4  1992/11/26  17:05:38  hk
 * Viele Funktionen von hier nach list.lisp.
 *
 * Revision 1.3  1992/09/28  17:20:28  hk
 * Lerror -> Labort, neues Lerror mit Lisp-Parameter
 *
 * Revision 1.2  1992/07/21  14:58:07  hk
 * Fset_car --> set_car, Fset_cdr --> set_cdr.
 *
 * Revision 1.1  1992/03/24  17:03:37  hk
 * Initial revision
 *----------------------------------------------------------------------------*/

#include <c_decl.h>
#include "sys.h"

/* Laufzeitfehlermeldungen */

char No_list[] = "~a is not a list";

/*------------------------------------------------------------------------------
 * LIST &rest args                                                              
 *----------------------------------------------------------------------------*/
LISP_FUN_NARGS(Flist)
{
   if(nargs == 0)               /* (list) = () */
   {
      LOAD_NIL(ARG(0));
   }
   else
   {
      CL_FORM *lptr, *lp;
      int i;

      lptr = form_alloc(ARG(nargs), nargs * CONS_SIZE);
      nargs--;
      for(lp = lptr, i = 0; i < nargs; lp += CONS_SIZE, i++)
      {
         INIT_CONS(lp);
         COPY(ARG(i),CAR(lp));
         LOAD_CONS(lp + CONS_SIZE, CDR(lp));
      }
      INIT_CONS(lp);
      COPY(ARG(i), CAR(lp));
      LOAD_NIL(CDR(lp));
      LOAD_CONS(lptr, ARG(0));
   }
}

/*------------------------------------------------------------------------------
 * LIST* arg &rest others                                                       
 *----------------------------------------------------------------------------*/
LISP_FUN_NARGS(FlistX)
{
   CL_FORM *lptr, *lp;
   int i;

   if(nargs == 1)               /* (list* x) = x */
      return;

   nargs--;
   lptr = form_alloc(ARG(nargs + 1),  nargs * CONS_SIZE);
   for(lp = lptr, i = 0; i < nargs; lp += CONS_SIZE, i++)
   {
      INIT_CONS(lp);
      COPY(ARG(i), CAR(lp));
      LOAD_CONS(lp + CONS_SIZE, CDR(lp));
   }
   COPY(ARG(i), CDR(lp - CONS_SIZE));
   LOAD_CONS(lptr, ARG(0));
}

/*------------------------------------------------------------------------------
 * APPEND &rest lists                                                           
 *----------------------------------------------------------------------------*/
LISP_FUN_NARGS(Fappend)
{
   int i, list_len = 0;
   CL_FORM *lptr, *lp1, *lp2;

   switch(nargs)
   {
   case 0:                      /* (APPEND) = NIL */
      LOAD_NIL(ARG(0));
      return;
   case 1:                      /* (APPEND arg) = arg */
      return;
   default:
      /* Gesamtlistenlänge der ersten  N - 1 Argumente bestimmen */
      /*---------------------------------------------------------*/
      nargs--;
      for(i = 0; i < nargs; i++) 
      {
         if (CL_CONSP(ARG(i)))
            for (lptr = ARG(i); CL_CONSP(lptr); lptr = GET_CDR(lptr))
               list_len++;
         else if (!CL_NILP(ARG(i)))
            Lerror(ARG(i), No_list);
      }
      
      /* Liste leer, letztes Argument zurückgeben */
      /*------------------------------------------*/
      if(list_len == 0)
      {
         COPY(ARG(nargs), ARG(0));
         return;
      }

      /* Kopieren der ersten N - 1 Listen */
      /*----------------------------------*/
      lptr = form_alloc(ARG(nargs + 1), list_len * CONS_SIZE);

      for (lp1 = lptr, i = 0; i < nargs; i++)
      {
         if (CL_CONSP(ARG(i))) /* nur CONS oder NIL möglich */
         {
            for (lp2 = ARG(i); CL_CONSP(lp2); lp2 = GET_CDR(lp2))
            {
               INIT_CONS(lp1);
               COPY(GET_CAR(lp2), CAR(lp1));
               LOAD_CONS(lp1 + CONS_SIZE, CDR(lp1));
               lp1 += CONS_SIZE;
            }
         }
      }
      /* Anhängen der Restliste */
      /*------------------------*/
      COPY(ARG(i), CDR(lp1 - CONS_SIZE));
      LOAD_CONS(lptr, ARG(0));
   }
}
