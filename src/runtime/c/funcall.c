/*------------------------------------------------------------------------------
 * Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel
 *------------------------------------------------------------------------------
 * Projekt  : APPLY - A Practicable And Portable Lisp Implementation
 *            ------------------------------------------------------
 * Funktion : Laufzeitsystem
 *            - FUNCALL, APPLY
 *
 * $Revision: 1.13 $
 * $Log: funcall.c,v $
 * Revision 1.13  1994/05/18  15:19:04  sma
 * Makros für Zugriff auf funktionale Objekte nach obrepX verlagert.
 *
 * Revision 1.12  1994/02/03  17:34:08  sma
 * Änderungen für Optimierung von &rest-Paramtern.
 *
 * Revision 1.11  1994/01/25  15:15:21  sma
 * STACK(base, x) -> ARG(x)
 *
 * Revision 1.10  1993/06/16  14:43:22  hk
 * Copyright Notiz eingefuegt.
 *
 * Revision 1.9  1993/04/22  10:29:34  hk
 * fun_decl.h -> sys.h.
 *
 * Revision 1.8  1993/04/06  17:22:16  hk
 * Tabs entfernt.
 *
 * Revision 1.7  1993/03/25  12:55:43  sma
 * neuer Look
 *
 * Revision 1.6  1993/02/17  15:37:21  hk
 * CLICC -> APPLY, Revison Keyword.
 *
 * Revision 1.5  1992/10/07  15:56:21  hk
 * Schreibfehler.
 *
 * Revision 1.4  1992/09/28  17:20:28  hk
 * Lerror -> Labort, neues Lerror mit Lisp-Parameter
 *
 * Revision 1.3  1992/09/23  14:46:39  hk
 * Falls funcall mit einem Argument aufgerufen wird, das keine Funktion
 * ist, dann wird dieses Argument in der Fehlermeldung angegeben.
 *
 * Revision 1.2  1992/04/15  11:09:08  hk
 * Aufruf von Continuation eingefuegt.
 *
 * Revision 1.1  1992/03/24  17:03:37  hk
 * Initial revision
 *----------------------------------------------------------------------------*/

#include <c_decl.h>
#include "sys.h"

char TOO_FEW_ARGS[] = "too few arguments";

/*----------------------------------------------------------------------------*/
#define CHK_GENAU(parspec) ((parspec) >= 0)
#define CHK_MIN(parspec) ((parspec) < 0)
#define GET_GENAU(parspec) ((parspec))
#define GET_MIN(parspec) (- (parspec) - 1)
#define CREATE_GENAU(count) (count)
#define CREATE_MIN(count) (- (count) - 1)

/*------------------------------------------------------------------------------
 * FUNCALL fn &rest arguments
 *------------------------------------------------------------------------------
 * Moegliche Typen:
 * - CL_CLOSURE: (zeigt auf eine Folge von LISP-Daten)
 *   - Laenge
 *   - Zeiger auf Code
 *   - Parameter-Spezifikation
 *   - .. - Informationen ueber freie Variablen und Funktionen
 *
 * - CL_GLOBFUN: (zeigt auf eine C-Structur in einem globalen C-Array)
 *   - Zeiger auf Code
 *   - Parameter Spezifikation
 *
 * - CL_DOWNFUN: (zeigt auf eine C-Struktur im C-Stack)
 *   - Zeiger auf Code
 *   - Parameter-Spezifikation
 *   - Zeiger auf Display-Array
 * - CL_UNIQUE_TAG: (Zahl, die eine Continuation eindeutig identifiziert)
 *----------------------------------------------------------------------------*/
void Ffuncall(base, nargs)
CL_FORM *base;
int nargs;
{
   int par_spec;

   nargs--;
   switch (TYPE_OF(ARG(0)))
   {
      case CL_GLOBFUN:
         par_spec = GET_GLOBFUN_PARSPEC(GET_GFARG(ARG(0)));
         if (CHK_GENAU(par_spec))
         {
            /* feste Anzahl von Parametern
             * Anzahl der Parameter wird ueberprueft*/
            if (nargs != GET_GENAU(par_spec))
               Labort(ILLEGAL_ARGS);
            /* Aufruf der Funktion, Parameter sind geprueft */
            GET_GLOBFUN_CODE(GET_GFARG(ARG(0))) (ARG(1));
         }
         else
         {
            /* Mindest-Anzahl der Parameter wird ueberprueft */
            if (nargs < GET_MIN(par_spec))
               Labort(TOO_FEW_ARGS);
            /* Aufruf der Funktion, Mindest-Parameter-Zahl ist geprueft */
            GET_GLOBFUN_CODE(GET_GFARG(ARG(0))) (ARG(1), nargs);
         }
         /* das Resultat an die Stelle kopieren, wo es von der aufrufenden
          * Funktion erwartet wird.*/
         COPY(ARG(1), ARG(0));
         break;

      case CL_CLOSURE:
         par_spec = GET_CLOSURE_PARSPEC(ARG(0));
         if (CHK_GENAU(par_spec))
         {
            /* feste Anzahl von Parametern
             * Anzahl der Parameter wird ueberprueft*/
            if (nargs != GET_GENAU(par_spec))
               Labort(ILLEGAL_ARGS);
            /* Aufruf der Funktion, Parameter sind geprueft
             * Zeiger auf die Closure-Datenstruktur als 1. Parameter */
            GET_CLOSURE_CODE(ARG(0)) (ARG(0));
         }
         else
         {
            /* Mindest-Anzahl der Parameter wird ueberprueft
             *-----------------------------------------------*/
            if (nargs < GET_MIN(par_spec))
               Labort(TOO_FEW_ARGS);

            /* Aufruf der Funktion, Mindest-Parameter-Zahl ist geprueft
             * Zeiger auf die Closure-Datenstruktur als 1. Parameter
             *-------------------------------------------------------*/
            GET_CLOSURE_CODE(ARG(0)) (ARG(0), nargs);
         }
         /* Resultat wurde schon an der richtigen Position erzeugt
          *-------------------------------------------------------*/
         break;

      case CL_DOWNFUN:
         par_spec = GET_DOWNFUN_PARSPEC(GET_DFARG(ARG(0)));
         if (CHK_GENAU(par_spec))
         {
            /* feste Anzahl von Parametern
             * Anzahl der Parameter wird ueberprueft
             *---------------------------------------*/
            if (nargs != GET_GENAU(par_spec))
               Labort(ILLEGAL_ARGS);

            /* Aufruf der Funktion, Parameter sind geprueft
             *----------------------------------------------*/
            GET_DOWNFUN_CODE(GET_DFARG(ARG(0)))
               (ARG(1), GET_DOWNFUN_DISPLAY(GET_DFARG(ARG(0))));
         }
         else
         {
            /* Mindest-Anzahl der Parameter wird ueberprueft
             *-----------------------------------------------*/
            if (nargs < GET_MIN(par_spec))
               Labort(TOO_FEW_ARGS);

            /* Aufruf der Funktion, Mindest-Parameter-Zahl ist geprueft
             *----------------------------------------------------------*/
            GET_DOWNFUN_CODE(GET_DFARG(ARG(0)))
               (ARG(1), GET_DOWNFUN_DISPLAY(GET_DFARG(ARG(0))), nargs);

         }
         /* das Resultat an die Stelle kopieren, wo es von der aufrufenden
          * Funktion erwartet wird.
          *-------------------------*/
         COPY(ARG(1), ARG(0));
         break;

      case CL_UNIQUE_TAG:
         if (nargs != 1)
            Labort(ILLEGAL_ARGS);
         call_cont(ARG(0));

      case CL_SYMBOL:
         Lerror(ARG(0), "Symbol ~A is not a function");

      default:
         Lerror(ARG(0), "~A is not a function");
   }
}

/*------------------------------------------------------------------------------
 * APPLY function arg &rest more-args
 *----------------------------------------------------------------------------*/
void Fapply(base, nargs)
CL_FORM *base;
int nargs;
{
   CL_FORM *last = ARG(nargs - 1);
   CL_FORM *list = last;

   nargs--;

   while (CL_CONSP(list))
   {
      list = GET_CAR(list);
      COPY(list, last);
      list++;
      last++;
      nargs++;
   }
   Ffuncall(base, nargs);
}

/*------------------------------------------------------------------------------
 * rest_apply
 *----------------------------------------------------------------------------*/
void rest_apply(base, nargs, len, rest)
CL_FORM *base;
int nargs, len;
CL_FORM *rest;
{
   CL_FORM *last = ARG(nargs);
   while (len-- > 0)
   {
      COPY(rest, last);
      rest++;
      last++;
      nargs++;
   }
   Ffuncall(base, nargs);
}
