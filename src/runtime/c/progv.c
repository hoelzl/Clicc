/*------------------------------------------------------------------------------
 * Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel
 *------------------------------------------------------------------------------
 * Projekt  : APPLY - A Practicable And Portable Lisp Implementation
 *            ------------------------------------------------------
 * Funktion : Laufzeitsystem
 *            - Special Form PROGV implementiert als Systemfunktion
 *
 * $Revision: 1.7 $
 * $Log: progv.c,v $
 * Revision 1.7  1994/04/23  17:01:02  sma
 * STACK(base, x) -> ARG(x) sowie korrekte Prüfung auf konstante Symbole.
 *
 * Revision 1.6  1994/01/05  12:53:49  sma
 * Namensänderung: rt-progv-internal mit dem Präfix rt_ versehen und den
 * Postfix _internal entfernt. STACK(base,x) -> ARG(x)
 *
 * Revision 1.5  1993/06/16  14:43:22  hk
 * Copyright Notiz eingefuegt.
 *
 * Revision 1.4  1993/04/22  10:29:34  hk
 * fun_decl.h -> sys.h.
 *
 * Revision 1.3  1993/02/17  15:42:59  hk
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

char SYM_EXPECTED[] = "~a is not a symbol";
char TRY_BIND_CONST[] = "can not bind ~a, which is a constant";

/*------------------------------------------------------------------------------
 * RT::PROGV symbol-list value-list body-function
 *----------------------------------------------------------------------------*/
void rt_progv(base)
CL_FORM *base;
{
   CL_FORM *syms = ARG(0), *vals = ARG(1);
   CL_FORM *saved_bind_top = bind_top; 
   
   while(CL_CONSP(syms))
   {
      syms = GET_CAR(syms);
      if(!CL_SYMBOLP(syms))
      {
         COPY(syms, ARG(0)); 
         Lerror(ARG(0), SYM_EXPECTED);
      }
      else if(SYM_IS_CONST(syms))
      {
         COPY(syms, ARG(0)); 
         Lerror(ARG(0), TRY_BIND_CONST);
      }
      else
      {
         /* Symbol + bisherigen Wert auf Binding-Stack retten. */
         /* -------------------------------------------------- */
         SAVE_SPECIAL(GET_FORM(syms));
         if(CL_CONSP(vals))
         {
            /* es sind noch Werte da */
            /* --------------------- */
            vals = GET_CAR(vals);
            COPY(vals, SYM_VALUE(syms));
            vals = CDR(vals);
         }
         else
         {
            /* Liste der Werte ist aufgebraucht */
            /* -------------------------------- */
            LOAD_UNBOUND(SYM_VALUE(syms));
         }
         syms = CDR(syms);
      }
   }

   /* Aufruf des Rumpfes */
   /* ------------------ */
   COPY(ARG(2), ARG(0));
   Ffuncall(ARG(0), 1);

   /* dynamische Variablen restaurieren */
   /* --------------------------------- */
   unwind_to(saved_bind_top);
}



