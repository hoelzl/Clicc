/*------------------------------------------------------------------------------
 * Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel
 *-----------------------------------------------------------------------------
 * Projekt  : APPLY - A Practicable And Portable Lisp Implementation
 *            ------------------------------------------------------
 *  Funktion : Laufzeitsystem
 *             - Restaurieren von Special-Variablen
 *             - Special-Forms UNWIND-PROTECT, CATCH, THROW
 *               implementiert als Systemfunktionen.
 *
 * $Revision: 1.10 $
 * $Log: catch.c,v $
 * Revision 1.10  1994/01/05  12:46:50  sma
 * Namensänderung: Alle Laufzeitsystemfunktionen mit dem Präfix rt_
 * versehen und den Postfix _internal entfernt. STACK(base,x) -> ARG(x)
 *
 * Revision 1.9  1993/07/06  16:27:42  sma
 * OFFSET-Makro eingeführt.
 *
 * Revision 1.8  1993/06/16  14:43:22  hk
 * Copyright Notiz eingefuegt.
 *
 * Revision 1.7  1993/04/22  10:29:34  hk
 * fun_decl.h -> sys.h.
 *
 * Revision 1.6  1993/02/17  15:29:47  hk
 * CLICC -> APPLY, Revison Keyword.
 *
 * Revision 1.5  1992/09/28  17:20:28  hk
 * Lerror -> Labort, neues Lerror mit Lisp-Parameter
 *
 * Revision 1.4  1992/07/28  10:14:34  hk
 * Schreibfehler.
 *
 * Revision 1.3  1992/06/05  14:22:10  hk
 * Funktion throw_internal hinzugefuegt, die MV auf dem Stack erwartet.
 * catch_internal angepasst, so dass es MV auf dem Stack erwartet.
 *
 * Revision 1.2  1992/06/04  07:19:41  hk
 * Umgestellt auf Continuations
 *
 * Revision 1.1  1992/03/24  17:03:37  hk
 * Initial revision
 *----------------------------------------------------------------------------*/

#include <c_decl.h>
#include "sys.h"

/*------------------------------------------------------------------------------
 * Fehlermeldung
 *----------------------------------------------------------------------------*/
char NO_CONT[] = "Continuation called outside of its dynamic extent";
char NO_CATCH[] = "Called non existing catch";

/*------------------------------------------------------------------------------
 * lokale Konstanten
 *----------------------------------------------------------------------------*/
#define BINDINGS_STACK_SIZE 1000

/*------------------------------------------------------------------------------
 * globale Variablen
 *----------------------------------------------------------------------------*/

/*------------------------------------------------------------------------------
 * Binding-Stack wachst von niedrigen zu hohen Adressen; 'bind_top' zeigt auf 
 * den naechsten freien Eintrag.                   
 *----------------------------------------------------------------------------*/
CL_FORM bind_stack[BINDINGS_STACK_SIZE];
CL_FORM *bind_top = bind_stack;

/*------------------------------------------------------------------------------
 * die zuletzt installierte Continuation
 *----------------------------------------------------------------------------*/
CONTENV *last_cont = NULL;

/*------------------------------------------------------------------------------
 * unwind_to
 * Binding-Stack abarbeiten, bis 'bind_top' den Wert 'saved_bind_top' hat.
 * ACHTUNG, hier wird davon ausgegangen, dass 'bind_top' von kleinen
 * zu grossen Adressen waechst.
 *----------------------------------------------------------------------------*/
void unwind_to(saved_bind_top)
CL_FORM *saved_bind_top;
{
   while (bind_top > saved_bind_top)
   {
      RESTORE_SPECIAL;
   }
}

/*------------------------------------------------------------------------------
 *  RT::CATCH tag body (funarg)
 *
 *  CATCH erzeugt evtl. MV
 *----------------------------------------------------------------------------*/
void rt_catch (base)
CL_FORM *base;
{
   CONTENV new_cont;
   CL_FORM *caller_base;
   
   new_cont.bind_top = bind_top;
   new_cont.last = last_cont;
   last_cont = &new_cont;
   if ((caller_base = (CL_FORM *)SETJMP(new_cont.jmp_buf)) == NULL)
   {
      /* Nicht von throw */
      /* Aufruf des Rumpfes */
      Ffuncall(ARG(1), 1);
      COPY(ARG(1), ARG(0));

      /* aktuellen Catcher entfernen */
      last_cont = new_cont.last;
   }
   else
   {
      /* Kontroll Transfer von Throw */
      /* aktuellen Catcher entfernen */
      last_cont = new_cont.last;

      /* Pruefen, ob der 1. Parameter von THROW gleich dem 1. Parameter */
      /* von CATCH ist */
      if (EQ(OFFSET(caller_base, 0), ARG(0)))
      {
         /* Passendes Tag gefunden */
         /* Der 2. Parameter von THROW wird das Resultat von CATCH. */
         /* weiter Werte evtl. in 'mv_buf' */
         Fvalues(OFFSET(caller_base, 1), mv_count);
         COPY(OFFSET(caller_base, 1), ARG(0));
      }
      else
         call_cont(caller_base);
   }
}

/*------------------------------------------------------------------------------
 * LISP Parameter:
 * ARG(0): Tag
 * ARG(1): Resultat (+ evtl. Multiple-Values)
 *----------------------------------------------------------------------------*/
void call_cont (base)
CL_FORM *base;
{
   /* kein normales Error benutzen, weil das 'call_cont' benutzten wuerde */
   if (last_cont == NULL)
      Labort(NO_CONT);
   unwind_to(last_cont->bind_top);
   LONGJMP(last_cont->jmp_buf, base);
}

/*------------------------------------------------------------------------------
 * RT::THROW tag result (evtl. Multiple-Values)
 *----------------------------------------------------------------------------*/
void rt_throw(base, nargs)
CL_FORM *base;
int nargs;
{
   /* kein normales Error benutzen, weil das 'call_cont' benutzten wuerde */
   if (last_cont == NULL)
      Labort(NO_CATCH);
   unwind_to(last_cont->bind_top);
   mv_count = nargs - 1;
   LONGJMP(last_cont->jmp_buf, base);
}

/*------------------------------------------------------------------------------
 * (unwind-protect protected-form {cleanup-form}*)
 *
 * --Pass 1-->
 *
 * (rt::unwind-protect
 *    (function (lambda () protected-form))
 *    (function (lambda () {cleanup-form}*))
 * )
 * LISP Parameter:
 * ARG(0): protected Form (Funarg)
 * ARG(1): cleanup Forms (Funarg)
 *
 * unwind-protect erzeugt soviele MV, wie protected-form
 *----------------------------------------------------------------------------*/
void rt_unwind_protect (base)
CL_FORM *base;
{
   CONTENV new_cont;
   CL_FORM *caller_base;
   int local_mv_count;

   new_cont.bind_top = bind_top;
   new_cont.last = last_cont;
   last_cont = &new_cont;
   if((caller_base = (CL_FORM *)SETJMP(new_cont.jmp_buf)) == NULL)
   {
      /* Nicht von throw */
      /* Aufruf der protected Form; Cleanup-Form nicht ueberschreiben */
      COPY(ARG(0), ARG(2));
      Ffuncall(ARG(2), 1);

      /* aktuellen Catcher entfernen */
      last_cont = new_cont.last;

      local_mv_count = mv_count;
      if(local_mv_count > 1)
      {
         /* Multiple-Values die von Protected-Form erzeugt wurde, retten,
            da 'mv_buf' evtl. von Cleanup-Forms benutzt wird. */
         save_values(ARG(2));
      }

      /* Resultat von Protected-Form wird Resultat von UNWIND-PROTECT */
      COPY(STACK(base,2), ARG(0));
      /* Aufruf der Cleanup-Form OBERHALB des Resultats der Protected-Form */
      Ffuncall(ARG(1), 1);

      if(local_mv_count > 1)
      {
         /* Multiple-Values restaurieren */
         Fvalues_list(ARG(0));
      }
   }
   else
   {
      /* Kontroll Transfer von call_cont */
      /* wie bei CATCH, aber nicht Tag pruefen, sondern Cleanup-Form immer */
      /* ausfuehren und mit 'call_cont' weitermachen */

      /* aktuelle Continuation entfernen */
      last_cont = new_cont.last;

      local_mv_count = mv_count;

      if(local_mv_count > 1)
      {
         /* Die von 'call_cont' erzeugten Multiple-Values retten, */
         /* da mv_buf evtl. von Cleanup-Forms benutzt wird. */
         save_values(OFFSET(caller_base, 1));
      }

      /* Aufruf der Cleanup-Form im Stack oberhalb der beiden Argumente */
      /* von THROW  */
      COPY(ARG(1), OFFSET(caller_base, 2));
      Ffuncall(OFFSET(caller_base, 2), 1);

      if(local_mv_count > 1)
      {
         /* Multiple-Values restaurieren */
         Fvalues_list(OFFSET(caller_base, 1));
      }
      call_cont(caller_base);
   }
}
