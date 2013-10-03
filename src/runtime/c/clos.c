/*------------------------------------------------------------------------------
 * Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel
 *-----------------------------------------------------------------------------
 * Projekt  : APPLY - A Practicable And Portable Lisp Implementation
 *            ------------------------------------------------------
 * Funktion : Laufzeitsystem: Funktionen des Objektsystems
 *
 * $Revision: 1.16 $
 * $Log: clos.c,v $
 * Revision 1.16  1994/04/23  16:20:37  sma
 * Kosmetik.
 *
 * Revision 1.15  1994/01/05  12:47:42  sma
 * Namensänderung: Alle Laufzeitsystemfunktionen mit dem Präfix rt_
 * versehen und den Postfix _internal entfernt. STACK(base,x) -> ARG(x)
 *
 * Revision 1.14  1993/09/13  11:51:20  sma
 * Fehler in Längenangaben von Arrays, Vectoren und Instanzen beseitigt
 * durch Einführen des SET_AR_SIZE-Makros.
 *
 * Revision 1.13  1993/08/30  15:45:41  sma
 * Syntaxfehler korrigiert
 *
 * Revision 1.12  1993/08/30  15:35:37  sma
 * Syntaxfehler korrigiert
 *
 * Revision 1.11  1993/08/30  13:20:03  sma
 * Instanzen werden jetzt direkt statt ueber make-array erzeugt.
 *
 * Revision 1.10  1993/08/18  15:27:25  ft
 * Da die Funktionen instancep, instance-ref, instance-set und
 * set-slot-unbound inline compiliert werden, konnten sie hier gestrichen
 * werden.
 *
 * Revision 1.9  1993/08/10  11:47:45  ft
 * instancep benutzt jetzt ein C-Makro.
 *
 * Revision 1.8  1993/07/14  08:53:40  ft
 * Änderung der Parameterreihenfolge in instance-ref/set.
 *
 * Revision 1.7  1993/07/06  16:16:38  sma
 * OFFSET-Makro eingeführt.
 *
 * Revision 1.6  1993/06/16  14:43:22  hk
 * Copyright Notiz eingefuegt.
 *
 * Revision 1.5  1993/04/22  10:29:34  hk
 * fun_decl.h -> sys.h.
 *
 * Revision 1.4  1993/04/15  13:06:13  ft
 * Erweiterung um set-slot-unbound.
 *
 * Revision 1.3  1993/03/12  10:00:10  ft
 * Zugriff auf Instanzen optimiert.
 *
 * Revision 1.2  1993/02/17  15:32:05  hk
 * CLICC -> APPLY, Revison Keyword.
 *
 * Revision 1.1  1992/12/11  07:20:27  ft
 * Initial revision
 *----------------------------------------------------------------------------*/

#include <c_decl.h>
#include "sys.h"

/*------------------------------------------------------------------------------
 * lokale Konstante
 *----------------------------------------------------------------------------*/
#define INSTANCE_HEADER 1


/*------------------------------------------------------------------------------
 * RT::MAKE-INSTANCE size
 *----------------------------------------------------------------------------*/
LISP_FUN(rt_make_instance)
{
   long size = GET_FIXNUM(ARG(0));
   CL_FORM *vector;

   vector = form_alloc(ARG(0), size + INSTANCE_HEADER);
   INIT_INSTANCE(vector, size);
   LOAD_INSTANCE(vector, ARG(0));
}

