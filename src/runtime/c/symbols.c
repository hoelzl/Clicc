/*------------------------------------------------------------------------------
 * Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel
 *-----------------------------------------------------------------------------
 * Projekt  : APPLY - A Practicable And Portable Lisp Implementation
 *            ------------------------------------------------------
 * Funktion : Laufzeitsystem
 *            - SYMBOL-VALUE
 *            - BOUNDP
 *            - SET
 *            - MAKUNBOUND
 *            - SYMBOL-PLIST
 *            - set-symbol-plist
 *            - SYMBOL-NAME
 *            - MAKE-SYMBOL
 *            - SYMBOL-PACKAGE
 *            - set-symbol-package
 *            - set-constant-flag
 *            - setup-symbols-iterator
 *
 * $Revision: 1.19 $
 * $Log: symbols.c,v $
 * Revision 1.19  1994/06/22  13:27:38  hk
 * lisp.h wird nicht eingelesen, sondern nur eine extern Deklaration f"ur
 * die Funktion rt_setup_symbol des Lisp-Moduls eingef"ugt. lisp.h
 * existiert evtl. noch gar nicht, wenn man diese datei "ubersetzt.
 *
 * Revision 1.18  1994/05/20  08:44:38  uho
 * lisp.h mit eingelesen, um Prototypen von Lisp-Funktionen bekanntzumachen.
 *
 * Revision 1.17  1994/01/24  16:41:52  sma
 * Definitiond der Symbole T und NIL gelöscht. Passiert jetzt `erst' im
 * LISP-Modul.
 *
 * Revision 1.16  1994/01/21  13:30:38  sma
 * Fast alle Funktionen werden jetzt in cginline inline-compiliert. Dies
 * ist der bescheidene Rest.
 *
 * Revision 1.15  1994/01/13  16:43:26  sma
 * Änderungen an Funktionen für symbols. Mehr Lisp, weniger C.
 * rt::(set)-struct-ref-internal heißt jetzt rt::structure-ref. Die
 * set-Variante wird jetzt mit setf definiert.
 *
 * Revision 1.14  1994/01/05  12:55:50  sma
 * Namensänderung: Alle Laufzeitsystemfunktionen mit dem Präfix rt_
 * versehen und den Postfix _internal entfernt.
 *
 * Revision 1.13  1993/12/09  17:26:03  sma
 * CL_INIT2-Makros. STACK(base, xxx) -> ARG(xxx). Neu eingerückt.
 *
 * Revision 1.12  1993/09/07  17:19:04  sma
 * MAKE_-Makros eingeführt und setup_symbols_iterator verändert.
 *
 * Revision 1.11  1993/07/06  12:32:50  sma
 * OFFSET-Makro eingeführt.
 *
 * Revision 1.10  1993/06/16  14:43:22  hk
 * Copyright Notiz eingefuegt.
 *
 * Revision 1.9  1993/05/12  11:36:38  hk
 * symbol_package_index definiert, wie Fsymbol_package, aber mit anderem
 * Resultattyp.
 *
 * Revision 1.8  1993/05/08  18:16:23  hk
 * set_symbol_plist -> Fset_symbol_plist, Argumentreihenfolge geaendert.
 *
 * Revision 1.7  1993/04/22  10:23:04  hk
 * fun_decl.h -> sys.h, Symbole NIL + T in Ssys definiert,
 * Funktionen fuer den Zugriff auf Komponenten von Symbolen umgestellt,
 * so dass sie die Komponenten von NIL kennen, auch wenn der Wert einer
 * CL_FORM, die NIL darstellt, keinen Zeiger auf das Symbol NIL enthaelt.
 *
 * Revision 1.6  1993/02/17  15:48:19  hk
 * CLICC -> APPLY, Revison Keyword.
 *
 * Revision 1.5  1993/01/08  09:44:13  hk
 * Namen C_ nach c_.
 *
 * Revision 1.4  1992/09/30  17:25:21  hk
 * unbound_value_p neu.
 *
 * Revision 1.3  1992/09/28  17:20:28  hk
 * Lerror -> Labort, neues Lerror mit Lisp-Parameter
 *
 * Revision 1.2  1992/07/21  14:57:11  hk
 * Fset_symbol_plist --> set_symbol_plist.
 *
 * Revision 1.1  1992/03/24  17:03:37  hk
 * Initial revision
 *----------------------------------------------------------------------------*/

#include <c_decl.h>
#include "sys.h"

/* Referenz in das Lisp-Modul */
/*----------------------------*/
extern void rt_setup_symbol(/* CL_FORM *base */);

/*------------------------------------------------------------------------------
 * RT::MAKE-SYMBOL name
 *----------------------------------------------------------------------------*/
LISP_FUN(rt_make_symbol)
{
   CL_FORM *sym = form_alloc(ARG(1), SYM_SIZE);

   INIT_SYMBOL(sym, ARG(0));  /* benötigt name in ARG(1) */
   LOAD_SYMBOL(sym, ARG(0));
}

/*------------------------------------------------------------------------------
 * RT::SETUP-SYMBOLS-ITERATOR first-sym package-vector
 * Wendet setup-symbol auf alle zur Übersetzungszeit definierten
 * Symbole eines Moduls an.
 *----------------------------------------------------------------------------*/
LISP_FUN(rt_setup_symbols_iterator)
{
   CL_FORM *sym = GET_SYMBOL(ARG(0));
   
   /* das Array ist mit einem END_SYMDEF-Eintrag abgeschlossen */
   /* -------------------------------------------------------- */
   while (!IS_END_SYMDEF(sym))
   {
      LOAD_SYMBOL(sym, ARG(2));
      COPY(ARG(1), ARG(3));
      rt_setup_symbol(ARG(2));
      sym += SYM_SIZE; 
   }
}
