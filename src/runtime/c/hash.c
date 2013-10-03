/*------------------------------------------------------------------------------
 * Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel
 *------------------------------------------------------------------------------
 * Projekt  : APPLY - A Practicable And Portable Lisp Implementation
 *            ------------------------------------------------------
 * Funktion : Laufzeitsystem
 *            - Low Level Hash-Funktionen
 *
 * $Revision: 1.13 $
 * $Log: hash.c,v $
 * Revision 1.13  1994/04/28  09:47:43  sma
 * LOAD_FIXNUM, LOAD_CHAR und LOAD_FLOAT um 3. Argument ergänzt.
 *
 * Revision 1.12  1994/04/23  16:47:33  sma
 * hash() umgeschrieben, so daß keine Länge mehr übergeben werden muß.
 *
 * Revision 1.11  1994/01/24  16:29:02  sma
 * combine-hash jetzt in LISP implementiert.
 *
 * Revision 1.10  1994/01/05  12:48:26  sma
 * Namensänderung: Alle Laufzeitsystemfunktionen mit dem Präfix rt_
 * versehen.
 *
 * Revision 1.9  1993/12/09  15:08:05  sma
 * string-hash und sxhash-simple-string gelöscht und in Lisp
 * programmiert. sxhash-string funktioniert jetzt nur noch mit
 * simple-strings statt mit beliebigen strings. STACK(base, xxx) ->
 * ARG(xxx)
 *
 * Revision 1.8  1993/06/30  16:41:45  hk
 * Neue Funktionen: sxhash_simple_string, sxhash_string, combine_hash.
 *
 * Revision 1.7  1993/06/16  14:43:22  hk
 * Copyright Notiz eingefuegt.
 *
 * Revision 1.6  1993/04/22  10:29:34  hk
 * fun_decl.h -> sys.h.
 *
 * Revision 1.5  1993/02/17  15:42:20  hk
 * CLICC -> APPLY, Revison Keyword.
 *
 * Revision 1.4  1993/01/08  09:44:13  hk
 * Namen C_ nach c_.
 *
 * Revision 1.3  1992/10/05  17:00:26  hk
 * Kommentar zu string-hash korrigiert.
 *
 * Revision 1.2  1992/09/28  17:20:28  hk
 * Lerror -> Labort, neues Lerror mit Lisp-Parameter
 *
 * Revision 1.1  1992/03/24  17:03:37  hk
 * Initial revision
 *----------------------------------------------------------------------------*/

#include <c_decl.h>
#include "sys.h"

/*------------------------------------------------------------------------------
 * Berechnet einen Hash-Index (unsigned long) aus den Zeichen eines C-Strings.
 *----------------------------------------------------------------------------*/
unsigned long hash(str)
char *str;
{
   unsigned char c;
   unsigned long ret = 0;
   
   while ((c = *str++) != 0)
   {
      ret <<= 4;
      ret += c;
   }
   return(ret);
}

/*------------------------------------------------------------------------------
 * RT::SXHASH-STRING string
 * Berechnet einen Hash-Index (Fixnum) aus einem simple-string
 *----------------------------------------------------------------------------*/
LISP_FUN(rt_sxhash_string)
{
   LOAD_FIXNUM(ARG(0), hash(get_c_string(ARG(0))), ARG(0));
}
