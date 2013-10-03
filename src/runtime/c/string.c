/*------------------------------------------------------------------------------
 * Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel
 *------------------------------------------------------------------------------
 * Projekt  : APPLY - A Practicable And Portable Lisp Implementation
 *            ------------------------------------------------------
 * Funktion : System-Funktionen: Strings
 *
 * $Revision: 1.10 $
 * $Log: string.c,v $
 * Revision 1.10  1994/01/05  12:54:56  sma
 * make-string geändert. INIT_VEC_CHAR und LOAD_VEC_CHAR Makros
 * eingefügt.
 *
 * Revision 1.9  1993/12/09  15:16:25  sma
 * Aufgrund der neuen array-Repräsentation konnten alle string accessor
 * Funktionen in Lisp programmiert werden und hier rausfliegen.
 * get_c_string kann jetzt nur noch den C-String eines uebergebenen Lisp
 * simple-strings ermitteln. STACK(base, xxx) -> ARG(xxx)
 *
 * Revision 1.8  1993/10/13  18:16:01  sma
 * 1x TYPE_OF durch CL_STRINGP ersetzt und das Setzen von AR_SIZE in
 * make_string_internal korrigiert.
 *
 * Revision 1.7  1993/08/26  15:58:21  hk
 * make_string_internal: Typ void
 *
 * Revision 1.6  1993/06/16  14:43:22  hk
 * Copyright Notiz eingefuegt.
 *
 * Revision 1.5  1993/05/08  18:17:56  hk
 * Argumentreihenfolge von set-char-internal und set-schar-internal
 * geaendert.
 *
 * Revision 1.4  1993/04/22  10:29:34  hk
 * fun_decl.h -> sys.h.
 *
 * Revision 1.3  1993/02/17  15:46:30  hk
 * CLICC -> APPLY, Revison Keyword.
 *
 * Revision 1.2  1993/01/08  09:44:13  hk
 * Namen C_ nach c_.
 *
 * Revision 1.1  1992/03/24  17:03:37  hk
 * Initial revision
 *----------------------------------------------------------------------------*/

#include <c_decl.h>
#include "sys.h"
#include <string.h>

/*------------------------------------------------------------------------------
 * Ermittelt den 'eigentlichen' String eines LISP-Strings
 * Resultat: Zeiger auf den String
 *----------------------------------------------------------------------------*/
char *get_c_string (lisp_string)
CL_FORM *lisp_string;
{
   return AR_STRING(GET_FORM(lisp_string));
}

/*------------------------------------------------------------------------------
 * Erzeugt aus einem C-String einen LISP-String nach 'base'
 *----------------------------------------------------------------------------*/
void make_string(base, string)
CL_FORM *base;
char    *string;
{
   long str_len = strlen(string);
   CL_FORM *header;
   char *chptr;

   chptr = char_alloc(ARG(0), str_len);
   strncpy(chptr, string, str_len);
   header = form_alloc(ARG(0), 2L);
   LOAD_CHAR_PTR(chptr, AR_BASE(header));
   INIT_VEC_CHAR(header, str_len);
   LOAD_VEC_CHAR(header, ARG(0));
}
