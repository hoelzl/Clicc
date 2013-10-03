/*------------------------------------------------------------------------------
 * Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel
 *-----------------------------------------------------------------------------
 * Projekt  : APPLY - A Practicable And Portable Lisp Implementation
 *            ------------------------------------------------------
 * Funktion : System-Funktionen: Structures
 *
 * $Revision: 1.13 $
 * $Log: structure.c,v $
 * Revision 1.13  1994/01/24  16:26:39  sma
 * New-struct bekommt jetzt keinen type mehr uebergeben, dieser wird in
 * LISP zugewiesen.
 *
 * Revision 1.12  1994/01/22  18:20:12  sma
 * Alle anderen funktionen werden jetzt inline-compiliert.
 *
 * Revision 1.11  1994/01/13  16:42:48  sma
 * Quelltext verschönert, Funktionen vereinfacht. (Funktionen sind jetzt
 * bereit, nach gcinline verschoben zu werden.)
 *
 * Revision 1.10  1994/01/05  12:55:16  sma
 * Namensänderung: Alle Laufzeitsystemfunktionen mit dem Präfix rt_
 * versehen und den Postfix _internal entfernt. STACK(base,x) -> ARG(x)
 *
 * Revision 1.9  1993/10/14  12:56:46  sma
 * new-struct-internal gestrichen, dafür new-struct von lisp nach C
 * verschoben und 2x TYPE_OF durch CL_STRUCTP ersetzt.
 *
 * Revision 1.8  1993/07/06  13:21:02  sma
 * OFFSET-Makro eingeführt.
 *
 * Revision 1.7  1993/06/16  14:43:22  hk
 * Copyright Notiz eingefuegt.
 *
 * Revision 1.6  1993/04/22  10:29:34  hk
 * fun_decl.h -> sys.h.
 *
 * Revision 1.5  1993/04/05  10:43:37  hk
 * Resultattypen korrigiert.
 *
 * Revision 1.4  1993/02/17  15:47:36  hk
 * CLICC -> APPLY, Revison Keyword.
 *
 * Revision 1.3  1993/01/11  14:09:05  hk
 * structure -> struct
 *
 * Revision 1.2  1992/09/28  17:20:28  hk
 * Lerror -> Labort, neues Lerror mit Lisp-Parameter
 *
 * Revision 1.1  1992/03/24  17:03:37  hk
 * Initial revision
 *----------------------------------------------------------------------------*/

#include <c_decl.h>
#include "sys.h"

#define STRUCT_HEADER 2

/*------------------------------------------------------------------------------
 * RT::NEW-STRUCT size
 *----------------------------------------------------------------------------*/
LISP_FUN(rt_new_struct)
{
	long size = GET_FIXNUM(ARG(0));
   CL_FORM *structure;
   
   structure = form_alloc(ARG(1), size + STRUCT_HEADER);
   INIT_STRUCT(structure, size);
   LOAD_STRUCT(structure, ARG(0));
}
