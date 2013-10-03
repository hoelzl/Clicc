/*------------------------------------------------------------------------------
 * Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel
 *------------------------------------------------------------------------------
 * Projekt  : APPLY - A Practicable And Portable Lisp Implementation
 *            ------------------------------------------------------
 * Funktion : System-Funktionen: Characters
 *
 * $Revision: 1.17 $
 * $Log: character.c,v $
 * Revision 1.17  1994/05/22  15:50:05  sma
 * LOAD_FIXNUM -> LOAD_SMALLFIXNUM um Compiler-Warnung abzuschaffen.
 *
 * Revision 1.16  1994/04/28  09:43:25  sma
 * LOAD_FIXNUM, LOAD_CHAR und LOAD_FLOAT um 3. Argument ergänzt.
 *
 * Revision 1.15  1994/04/23  16:30:42  sma
 * RET_BOOL_OPT eingeführt. Dieses Makro lädt nicht explizit einen "true
 * value", wenn das Prädikat erfüllt ist.
 *
 * Revision 1.14  1994/02/01  14:13:36  uho
 * In rt_digit_char das fehlende 'X' in digitchars ergänzt.
 *
 * Revision 1.13  1994/01/14  09:20:41  sma
 * Character-Funktionen neu geschrieben. Mehr Lisp, weniger C. Alle
 * zeichensatzunabhängigen Funktionen befinden sich jetzt im LISP-Teil.
 *
 * Revision 1.12  1994/01/05  12:47:15  sma
 * Namensänderung: Alle Laufzeitsystemfunktionen mit dem Präfix rt_
 * versehen und den Postfix _internal entfernt. STACK(base,x) -> ARG(x)
 *
 * Revision 1.11  1993/08/27  11:48:44  sma
 * cproto-Warnungen wegen mehrzeiligem String beseitigt
 *
 * Revision 1.10  1993/08/26  15:31:07  sma
 * include <string.h> eingefuegt und char_table wieder entfernt.
 *
 * Revision 1.9  1993/08/24  16:20:43  sma
 * Variable char_table eingefügt und etwas aufgeräumt.
 *
 * Revision 1.8  1993/06/16  14:43:22  hk
 * Copyright Notiz eingefuegt.
 *
 * Revision 1.7  1993/04/22  10:29:34  hk
 * fun_decl.h -> sys.h.
 *
 * Revision 1.6  1993/02/17  15:29:25  hk
 * CLICC -> APPLY, Revison Keyword.
 *
 * Revision 1.5  1993/01/08  09:44:13  hk
 * Namen C_ nach c_.
 *
 * Revision 1.4  1992/11/22  13:58:16  kl
 * strncasecmp in our_strncasecmp umbenannt.
 *
 * Revision 1.3  1992/09/28  17:20:28  hk
 * Lerror -> Labort, neues Lerror mit Lisp-Parameter
 *
 * Revision 1.2  1992/07/21  13:49:59  hk
 * Fchareq --> FcharE, etc..
 *
 * Revision 1.1  1992/03/24  17:03:37  hk
 * Initial revision
 *
 * 13.01.92 : strncasecmp selbst kodiert
 *
 * 25.06.91 : strnicmp durch strncasecmp ersetzt
 *
 * 12.11.90 : Erste Version
 *----------------------------------------------------------------------------*/

#include <c_decl.h>
#include "sys.h"
#include <ctype.h>
#include <string.h>

/*------------------------------------------------------------------------------
 * Zeichenklassen-Tests
 *----------------------------------------------------------------------------*/

LISP_FUN(rt_standard_char_p)
{
   int ch = GET_CHAR(ARG(0));
   RET_BOOL_OPT(isprint(ch) || ch == '\n');
}

LISP_FUN(rt_graphic_char_p)
{
   RET_BOOL_OPT(isprint(GET_CHAR(ARG(0))));
}

LISP_FUN(rt_alpha_char_p)
{
   RET_BOOL_OPT(isalpha(GET_CHAR(ARG(0))));
}
   
LISP_FUN(rt_upper_case_p)
{
   RET_BOOL_OPT(isupper(GET_CHAR(ARG(0))));
}

LISP_FUN(rt_lower_case_p)
{
   RET_BOOL_OPT(islower(GET_CHAR(ARG(0))));
}

LISP_FUN(rt_both_case_p)
{
   int ch = GET_CHAR(ARG(0));
   /* Buchstabe, für den ein äquivalenter GROSS/kleinbuchstabe existiert */
   RET_BOOL_OPT(isalnum(ch) && (islower(ch) && toupper(ch) != ch
                                || isupper(ch) && tolower(ch) != ch));
}

/*------------------------------------------------------------------------------
 * RT:DIGIT-CHAR char
 *----------------------------------------------------------------------------*/
LISP_FUN(rt_digit_char)
{
   static char digitchars[] =
      "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
   int ch = GET_CHAR(ARG(0));

   if (isalnum(ch))
   {
      ch = (int)(strchr(digitchars, ch) - digitchars);
      if (ch >= 36) ch -= 26;
      LOAD_SMALLFIXNUM(ch, ARG(0));
   }
   else
      LOAD_NIL(ARG(0));
}

/*------------------------------------------------------------------------------
 * RT::CHAR-CODE char
 *----------------------------------------------------------------------------*/
LISP_FUN(rt_char_code)
{
    LOAD_SMALLFIXNUM((int)GET_CHAR(ARG(0)), ARG(0));
}

/*------------------------------------------------------------------------------
 * RT::CODE-CHAR code
 *----------------------------------------------------------------------------*/
LISP_FUN(rt_code_char)
{
    LOAD_CHAR(ARG(1), GET_FIXNUM(ARG(0)), ARG(0));
}

/*------------------------------------------------------------------------------
 * RT::CHAR-UPCASE charcode
 *----------------------------------------------------------------------------*/
LISP_FUN(rt_char_upcase)
{
   int ch = GET_CHAR(ARG(0));
   LOAD_CHAR(ARG(0), islower(ch) ? toupper(ch) : ch, ARG(0));
}

/*------------------------------------------------------------------------------
 * RT::CHAR-DOWNCASE charcode
 *----------------------------------------------------------------------------*/
LISP_FUN(rt_char_downcase)
{
   int ch = GET_CHAR(ARG(0));
   LOAD_CHAR(ARG(0), isupper(ch) ? tolower(ch) : ch, ARG(0));
}
   

