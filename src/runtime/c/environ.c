/*------------------------------------------------------------------------------
 * Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel
 *------------------------------------------------------------------------------
 * Projekt  : APPLY - A Practicable And Portable Lisp Implementation
 *            ------------------------------------------------------
 * Funktion : Laufzeitsystem: Zugriff auf System Resourcen
 *            - Environment Variablen
 *            - Komandozeilen Argumente
 *
 * $Revision: 1.13 $
 * $Log: environ.c,v $
 * Revision 1.13  1994/04/28  09:44:34  sma
 * LOAD_FIXNUM, LOAD_CHAR und LOAD_FLOAT um 3. Argument ergänzt.
 *
 * Revision 1.12  1993/12/09  15:05:01  sma
 * Aufgrund der neuen Array- (und String-)Repräsentation müssen c-system
 * und c-environ in Lisp kodiert werden, die ggf. Strings in
 * simple-strings konvertieren und dann die neuen Funktionen
 * c-system-internal bzw c-environ-internal aufgrufen.  STACK(base, xxx)
 * -> ARG(xxx)
 *
 * Revision 1.11  1993/11/04  12:56:30  uho
 * Funktion  c_system  zum Aufruf externer Programme definiert.
 *
 * Revision 1.10  1993/06/16  14:43:22  hk
 * Copyright Notiz eingefuegt.
 *
 * Revision 1.9  1993/04/22  10:29:34  hk
 * fun_decl.h -> sys.h.
 *
 * Revision 1.8  1993/02/17  15:35:37  hk
 * CLICC -> APPLY, Revison Keyword.
 *
 * Revision 1.7  1993/01/08  09:44:13  hk
 * Namen C_ nach c_.
 *
 * Revision 1.6  1993/01/07  12:50:55  hk
 * In c_argv wieder tmp_buf definiert.
 *
 * Revision 1.5  1993/01/06  16:17:50  hk
 * Nicht benutzte Fehlermeldung gestrichen.
 *
 * Revision 1.4  1993/01/06  16:08:25  hk
 * tmp_buf gestrichen.
 *
 * Revision 1.3  1993/01/06  16:07:23  hk
 * Beim Aufruf von getenv wird der Lisp-String direkt als C-String
 * verwendet, da alle Lisp-Character-Arrays nun durch ein 0 Character
 * beendet sind.
 *
 * Revision 1.2  1992/09/28  17:20:28  hk
 * Lerror -> Labort, neues Lerror mit Lisp-Parameter
 *
 * Revision 1.1  1992/03/24  17:03:37  hk
 * Initial revision
 *----------------------------------------------------------------------------*/

#include <c_decl.h>
#include "sys.h"
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

extern int glob_argc;         /* in main.c */
extern char **glob_argv;      /* in main.c */

char ARG_FILE_NOT_FOUND[]= "File ~a not found for argument expansion";
char ARG_FILE_ERR[]= "in file ~a while expanding argument";
char ARG_STRING_TOO_LONG[]= "Argument string too long: ~a";

/*------------------------------------------------------------------------------
 * c-getenv-internal string
 *----------------------------------------------------------------------------*/
void c_getenv_internal(base)
CL_FORM *base;
{
   char *str = getenv(get_c_string(ARG(0)));

   if(str == NULL)
   {
      LOAD_NIL(ARG(0));
   }
   else
   {
      make_string(ARG(0), str);
   }
}

/*------------------------------------------------------------------------------
 * c-system_internal string
 *----------------------------------------------------------------------------*/
void c_system_internal(base)
CL_FORM *base;
{	
   LOAD_FIXNUM(ARG(0), system(get_c_string(ARG(0))), ARG(0));
}

/*------------------------------------------------------------------------------
 * c-argc
 *----------------------------------------------------------------------------*/
void c_argc(base)
CL_FORM *base;
{
   LOAD_FIXNUM(ARG(0), glob_argc, ARG(0));
}

/*------------------------------------------------------------------------------
 * c-argv
 *----------------------------------------------------------------------------*/
void c_argv(base)
CL_FORM *base;
{
   char tmp_buf[256];
   int arg = 0;
   int stk = 0;
   
   while(arg < glob_argc)
   {
      /* expand argument "@filename" to contents of file */
      /* !!! Dokumentieren ! */
      /* ----------------------------------------------- */
      if('@' == glob_argv[arg][0])
      {
         FILE *f;

         f = fopen(&glob_argv[arg][1], "r");
         if(!f)
         {
            make_string(ARG(stk), &glob_argv[arg][1]);
            Lerror(ARG(stk), ARG_FILE_NOT_FOUND);
         }
         else
         {
            int j;

            j = 0;
            do
            {
               tmp_buf[j] = getc(f);
               if(isspace(tmp_buf[j]) || EOF == tmp_buf[j])
               {
                  if(ferror (f))
                  {
                     make_string(ARG(stk), &glob_argv[arg][1]);
                     Lerror(ARG(stk), ARG_FILE_ERR);
                  }
                  if(j > 0)
                  {
                     tmp_buf[j] = 0;
                     make_string(ARG(stk), tmp_buf);
                     stk++;
                  }
                  j = 0;
               }
               else
               {
                  j++;
                  if(j >= sizeof(tmp_buf))
                  {
                     tmp_buf[--j]='\0';
                     make_string(ARG(stk), tmp_buf);
                     Lerror(ARG(stk), ARG_STRING_TOO_LONG);
                  }
               }                  
            }
            while(!feof(f));
            arg++;
         }
      }
      else
      {
         make_string(ARG(stk), glob_argv[arg]);
         stk++;
         arg++;
      }
   }
   Flist(ARG(0), stk);
   glob_argc = stk;
}
