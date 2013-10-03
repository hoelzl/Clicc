/*------------------------------------------------------------------------------
 * Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel
 *---------------------------------------------------------------------------
 * Projekt  : APPLY - A Practicable And Portable Lisp Implementation
 *            ------------------------------------------------------
 * Funktion : Laufzeitsystem
 *            - c_fopen
 *            - c_fclose
 *            - c_ftell
 *            - c_fseek
 *            - c_file_length
 *            - c_stdin
 *            - c_stdout
 *            - c_fgetc
 *            - c_fputc
 *            - c_ungetc
 *
 * $Revision: 1.11 $
 * $Log: file.c,v $
 * Revision 1.11  1994/04/28  09:45:38  sma
 * LOAD_CFILE, LOAD_FIXNUM, LOAD_CHAR und LOAD_FLOAT um 3. Argument ergänzt.
 *
 * Revision 1.10  1994/01/13  16:41:13  sma
 * STACK(base, x) -> ARG(x). Eine Typcasts eingefügt, damit chars mit
 * code > 128 korrekt verarbeit werden. Quelltext verschönert.
 *
 * Revision 1.9  1993/10/13  16:54:29  sma
 * Da LOAD_CFILE zu einem Block expandieren kann, muß es bei if/then/else
 * in { } stehen.
 *
 * Revision 1.8  1993/06/16  14:43:22  hk
 * Copyright Notiz eingefuegt.
 *
 * Revision 1.7  1993/04/22  10:29:34  hk
 * fun_decl.h -> sys.h.
 *
 * Revision 1.6  1993/03/25  12:52:44  sma
 * neuer Look
 *
 * Revision 1.5  1993/02/17  15:36:34  hk
 * CLICC -> APPLY, Revison Keyword.
 *
 * Revision 1.4  1993/01/08  09:44:13  hk
 * Namen C_ nach c_.
 *
 * Revision 1.3  1993/01/06  16:29:51  hk
 * C-fopen vereinfacht, da nun Lisp-Strings als C-strings verwendet
 * werden koennen.
 *
 * Revision 1.2  1992/09/28  17:20:28  hk
 * Lerror -> Labort, neues Lerror mit Lisp-Parameter
 *
 * Revision 1.1  1992/03/24  17:03:37  hk
 * Initial revision
 *--------------------------------------------------------------------------*/

#include <c_decl.h>
#include "sys.h"


char NOT_UNREAD[] = "Could not unread character ~s";

/*------------------------------------------------------------------------------
 * C-OPEN name mode
 *----------------------------------------------------------------------------*/
LISP_FUN(c_fopen)
{
   FILE *f = fopen(get_c_string(ARG(0)), get_c_string(ARG(1)));

   if (f != NULL)
   {
      LOAD_CFILE(ARG(0), f, ARG(0));
   }
   else
      LOAD_NIL(ARG(0));
}

/*------------------------------------------------------------------------------
 * C-FCLOSE cfile
 *----------------------------------------------------------------------------*/
LISP_FUN(c_fclose)
{
   fclose(GET_CFILE(ARG(0)));
}

/*------------------------------------------------------------------------------
 * C-FTELL cfile
 *----------------------------------------------------------------------------*/
LISP_FUN(c_ftell)
{
   LOAD_FIXNUM(ARG(1), ftell(GET_CFILE(ARG(0))), ARG(0));
}

/*------------------------------------------------------------------------------
 * C-FSEEK cfile offset
 *----------------------------------------------------------------------------*/
LISP_FUN(c_fseek)
{
   fseek(GET_CFILE(ARG(0)), (long)GET_FIXNUM(ARG(1)), 0);
}

/*------------------------------------------------------------------------------
 * C-FILE-LENGTH cfile
 *----------------------------------------------------------------------------*/
LISP_FUN(c_file_length)
{
   FILE *f = GET_CFILE(ARG(0));
   long pos = ftell(f);
   long len;

   /* Dateiende */
   fseek(f, 0L, 2);
   len = ftell(f);
   fseek(f, pos, 0);
   LOAD_FIXNUM(ARG(0), len, ARG(0));
}

/*------------------------------------------------------------------------------
 * C-STDIN
 *----------------------------------------------------------------------------*/
LISP_FUN(c_stdin)
{
   LOAD_CFILE(ARG(0), stdin, ARG(0));
}

/*------------------------------------------------------------------------------
 * C-STDOUT
 *----------------------------------------------------------------------------*/
LISP_FUN(c_stdout)
{
   LOAD_CFILE(ARG(0), stdout, ARG(0));
}

/*------------------------------------------------------------------------------
 * C-FGETC cfile
 *----------------------------------------------------------------------------*/
#if 1
LISP_FUN(c_fgetc)
{
   int c = fgetc(GET_CFILE(ARG(0)));

   if (c == EOF)
      LOAD_NIL(ARG(0));
   else
   {
      LOAD_CHAR(ARG(0), c, ARG(0));
   }
}
#else
LISP_FUN(c_fgetc)
{
   static char buf[256];
   static char *ptr = buf;

   if (*ptr == '\0')
   {
      if (!fgets(buf, sizeof(buf), GET_CFILE(ARG(0))))
      {
         LOAD_NIL(ARG(0));
         buf[0] = '\0';
         ptr = buf;
         return;
      }
      else
      {
         ptr = buf;
      }
   }
   LOAD_CHAR(ARG(0), *ptr++, ARG(0));
}
#endif

/*------------------------------------------------------------------------------
 * C-FPUTC char cfile
 *----------------------------------------------------------------------------*/
LISP_FUN(c_fputc)
{
   fputc((int)GET_CHAR(ARG(0)), GET_CFILE(ARG(1)));
}

/*-------------------------------------------------------------------------
 * C-UNGETC char cfile
 *-------------------------------------------------------------------------*/
LISP_FUN(c_ungetc)
{
   if (ungetc((int)GET_CHAR(ARG(0)), GET_CFILE(ARG(1))) == EOF)
      Lerror(ARG(0), NOT_UNREAD);
}
