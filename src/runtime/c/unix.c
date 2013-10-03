/*------------------------------------------------------------------------------
 * Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel
 *---------------------------------------------------------------------------
 * Projekt  : APPLY - A Practicable And Portable Lisp Implementation
 *            ------------------------------------------------------
 * Funktion : Laufzeitsystem
 *            - unix-current-directory
 *            - unix-lstat
 *            - unix-stat
 *            - unix-readlink
 *            - unix-get-unix-error-msg
 *
 * $Revision: 1.11 $
 * $Log: unix.c,v $
 * Revision 1.11  1994/05/26  10:59:33  uho
 * In unix_link_mode und unix_file_mode LOAD_SMALLFIXNUM in LOAD_FIXNUM
 * geaendert, da buf.st_mode kein Byte sein muss.
 *
 * Revision 1.10  1994/05/22  15:10:09  sma
 * LOAD_FIXNUM -> LOAD_SMALLFIXNUM um Compiler-Warnung abzuschaffen.
 *
 * Revision 1.9  1994/04/28  09:52:02  sma
 * LOAD_FIXNUM, LOAD_CHAR und LOAD_FLOAT um 3. Argument ergänzt.
 *
 * Revision 1.8  1994/01/22  18:33:39  sma
 * STACK(base, x) --> ARG(x) sowie kosmetische Korrekturen.
 *
 * Revision 1.7  1993/07/22  15:44:13  uho
 * Wie kam nur diese Klammer weg?
 *
 * Revision 1.6  1993/07/22  12:41:22  uho
 * Fuer GCC unter DOS, liefern 'unix_link_mod' und 'unix_read_link' fuer DOS
 * angemessene Ergebnisse.
 *
 * Revision 1.5  1993/07/06  11:37:17  sma
 * OFFSET-Makro eingeführt
 *
 * Revision 1.4  1993/06/16  14:43:22  hk
 * Copyright Notiz eingefuegt.
 *
 * Revision 1.3  1993/04/22  10:29:34  hk
 * fun_decl.h -> sys.h.
 *
 * Revision 1.2  1993/02/17  15:50:31  hk
 * CLICC -> APPLY, Revison Keyword.
 *
 * Revision 1.1  1993/01/19  14:39:14  hk
 * Initial revision
 *--------------------------------------------------------------------------*/

#include <c_decl.h>
#include "sys.h"
#include <sys/param.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>

#ifdef __STDC__
extern char *getwd(char pathname[]);
#else
extern char *getwd();
#endif

/*------------------------------------------------------------------------------
 * unix-current-directory
 * Returns 2 values
 *----------------------------------------------------------------------------*/
LISP_FUN(unix_current_directory)
{
   char pathname[MAXPATHLEN];
   char *result;
   
   result = getwd(pathname);
   make_string(ARG(0), pathname);
   COPY(ARG(0), OFFSET(mv_buf, 0));
   mv_count = 2;
   RET_BOOL(result != NULL);
}

/*------------------------------------------------------------------------------
 * unix-file-mode name
 *----------------------------------------------------------------------------*/
LISP_FUN(unix_file_mode)
{
   struct stat buf;
   int success = stat(get_c_string(ARG(0)), &buf);

   if(success == 0)
   {
      LOAD_FIXNUM(ARG(0), buf.st_mode, ARG(0));
   }
   else
   {
      LOAD_NIL(ARG(0));
   }
}

/*------------------------------------------------------------------------------
 * UNIX-LINK-MODE name
 *----------------------------------------------------------------------------*/
LISP_FUN(unix_link_mode)
{
   struct stat buf;
#ifdef MSDOS   
   int success = stat(get_c_string(ARG(0)), &buf);  /* No links under DOS */
#else
   int success = lstat(get_c_string(ARG(0)), &buf); 
#endif
   
   if (success == 0)
   {
      LOAD_FIXNUM(ARG(0), buf.st_mode, ARG(0));
   }
   else
   {
      LOAD_NIL(ARG(0));
   }
}

/*------------------------------------------------------------------------------
 * UNIX-READLINK name
 * Returns 1 or 2 values
 *----------------------------------------------------------------------------*/
LISP_FUN(unix_readlink)
{
   char buf[MAXPATHLEN];
#ifdef MSDOS
   int success = -1;            /* reading a symbolic link is always an error,
                                   because there are none */
   errno = EINVAL;              /* The named file is not a symbolic link. */
#else
   int success = readlink(get_c_string(ARG(0)), buf, sizeof(buf)-1);
#endif

   if(success >= 0)
   {
      buf[success] = '\0';
      make_string(ARG(0), buf);
   }
   else
   {
      LOAD_NIL(ARG(0));
      LOAD_SMALLFIXNUM(errno, OFFSET(mv_buf, 0));
      mv_count = 2;
   }
}
