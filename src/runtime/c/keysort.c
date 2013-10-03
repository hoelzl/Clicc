/*------------------------------------------------------------------------------
 * Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel
 *------------------------------------------------------------------------------
 * Projekt  : APPLY - A Practicable And Portable Lisp Implementation
 *            ------------------------------------------------------
 * Funktion : Laufzeitsystem
 *            Sortieren von Key-Parametern
 *            Ueberzaehlige Key-parameter in Abhaengigkeit von
 *            :ALLOW-OTHER-KEYS abweisen
 *
 * $Revision: 1.8 $
 * $Log: keysort.c,v $
 * Revision 1.8  1994/01/05  12:49:17  sma
 * Namensänderung: init_keysort mit dem Präfix rt_ versehen.
 * STACK(base, x) -> ARG(x).
 *
 * Revision 1.7  1993/06/16  14:43:22  hk
 * Copyright Notiz eingefuegt.
 *
 * Revision 1.6  1993/04/22  10:29:34  hk
 * fun_decl.h -> sys.h.
 *
 * Revision 1.5  1993/02/17  15:38:05  hk
 * CLICC -> APPLY, Revison Keyword.
 *
 * Revision 1.4  1992/09/28  17:20:28  hk
 * Lerror -> Labort, neues Lerror mit Lisp-Parameter
 *
 * Revision 1.3  1992/08/07  11:20:33  hk
 * Neue Funktion init_keysort zum Initialisieren von other_key_sym.
 *
 * Revision 1.2  1992/08/06  16:03:55  hk
 * Neue Variable other_key_sym, die Zeiger auf Symbol :allow-other-keys
 * enthaelt. Wird durch neue Funktion init_keysort initialisiert.
 *
 * Revision 1.1  1992/03/24  17:03:37  hk
 * Initial revision
 *----------------------------------------------------------------------------*/

#include <c_decl.h>
#include "sys.h"

char ODD_KEY_ARGS[] = "Odd number of keyword arguments";
char NO_OTHER_KEYS[] = "other Keys not allowed";

/*----------------------------------------------------------------------------*/
#define K_OTHER_KEY other_key_sym

/*------------------------------------------------------------------------------
 * Ein Zeiger auf das Symbol ':ALLOW-OTHER-KEYS
 * Es wird davon ausgegangen, dass die Adresse des Symbols konstant bleibt.
 *----------------------------------------------------------------------------*/
CL_FORM *other_key_sym;

/*------------------------------------------------------------------------------
 * Aufruf in startup mit (init-keysort ':ALLOW-OTHER-KEYS)
 *----------------------------------------------------------------------------*/
void rt_init_keysort (base)
CL_FORM *base;
{
   other_key_sym = GET_SYMBOL(ARG(0));
}

/*------------------------------------------------------------------------------
 * k: Keyword, v: Value, V: sortierte Werte, _: unbelegt (NIL), ?: undefiniert
 * nargs = 10, nkey=7
 *
 * k v k v k v k v k v ? ? ? ? ... 
 * ^                   ^
 * first_arg,          from
 *
 * --->
 *
 * _ _ _ _ _ _ _ k v k v k v k v k v ? ? ? ? ...
 *                                   ^
 *                                   to
 *
 * --->
 *
 * V V _ V _ V V k v k v k v k v k v ? ? ? ? ...
 *
 *----------------------------------------------------------------------------*/
void keysort (first_arg, nargs, nkey, keylist, suppl_flags, allow_other)
CL_FORM *first_arg;
int nargs;
int nkey;
CL_FORM *keylist[];
BOOL suppl_flags[];
BOOL allow_other;
{
   CL_FORM *from = first_arg + nargs;
   CL_FORM *to   = from + nkey;
   int i;
   BOOL other_keys = FALSE;
   
   /* Platz machen fuer 'nkey' Key-Werte */
   /* von rechts nach links kopieren */
   while(from > first_arg)
   {
      from--;
      to--;
      COPY(from, to);
   }

   /* Luecke mit NIL auffuellen, suppl_flags mit FALSE initialisieren */
   for(i = 0; i < nkey; i++)
   {
      /* Initialisierung notwendig, da lokales Array */
      suppl_flags[i] = FALSE;
      LOAD_NIL(from);
      from++;
   }

   /* pruefen, ob gerade Anzahl von Rest-Parametern */
   if(nargs & 1) Labort(ODD_KEY_ARGS);
   
   /* Key-Werte sortieren */
   
   from = to + nargs;

   /* Rest-Liste von rechts nach links nach Keywords durchsuchen, */
   /* damit bei doppelten Keywords der Wert des 1. Keywords genommen wird */
   while(from > to)
   {
      from -= 2;
      
      /* ueber keylist iterieren */
      for(i = 0; ; i++)
         if(i < nkey)
         /* ist es das naechste Keyword ? */
         {
            if(GET_FORM(from) == keylist[i] && CL_SYMBOLP(from))
            {
               /* Key gefunden, zugehoerigen Wert kopieren. */
               COPY(from + 1, first_arg + i);
               suppl_flags[i] = TRUE;
               break;
            }
         }
         else
         {
            /* ungueltiges Keyword gefunden */
            other_keys = TRUE;
            break;
         }
   }
   
   if(!allow_other && other_keys)
   {
         
      /* Rest-Liste nach :allow-other-key durchsuchen */
      /* von links nach rechts, damit der Wert des */
      /* 1. :allow-other-keys genommen wird */
      to = from + nargs;
      while(from < to)
      {
         if(GET_FORM(from) == K_OTHER_KEY && CL_SYMBOLP(from))
            if(CL_NILP (from + 1))
               /* :allow-other-keys NIL */
               Labort(NO_OTHER_KEYS);
            else
               /* Other-Keys erlaubt */
               return;
         from += 2;
      }
      Labort(NO_OTHER_KEYS);
   }
}

