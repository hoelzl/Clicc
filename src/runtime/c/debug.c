/*------------------------------------------------------------------------------
 * Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel
 *------------------------------------------------------------------------------
 * Projekt  : APPLY - A Practicable And Portable Lisp Implementation
 *            ------------------------------------------------------
 * Funktion : Print Funktion fuer CL_FORMs
 *
 * $Revision: 1.24 $
 * $Log: debug.c,v $
 * Revision 1.24  1994/05/31  15:17:06  sma
 * UNBOUND wieder als eigenen Tzp eingefuehrt.
 *
 * Revision 1.23  1994/05/18  15:18:12  sma
 * Anpassung für obrep2. Abstraktionen verbessert. (Außerdem Quelltext
 * teilweise neu formatiert)
 *
 * Revision 1.22  1994/05/06  08:26:24  hk
 * Structure-Printer angepa"st: size enth"alt die Anzahl der Slots, also
 * size+1
 *
 * Revision 1.21  1994/01/21  13:34:49  sma
 * Erneute Änderung der Symbolrepräsentation. Änderug für neue
 * Repräsentation von #<unbound>.
 *
 * Revision 1.20  1994/01/14  09:18:18  sma
 * Änderung der Symbol-Repräsentation auch für dc_pure_symbol.
 *
 * Revision 1.19  1994/01/13  16:36:51  sma
 * Änderung der Symbol-Repräsentation.
 *
 * Revision 1.18  1993/12/10  15:14:48  sma
 * in main.c wird stack_cont(stack, 0, 0) aufgerufen, was bis eben als
 * ungültiges Argument angesehen wurde. Ist korrigiert.
 *
 * Revision 1.17  1993/12/10  11:35:39  sma
 * extern-Declaration in stack_cont vergessen.
 *
 * Revision 1.16  1993/12/09  15:00:29  sma
 * Änderungen für neue array-Repräsentation. Code für jetzt unbenutzte Typen wie
 * CL_SMAR_FIXNUM entfernt. Source neu eingerückt. Check für stack_cont
 * eingebaut, der die "beliebten" BUS-Errors beim Debugen unterdrückt,
 * die entstehen, weil man stack_cont falsch aufruft.
 *
 * Revision 1.15  1993/10/27  12:23:59  sma
 * Auf die Länge von Strukturen und Instanzen wird jetzt (korrekterweise)
 * mit dem AR_SIZE-Makro zugegriffen.
 *
 * Revision 1.14  1993/09/19  18:02:32  sma
 * inspect um RT_*-Typen erweitert.
 * show_alist() zeigt formatiert eine a-liste an.
 *
 * Revision 1.13  1993/08/26  16:15:16  hk
 * Noch einen Cast für printf eingefügt.
 *
 * Revision 1.12  1993/08/26  15:51:26  hk
 * Einige Casts für Argumente vob printf eingefügt.
 *
 * Revision 1.11  1993/06/16  14:43:22  hk
 * Copyright Notiz eingefuegt.
 *
 * Revision 1.10  1993/06/09  12:46:16  hk
 * dc_area geloescht, symbol_module_i neu.
 *
 * Revision 1.9  1993/04/22  10:22:16  hk
 * fun_decl.h -> sys.h, Funktion sym_table gestrichen.
 *
 * Revision 1.8  1993/04/19  07:56:43  ft
 * Pruefung auf NIL bei Typ 50 geaendert.
 *
 * Revision 1.7  1993/03/24  14:43:08  sma
 * schoener formatiert (fuer ft)
 *
 * Revision 1.6  1993/02/17  15:33:52  hk
 * CLICC -> APPLY, Revison Keyword.
 *
 * Revision 1.5  1992/12/11  09:24:22  ft
 * Erweiterung von inspect um case's fuer Instanzen bzw. Klassen.
 *
 * Revision 1.4  1992/10/02  15:00:36  uho
 * Extern-Deklaration von fo_heap entfernt, da schon in fun_decl.h
 * enthalten.
 *
 * Revision 1.3  1992/08/04  16:48:38  hk
 * Schreibfehler.
 *
 * Revision 1.2  1992/08/04  16:41:45  hk
 * Weitere Print-Methoden fuer Lisp-Objekte eingefuegt.
 *
 * Revision 1.1  1992/03/24  17:03:37  hk
 * Initial revision
 *----------------------------------------------------------------------------*/

#include <c_decl.h>
#include "sys.h"

/*----------------------------------------------------------------------------*/
int print_length = 10;
int print_level = 2;

/*------------------------------------------------------------------------------
 * Prozedur    : INSPECT
 * Beschreibung: Zeigt eine LISP-Form an
 *----------------------------------------------------------------------------*/
void inspect(base, level, length, in_list_p)
CL_FORM *base;
int level, length;
BOOL in_list_p;
{
   CL_FORM *fptr;
   long i, size;
   
   switch (TYPE_OF(base))
   {
	case CL_FIXNUM:              /*  1 */
      printf("%ld", GET_FIXNUM(base));
      return;
	case CL_FLOAT:               /*  2 */
      printf("%g[F]", GET_FLOAT(base));
      return;
	case CL_CHAR:                /*  3 */
      switch(GET_CHAR(base))
      {
		case '\b':
         printf("\\b");
         break;
		case '\f':
         printf("\\f");
         break;
		case '\n':
         printf("\\n");
         break;
		case '\r':
         printf("\\r");
         break;
		case '\t':
         printf("\\t");
         break;
		default:
         printf("\'%c\'", GET_CHAR(base));
      }
      return;
   case CL_UNBOUND:
      printf("#<unbound>");
      return;
	case CL_SYMBOL:              /* 13 */
      fptr = SYM_NAME(base);
      printf("%.*s", (int)AR_SIZE(fptr), CHAR_AR(fptr));
      return;
	case CL_NIL:                 /* 14 */
      printf("NIL");
      return;
	case CL_CLOSURE:             /* 32 */
      printf("#<Closure>");
      return;
	case CL_DOWNFUN:             /* 33 */
      printf("#<Downfun>");
      return;
	case CL_GLOBFUN:             /* 34 */
      printf("#<Global function>");
      return;
	case CL_IND:                 /* 38 */
      printf("Indirection: ");
      inspect(INDIRECT(base), level, length, FALSE);
      return;
	case CL_CFILE:               /* 41 */
      printf("#<C File>");
      return;
	case CL_UNIQUE_TAG:          /* 42 */
      printf("#<Tag %ld>", AR_SIZE(base));
      return;
   }

   if (level > print_level)     /* *PRINT-LEVEL* ueberschritten ? */
   {
      printf("#");
      return;
   }

   fptr = GET_FORM(base);
   switch (TYPE_OF(base))
   {
      /*----------------------------------------------------------------------*/
	case CL_CONS:                /* 15 */
      if (!in_list_p)
         printf("(");
      inspect(CAR(fptr), level + 1, 0, FALSE);
      fptr = CDR(fptr);
      switch (TYPE_OF(fptr))
      {
		case CL_NIL:
         printf(")");
         break;
		case CL_CONS:
         printf(" ");
         if (length > print_length)
         {
            printf("... )");
            return;
         }
         inspect(fptr, level, length + 1, TRUE);
         break;
		default:
         printf(" . ");
         inspect(fptr, level + 1, 0, FALSE);
         printf(")");
         break;
      }
      break;

      /*----------------------------------------------------------------------*/
	case CL_SMVEC_T:             /* 20 */
      size = AR_SIZE(fptr);
      printf("#1A%ld(", size);
      for (i = 0; i < size; i++)
      {
         inspect(OFFSET(FORM_AR(fptr), i), level + 1, 0, FALSE);
         if (i < size - 1)
            printf(" ");
         if (i > print_length)
         {
            printf("...");
            break;
         }
      }
      printf(")");
      break;

      /*----------------------------------------------------------------------*/
	case CL_SMVEC_FIXNUM:        /* 21 */
   {
      long *fixnum_ar = FIXNUM_AR(fptr);

      size = AR_SIZE(fptr);
      printf("#1AFi%ld(", size);
      for(i = 0; i < size; i++)
      {
         printf("%ld", fixnum_ar[i]);
         if (i < size - 1)
            printf(" ");
         if (i > print_length)
         {
            printf("...");
            break;
         }
      }
      printf(")");
      break;
   }

      /*----------------------------------------------------------------------*/
	case CL_SMVEC_FLOAT:         /* 22 */
   {
		double *fl_ptr = FLOAT_AR(fptr);

		size = AR_SIZE(fptr);
		printf("#1AFl%ld(", size);
		for (i = 0; i < size; i++, fl_ptr++)
		{
         printf("%g", *fl_ptr);
         if (i < size - 1)
            printf(" ");
         if (i > print_length)
         {
            printf("...");
            break;
         }
		}
		printf(")");
		break;
   }
      /*----------------------------------------------------------------------*/
	case CL_SMSTR:               /* 23 */
      printf("\"%.*s\"", (int)AR_SIZE(fptr), AR_STRING(fptr));
      break;
      
      /*----------------------------------------------------------------------*/
	case CL_STRUCT:              /* 39 */
   {
		size = 1 + AR_SIZE(fptr);
		printf("#S(");
		for (i = 0; i < size; i++)
		{
         inspect(fptr + 1 + i, level + 1, 0, FALSE);
         if (i < size - 1)
            printf(" ");
         if (i > print_length)
         {
            printf("...");
            break;
         }
		}
		printf(")");
		break;
   }

      /*----------------------------------------------------------------------*/
	case CL_INSTANCE:            /* 50 */
   {
		size = AR_SIZE(fptr);
		if (CL_NILP(AR_BASE(fptr)))
         printf("#<CLASS ");
		else
		{
         printf("#<INSTANCE-OF-");
         inspect(AR_BASE(fptr), level + 1, 0, FALSE);
         printf(" ");
		}
		for (i = 1; i < size; i++)
		{
         inspect(fptr + 1 + i, level + 1, 0, FALSE);
         if (i < size - 1)
            printf(" ");
         if (i > print_length)
         {
            printf("...");
            break;
         }
		}
		printf(">");
		break;
   }

      /*----------------------------------------------------------------------*/
#ifdef RT_CHAR_PTR
   case RT_CHAR_PTR:
      printf("<char_ptr>\"%s\"", GET_CHAR_PTR(base));
      break;
   case RT_FLOAT_PTR:
      printf("<float_ptr %lx>", (long)GET_FLOAT_PTR(base));
      break;
   case RT_FIXNUM_PTR:
      printf("<fixnum_ptr %lx>", (long)GET_FIXNUM_PTR(base));
      break;
   case RT_FORM_PTR:
      printf("<form_ptr %lx>", (long)GET_FORM(base));
      break;
#endif
	default:
      printf("<Unknown object %d>", TYPE_OF(base));
   }
}

/*------------------------------------------------------------------------------
 * Zeigt aus dem Speicherbereich ab 'base + offset' 'nargs' viele Eintraege an
 *----------------------------------------------------------------------------*/
void stack_cont (base, offset, nargs)
CL_FORM *base;
int offset, nargs;
{
   int i;

   extern CL_FORM *fo_heap;     /* aus system.c */
   extern unsigned form_heapsize;

   /* Argumente auf Plausibilität prüfen, um BUS-Errors einzuschränken
    * Die Zahlen sind so gewählt, daß der häufigste Fall, bei
    * stack_cont(&Smain[100],0,1) das & zu vergessen, wahrscheinlich 
    * abgefangen wird. */
   if (offset < 0 || nargs < 0 || offset > 10000 || nargs > 1000 ||
       (long)base < 100 || base > fo_heap + 2 * form_heapsize)
   {
      printf("\n*** Ungueltige Argumente\n");
      return;
   }

   for (i = offset; i < offset + nargs; i++)
   {
      printf ("%2d: ", i);
      inspect (base + i, 0, 0, FALSE);
      printf ("\n");
   }
}

/*------------------------------------------------------------------------------
 * Zeigt den Namen, die Property-Liste und den Wert des Symbols an einer
 * gegebenen Adresse
 *----------------------------------------------------------------------------*/
void dc_pure_symbol (symbol)
CL_FORM *symbol;
{
   printf ("Symbol (0x%lx): %.*s\n",
           (long)symbol,
           (int)AR_SIZE(symbol + OFF_SYM_NAME),
           AR_STRING(symbol + OFF_SYM_NAME));
   printf ("Plist: ");
   inspect (symbol + OFF_SYM_PLIST, 0, 0, FALSE);
   printf ("\nValue: ");
   inspect (symbol + OFF_SYM_VALUE, 0, 0, FALSE);
   printf ("\n");
}

/*------------------------------------------------------------------------------
 * Zugriff auf das i-te Symbol im Symbol-Array eines Moduls
 *----------------------------------------------------------------------------*/
void symbol_module_i (sym_base, i)
CL_FORM *sym_base;
int i;
{
   dc_pure_symbol(SYMBOL(sym_base, i));
}

/*------------------------------------------------------------------------------
 * Zeigt eine Alist formatiert an.
 *----------------------------------------------------------------------------*/
void show_alist (base, i)
CL_FORM *base;
int i;
{
   printf("(");
   while (!CL_NILP(base))
   {
      if (!CL_CONSP(base))
      {
         printf("<error, ");
         inspect(base, 0, 0, FALSE);
         printf(" instead of cons>");
         break;
      }
      inspect(GET_CAR(base), 0, 0, FALSE);
      printf("\n ");
      base = GET_CDR(base);
   }
   printf(")\n");
}
