/*------------------------------------------------------------------------------
 * Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel
 *------------------------------------------------------------------------------
 * Projekt  : APPLY - A Practicable And Portable Lisp Implementation
 *            ------------------------------------------------------
 * Funktion : Deklarationen fuer das Laufzeitsystem
 * 
 * $Revision: 1.40 $
 * $Log: c_decl.h,v $
 * Revision 1.40  1994/05/18  15:10:42  sma
 * Datenstrukturen global_funarg und down_funarg nach obrepX verschoben.
 *
 * Revision 1.39  1994/04/28  09:42:12  sma
 * COPY und LISP_FUN Makros von obrep1 nach c_decl.h verschoben,
 * RET_BOOL_OPT eingeführt.
 *
 * Revision 1.38  1994/04/18  11:38:42  pm
 * Foreign Function Interface voellig ueberarbeitet.
 * - Weggefallene Macros entfernt
 *
 * Revision 1.37  1994/02/18  12:03:50  uho
 * Die Abfragen im Garbage-Collector auf konstante Datenobjekte und die
 * Heap-Konsistenz als Makros definiert. Bei Uebersetzung mit definitiertem
 * Preprozessorsymbol SHARED_LIBRARY werden alle Daten ausserhalb des
 * Heaps als konstant angesehen.
 *
 * Revision 1.36  1994/02/01  14:16:20  uho
 * 'extern'-Referenz auf 'Ssys' entfernt.
 *
 * Revision 1.35  1994/01/21  13:14:54  sma
 * Neues Makro LOAD_BOOL(expr, loc) welches das Lisp-äquivalent eines C
 * Wahrheitswerts nach "loc" schreibt. RET_BOOL basiert jetzt auf dieser
 * allgemeineren Form. Wird in cginline benutzt.
 *
 * Revision 1.34  1993/12/16  16:25:43  pm
 * Macros fuer den umgang mit dem FFI eingefuegt.
 *
 * Revision 1.33  1993/12/09  13:30:02  sma
 * Neues Makro ARG(xxxx) als Ersatz für STACK(base, xxxx)
 *
 * Revision 1.32  1993/11/22  09:24:02  hk
 * Neuer C-Code ONLY_ONCE in Initialisierungsfunktionen, der bewirkt,
 * da_ diese Funktionen hvchstens 1x ausgef|hrt werden.
 *
 * Revision 1.31  1993/10/14  16:05:11  sma
 * __OBREP wird nur == 1 definiert, wenn es nicht schon über die
 * Kommandozeile des Compilers gesetzt wurde.
 * CL_FORM wird erst innerhalb von obrepX.h definiert.
 * Die Definition von CONTENV, etc wird bis hinter die Einbindung von
 * obrepX.h verzögert.
 *
 * Revision 1.30  1993/09/09  09:55:29  uho
 * Zwei neue Macros definiert, deren Expansion bisher im Codegenerator
 * vorgenommen wurde: MEM_UP_MOVE und MV_TO_STACK.
 *
 * Revision 1.29  1993/09/07  16:10:24  sma
 * obrep?.h enthält objektrepäsentationsspezigische Definitionen
 *
 * Revision 1.28  1993/08/10  11:44:51  ft
 * Makro zum Pruefen auf den Typ CL_INSTANCE eingefuegt.
 *
 * Revision 1.27  1993/08/10  11:13:21  pm
 * Erweiterungen des FFI um Pointer
 *
 * Revision 1.26  1993/07/22  08:54:43  pm
 * LOAD- und GET-Funktionen für C-Strukturen
 *
 * Revision 1.25  1993/07/05  16:03:21  sma
 * Nur eine Zeile und trotzdem ein Fehler...
 *
 * Revision 1.24  1993/07/05  14:34:20  sma
 * OFFSET-Makro eingefuegt, welches anstelle des STACK-Makros benutzt werden
 * soll, wenn dieses nicht mit dem LISP-Stack zusammen eingesetzt wird.
 *
 * Revision 1.23  1993/07/05  12:30:31  hk
 * stack deklariert.
 *
 * Revision 1.22  1993/07/05  12:23:45  hk
 * ILLEGAL_ARGS deklariert.
 *
 * Revision 1.21  1993/06/30  17:10:51  hk
 * Deklaration fuer Ssys eingefuegt.
 *
 * Revision 1.20  1993/06/17  08:59:19  hk
 * Copyright Notiz eingefuegt.
 *
 * Revision 1.19  1993/06/14  16:58:42  hk
 * Deklaration fuer Variable Symbols herausgenommen, Kommentare neu formatiert.
 *
 * Revision 1.18  1993/06/04  18:03:56  wg
 * Tippfehler bei LOAD_C_... eliminiert.
 *
 * Revision 1.17  1993/06/04  13:41:43  pm
 * Globale Variable save_stack deklariert
 *
 * Revision 1.16  1993/05/31  16:57:03  pm
 * Fehler beseitigt
 *
 * Revision 1.15  1993/05/23  17:42:34  pm
 * alle primitiven C-Typen eingebaut
 * LOAD- und GET-Macros dafuer geschrieben
 *
 * Revision 1.14  1993/05/13  13:31:44  pm
 * GET_FOREIGN_INT eingebaut
 *
 * Revision 1.13  1993/05/03  12:26:42  pm
 * Erweiterung um die Tags fuer das FFI
 *
 * Revision 1.12  1993/04/22  10:31:09  hk
 * NIL_VALUE hat Wert 0,
 * SYMBOL und SYMVAL bekommen zusaetzliches Argument base,
 * einen Zeiger auf das Symbol-Array eines Moduls.
 *
 * Revision 1.11  1993/03/25  10:21:53  ft
 * CLASS_SIZE auf 6 erhoeht.
 *
 * Revision 1.10  1993/03/12  10:42:16  ft
 * Deklarationen zur Codegenerierung fuer Klassen.
 *
 * Revision 1.9  1993/02/17  16:21:49  hk
 * CLICC -> APPLY, Revison Keyword.
 *
 * Revision 1.8  1992/12/07  16:53:53  ft
 * Erweiterung um CL_INSTANCE.
 *
 * Revision 1.7  1992/11/26  11:43:48  pm
 * GET_STRING und GET_STREAM rausgeworfen (Relikte)
 *
 * Revision 1.6  1992/09/09  10:03:02  hk
 * Vorkommen von CL_PACKAGE entfernt, da nicht definiert.
 *
 * Revision 1.5  1992/09/08  15:12:26  hk
 * In LOAD_GLOBFUN wird nun GET_GFARG statt faelschlich GET_FORM benutzt.
 *
 * Revision 1.4  1992/09/08  13:46:17  hk
 * Extern Decl. fuer mv_args entfernt, da nicht benutzt.
 *
 * Revision 1.3  1992/04/15  08:59:26  hk
 * Schreibfehler korrigiert.
 *
 * Revision 1.2  1992/04/14  14:29:26  hk
 * CATCHENV in CONTENV und last_catch in last_cont umbenannt.
 *
 * Revision 1.1  1992/03/24  17:20:39  hk
 * Initial revision
 *
 *               1991/01/16            hk
 * BOOL wird durch char ersetzt, damit cproto es richtig erkennt
------------------------------------------------------------------------------*/

#include <stdio.h>
#include <setjmp.h>

/*------------------------------------------------------------------------------
 * Konfiguration
 *----------------------------------------------------------------------------*/
#ifndef __OBREP
#define __OBREP  1              /* Art der Datenrepräsentation */
#endif
#define __INT_GE_PTR            /* sizeof(int) >= sizeof(void *) */

#ifdef __STDC__                 /* PTR ist allgemeiner Zeiger */
typedef void *PTR;
#else
typedef char *PTR;
#endif


/*------------------------------------------------------------------------------
 * Typdefinitionen
 *----------------------------------------------------------------------------*/
#define NOT(x)  (!(x)) /* zur besseren Lesbarkeit des erzeugten Codes */
#define BOOL  char
#ifndef FALSE
#define FALSE  0
#define TRUE  1
#endif


/*------------------------------------------------------------------------------
 * Vorwärts-Deklaration
 *----------------------------------------------------------------------------*/
typedef struct global_funarg GLOBAL_FUNARG;
typedef struct down_funarg DOWN_FUNARG;
typedef struct contenv CONTENV;


/*------------------------------------------------------------------------------
 * Typen der von CLICC erzeugten C-Funktionen
 *----------------------------------------------------------------------------*/
typedef void GLOBAL_FUN  (/*CL_FORM *base, ...*/);
typedef void LOCAL_FUN   (/*CL_FORM *base, CL_FORM **display, ...*/);
typedef void CLOSURE_FUN (/*CL_FORM *base, ...*/);


/*------------------------------------------------------------------------------
 * Alle Datenrepräsentationsabhängigen Definitionen einbinden...
 *----------------------------------------------------------------------------*/
#if __OBREP == 1
#include "obrep1.h"
#elif __OBREP == 2
#include "obrep2.h"
#else
/* Dies sollte einen Laufzeitfehler erzeugen :-) */
#include <error: __OBREP isn't defined or hold's an unsupported value>
#endif


/*------------------------------------------------------------------------------
 * Datenstruktur für Continuations
 *----------------------------------------------------------------------------*/
struct contenv
{
   struct contenv *last;
   CL_FORM *bind_top;
   jmp_buf jmp_buf;
};


/*------------------------------------------------------------------------------
 * Makro für den Zugriff auf den Stackframe einer LISP-Funktion
 *----------------------------------------------------------------------------*/
#define STACK(base, offset) ((base) + (offset))

/*------------------------------------------------------------------------------
 * Makro speziell für den Zugriff auf den LISP-Stack definiert durch "base"
 *----------------------------------------------------------------------------*/
#define ARG(offset) ((base) + (offset))

/*------------------------------------------------------------------------------
 * Makro für den Zugriff auf ein Element eines Vektors von CL_FORMs
 *----------------------------------------------------------------------------*/
#define OFFSET(vector, offset) ((vector) + (offset))


/*------------------------------------------------------------------------------
 * Makro für das Kopieren der CL_FORM-Struktur
 * Bei einigen Kompilern muss Komponentenweise kopiert werden.
 *----------------------------------------------------------------------------*/
#define COPY(source, dest) (*(dest) = *(source))

/* Falls komponentenweise kopiert werden muss, dann unbedingt den Typ
 * zuerst kopieren! Denn sonst wurden Aufrufe der Form
 * COPY(SYM_VALUE(ARG(1)), ARG(1))
 * nicht korrekt uebersetzt werden.
 *    #define COPY(source, dest)\
 *       (dest->tag = source->tag, dest->val = source->val)
 */


#ifdef __STDC__
#define LISP_FUN(name) void name(CL_FORM *base)
#define LISP_FUN_NARGS(name) void name(CL_FORM *base, int nargs)
#else
#define LISP_FUN(name) void name(base) CL_FORM *base;
#define LISP_FUN_NARGS(name) void name(base, nargs) CL_FORM *base; int nargs;
#endif


/*------------------------------------------------------------------------------
 * Retten/Restaurieren von SPECIAL Variablen.
 * Retten: 1. Wert auf Binding-Stack
 *         2. Symbol auf Binding-Stack.
 * Restaurieren: alten Wert in die Wert-Zelle des Symbols kopieren. 
 * 'sym' ist Zeiger auf das Symbol direkt, NICHT Zeiger auf eine CL_FORM
 * mit Typ CL_SYMBOL.
 *----------------------------------------------------------------------------*/
#define POP_SPECIAL bind_top -= 2
#define SAVE_SPECIAL(sym)  (COPY(OFF_SYM_VALUE + sym, bind_top), \
                            bind_top++, \
                            LOAD_SYMBOL(sym, bind_top), \
                            bind_top++)
#define BIND_SPECIAL(sym, new_val)  (SAVE_SPECIAL(sym), \
                                     COPY(new_val, OFF_SYM_VALUE + sym))
#define RESTORE_SPECIAL  (POP_SPECIAL, \
                          COPY(bind_top, SYM_VALUE(bind_top+1)))


/*-------------------------------------------------------------------------
 * Verschiebt in einem Array von CL_FORMs den Bereich [SrcLow .. SrcHigh[ um
 * MoveDiff CL_FORMs in Richtung hoeherer Adressen. Quelle und Ziel koennen
 * sich uebelappen.
 *-------------------------------------------------------------------------*/
#define MEM_UP_MOVE(SrcHigh, SrcLow, MoveDiff) \
{  CL_FORM *from = (SrcHigh),\
           *to   = from + (MoveDiff);\
      while(from > (SrcLow)) { \
          from--, to--; \
          COPY(from, to); \
      } \
} 


/*-------------------------------------------------------------------------
 * Kopiert N-1 CL_FORMs beginned bei Offset im aktuellen Activation Record in
 * den Multiple-Value Buffer.
 *-------------------------------------------------------------------------*/
#define MV_TO_STACK(n, offset) \
{ int i; \
  for (i=0; i<(n)-1; i++) { \
     COPY(&mv_buf[i], ARG(i+(offset)));\
  } \
}

/*------------------------------------------------------------------------------
 * Ein Befehl, der bei der ersten Ausfuehrung keine sichtbaren Effekt
 * hat und bei nachfolgenden Ausfuehrungen die Ausfuehrung der
 * aktuellen Funktion abbricht.
 *----------------------------------------------------------------------------*/
#define ONLY_ONCE \
{static BOOL was_here = FALSE; if(was_here) return; else was_here = TRUE;} 
  
/*------------------------------------------------------------------------------
 * eigenes setjmp/longjmp definieren, damit auch auf Maschinen, bei denen
 * sizeof(char*) > sizeof(int) gilt, Zeiger als Parameter von longjmp 
 * angegeben werden können.
 *----------------------------------------------------------------------------*/
#ifdef __INT_GE_PTR
#define SETJMP(buf)  setjmp(buf)
#define LONGJMP(buf, value)  longjmp(buf, (int)(value))
#else
 extern char *jmp_value;
#define SETJMP(buf)  (setjmp(buf) ? jmp_value : NULL)
#define LONGJMP(buf, value)  (jmp_value = (char *)(value), longjmp(buf, 1))
#endif

/*------------------------------------------------------------------------------
 * Konvertiert C-bool in LISP-Äquivalent
 *----------------------------------------------------------------------------*/
#define LOAD_BOOL(expr, loc)  if (expr) LOAD_T(loc); else LOAD_NIL(loc)

#define RET_BOOL(expr)  LOAD_BOOL(expr, ARG(0))

#define RET_BOOL_OPT(expr)  if (!(expr)) LOAD_NIL(ARG(0))

/*------------------------------------------------------------------------------
 * Speicherverwaltung: Konsistenzpruefung und Test auf Konstanten
 *----------------------------------------------------------------------------*/
#ifdef SHARED_LIBRARY

/* Keine Annahme, Konstanten wuerden in Adressen unterhalb des Heaps
   abgelegt sein. Jede Adress ausserhalb des Heaps wird als Adresse
   eines Konstanten Datums angesehen. */

/* ---------------------- Float Heap ---------------------------------------- */
#define FL_CONSTANTq(fptr)  ((fptr) < fl_heap1 || \
                             (fptr) >= fl_heap1 + 2 * fl_heapsize)
#define FL_OUT_OF_HEAPq(fptr) (0)
#define FL_WRONG_HEAPq(fptr) ((fptr) < old_fl_heap)

/* ---------------------- Form Heap  ---------------------------------------- */
#define FO_CONSTANTq(fptr) ((fptr) < (fo_heap) || \
                            (fptr) >= fo_heap + 2 * form_heapsize)
#define FO_OUT_OF_HEAPq(fptr) (0)
#define FO_WRONG_HEAPq(fptr) ((fptr) < old_form_heap)

/* --------------------- String/Fixnum Heap --------------------------------- */
#define AR_CONSTANTq(fptr,offs) \
       (AR_STRING((fptr)+(offs)) < (char *)fx_heap1 || \
        AR_STRING((fptr)+(offs)) >= (char *)(fx_heap1 + 2 * fx_heapsize))

#else /* ===================== !SHARED_LIBRARY ============================== */

/* Annahme, Konstanden werden in Adressen unterhalb des Heaps abgelegt. */

/* ---------------------- Float Heap ---------------------------------------- */
#define FL_CONSTANTq(fptr)  ((fptr) < fl_heap1) 
#define FL_OUT_OF_HEAPq(fptr) ((fptr) >= fl_heap1 + 2 * fl_heapsize)
#define FL_WRONG_HEAPq(fptr) \
       ((fptr) < old_fl_heap || (fptr) >= old_fl_heap + fl_heapsize)

/* ---------------------- Form Heap  ---------------------------------------- */
#define FO_CONSTANTq(fptr)  ((fptr) < (fo_heap))
#define FO_OUT_OF_HEAPq(fptr) ((fptr) >= fo_heap + 2 * form_heapsize)
#define FO_WRONG_HEAPq(fptr) \
       ((fptr) < old_form_heap || fptr >= old_form_heap + form_heapsize)

#define AR_CONSTANTq(fptr,offs) (AR_STRING((fptr)+(offs)) < (char *)fx_heap1) 
/* --------------------- String/Fixnum Heap --------------------------------- */

#endif /* SHARED_LIBRARY */


/*------------------------------------------------------------------------------
 * Extern Deklarationen
 *----------------------------------------------------------------------------*/
extern char TOO_MANY_ARGS[];    /* in main.c */
extern char ILLEGAL_ARGS[];     /* in main.c */
extern long tag_counter;        /* in main.c */
extern BOOL bool_result;        /* in main.c */
extern CL_FORM *stack;          /* in system.c */
extern int mv_count;            /* in values.c */
extern CL_FORM mv_buf[];        /* in values.c */
extern CL_FORM bind_stack[];    /* in catch.c */
extern CL_FORM *bind_top;       /* in catch.c */
extern CONTENV *last_cont;      /* in catch.c */
extern CL_FORM *save_stack;     /* in foreign.c */
extern char IllegalType[];      /* in foreign.c */
extern CL_INIT classes[];       /* in generated C Code */

/* Ende */
