/*------------------------------------------------------------------------------
 * Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel
 *------------------------------------------------------------------------------
 * Projekt  : APPLY - A Practicable And Portable Lisp Implementation
 *            ------------------------------------------------------
 * Funktion : obrep2.c - datenrepräsentationsspezifisch
 *
 * $Revision: 1.8 $
 * $Log: obrep2.c,v $
 * Revision 1.8  1994/06/17  15:12:33  sma
 * make_fixnum entfernt, Funktion wurde als Makro in obrep2.h
 * implementiert.
 *
 * Revision 1.7  1994/05/31  14:52:50  sma
 * Neuer Typ CL_UNBOUND.
 *
 * Revision 1.6  1994/05/26  09:21:51  sma
 * Fehler in gc_relocate bezeitigt. Ausserdem Wrong-Heap test eingebaut.
 *
 * Revision 1.5  1994/05/18  15:20:57  sma
 * Komplett neu geschrieben. Funktionsfähig bis auf
 * foreign-function-Datentypen.
 *
 * Revision 1.4  1994/02/18  12:11:00  uho
 * Das Ermitteln konstanter Datenobjekte und der Heapkonsistenz wird
 * durch Makroaufrufe vorgenommen.
 *
 * Revision 1.3  1993/11/04  14:05:11  sma
 * Fehler in save_form2 gehoben
 *
 * Revision 1.2  1993/10/29  15:21:23  sma
 * Fehlerkorrekturen.
 *
 * Revision 1.1  1993/10/14  15:43:26  sma
 * Initial revision
 *
 *----------------------------------------------------------------------------*/

#include <c_decl.h>
#include "sys.h"

#ifdef __STDC__
#include <stdlib.h>
#else
#include <malloc.h>
#endif

#if __OBREP == 2

/*------------------------------------------------------------------------------
 * LISP-Laufzeitstack
 *----------------------------------------------------------------------------*/
extern CL_FORM *stack;
extern CL_FORM *eos;              /* end of stack */
extern unsigned stacksize;

/*------------------------------------------------------------------------------
 * LISP-Heap fuer CL_FORMs
 *----------------------------------------------------------------------------*/
extern CL_FORM *fo_heap;
extern CL_FORM *form_heap;
extern CL_FORM *form_toh;
extern CL_FORM *form_eoh;
extern CL_FORM *old_form_heap;
extern unsigned form_heapsize;

/*------------------------------------------------------------------------------
 * LISP-Heap fuer INTEGERs und STRINGs
 *----------------------------------------------------------------------------*/
extern long *fx_heap1;
extern long *fx_heap;
extern long *fx_toh;
extern long *fx_eoh;
extern long *old_fx_heap;
extern unsigned fx_heapsize;


/*------------------------------------------------------------------------------
 * LISP-Heap fuer FLOATs
 *----------------------------------------------------------------------------*/
extern double *fl_heap1;
extern double *fl_heap;
extern double *fl_toh;
extern double *fl_eoh;
extern double *old_fl_heap;
extern unsigned fl_heapsize;

void gc_main();
void gc_relocate();
void gc_scan_newheap();

/*------------------------------------------------------------------------------
 * Speicherbereinigungsroutine
 *----------------------------------------------------------------------------*/
void do_gc(top)
CL_FORM *top;
{
   CL_FORM *p;

   /* Traversieren der Wurzelbereiche: Hauptmodul und importierte Modulen */
   gc_main();
   
   /* Traversieren des LISP-Laufzeitstacks */
   for (p = stack; p < top; p++)
      gc_relocate(p);
   
   /* Traversieren des Binding-Stacks */
   for (p = bind_stack; p < bind_top; p++)
      gc_relocate(p);
   
   /* Alle lebenden Daten in neuen heap ziehen */
   gc_scan_newheap();
}

#define CL_FORWARD ((1 << TAG_BITS) - 1)
#define GCMARK(loc) (TYPE_OF(loc) == CL_FORWARD)
#define GCSETMARK(loc) ((loc)->form[0].d = SIZE_TAG(CL_FORWARD, 0))
#define GCREF(loc) ((loc)->form[1].form)

/*------------------------------------------------------------------------------
 * Verschiebe nicht-konstante Daten in neuen Heap
 *----------------------------------------------------------------------------*/
void gc_relocate(f)
CL_FORM *f;
{
   CL_FORM *p, *np;
   long sz;

   /* Daten im Konstantenspeicher werden nicht kopiert */
   p = f->form;
   if (FO_CONSTANTq(p))
      return;

   /* Etwas Paranoia kann nicht schaden */
   if (FO_WRONG_HEAPq(p))
      Labort ("Wrong heap");
   
   /* Es es eine Referenz? */
   if (GCMARK(f))
   {
      f->form = GCREF(f);
      return;
   }
   
   /* Datum in neuen Heap kopieren */
   np = form_toh;
   switch (TYPE_OF(f))
   {
   case CL_FIXNUM:
   case CL_UNIQUE_TAG:
      np[0] = p[0];
      form_toh++;
      f->form = np;
      return;
   case CL_CONS:
      np[0] = p[0];
      np[1] = p[1];
      np[2] = p[2];
      form_toh += CONS_SIZE;
      break;
   case CL_SYMBOL:
      if (!AR_CONSTANTq(p, OFF_SYM_NAME))
         AR_STRING(p + OFF_SYM_NAME) = ch_swap(AR_STRING(p + OFF_SYM_NAME), 
                                               AR_SIZE(p + OFF_SYM_NAME));
      sz = SYM_SIZE;
      goto swap1;
   case CL_STRUCT:
      sz = AR_SIZE(p) + 2;
      goto swap1;
   case CL_CLOSURE:
   case CL_INSTANCE:
   case CL_SMVEC_T:
      sz = AR_SIZE(p) + 1;
   swap1:
      memcpy(np, p, sizeof(CL_FORM) * sz);
      form_toh += sz;
      break;
   case CL_SMVEC_FIXNUM:
      FIXNUM_AR(p) = fx_swap(FIXNUM_AR(p), AR_SIZE(p));
      goto swap2;
   case CL_SMVEC_FLOAT:
      FLOAT_AR(p) = fl_swap(FLOAT_AR(p), AR_SIZE(p));
      goto swap2;
   case CL_SMVEC_CHARACTER:
      if (!AR_CONSTANTq(p,0))
            AR_STRING(p) = ch_swap(AR_STRING(p), AR_SIZE(p));
      goto swap2;
   case CL_SMVEC_BIT:
      BIT_AR(p) = bits_swap(BIT_AR(p), AR_SIZE(p));
      goto swap2;
   case CL_FLOAT:
      FLOAT_AR(p) = fl_swap(FLOAT_AR(p), 1);
   case CL_IND:
   case CL_CFILE:
   swap2:
      np[0] = p[0];
      np[1] = p[1];
      form_toh += 2;
      break;
   default:
      fprintf(stderr, ";;; Unkonwn data type %d by GC\n", TYPE_OF(f));
      exit(1);
   }
   GCSETMARK(f);
   GCREF(f) = np;
   f->form = np;
}

void gc_scan_newheap()
{
   CL_FORM *p = form_heap;
   long sz;

   while (p < form_toh)
   {
      if (form_toh >= form_eoh)
         Labort("Heap overflow");

      switch (TAG_FIELD(p))
      {
      case CL_FIXNUM:
      case CL_UNIQUE_TAG:
         p++;
         break;
      case CL_CONS:
         gc_relocate(CAR(p));
         gc_relocate(CDR(p));
         p += CONS_SIZE;
         break;
      case CL_SYMBOL:
         gc_relocate(OFFSET(p, OFF_SYM_VALUE));
         gc_relocate(OFFSET(p, OFF_SYM_PLIST));
         gc_relocate(OFFSET(p, OFF_SYM_PACKAGE));
         p += SYM_SIZE;
         break;
      case CL_STRUCT:
         sz = AR_SIZE(p) + 1;
         goto scan;
      case CL_CLOSURE:
         sz = AR_SIZE(p) - 2;
         p += 2;
         goto scan;
      case CL_INSTANCE:
      case CL_SMVEC_T:
         sz = AR_SIZE(p);
      scan:
         p++;
         while (sz > 0)
         {
            gc_relocate(p);
            p++;
            sz--;
         }
         break;
      case CL_IND:
         gc_relocate(p + 1);
      case CL_SMVEC_FIXNUM:
      case CL_SMVEC_FLOAT:
      case CL_SMVEC_CHARACTER:
      case CL_SMVEC_BIT:
      case CL_FLOAT:
      case CL_CFILE:
         p += 2;
         break;
      default:
         fprintf(stderr, ";;; Unkonwn data type %d by GC\n", TAG_FIELD(p));
         exit(1);
      }
   }
}

/*============================================================================*/

/*------------------------------------------------------------------------------
 * Konstruktor-Funktionen für einfache Daten
 *----------------------------------------------------------------------------*/
CL_FORM nil_ob, unbound_ob;
CL_FORM char_ob[256];
CL_FORM fixnum_ob[2000];

void init_ob()
{
   int i;

   /* NIL */
   nil_ob.d = SIZE_TAG(CL_NIL, 0);
   /* unbound */
   unbound_ob.d = SIZE_TAG(CL_UNBOUND, 0);

   /* Characters */
   for (i = 0; i < 256; i++)
      char_ob[i].d = SIZE_TAG(CL_CHAR, i);

   /* Fixnums */
   for (i = 0; i < 2000; i++)
      fixnum_ob[i].d = SIZE_TAG(CL_FIXNUM, i - 1000);
}

/*------------------------------------------------------------------------------
 * Konstruktor-Funktionen
 *----------------------------------------------------------------------------*/

CL_FORM *make_flt(base, fl)
CL_FORM *base;
double *fl;
{
   CL_FORM *fld = form_alloc(base, 2);
   fld[0].d = SIZE_TAG(CL_FLOAT, 0);
   fld[1].fl = fl;
   return fld;
}

#endif
