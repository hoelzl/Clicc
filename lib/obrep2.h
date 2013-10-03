/*------------------------------------------------------------------------------
 * Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel
 *------------------------------------------------------------------------------
 * Projekt  : APPLY - A Practicable And Portable Lisp Implementation
 *            ------------------------------------------------------
 * Funktion : obrep2.h - datenrepräsentationsspezifisch
 *
 * $Revision: 1.11 $
 * $Log: obrep2.h,v $
 * Revision 1.11  1994/06/17  15:16:56  sma
 * MAKE_FIXNUM und ALLOC_CONS rufen nur noch form_alloc auf, wenn eine GC
 * durchgefuehrt werden muss, ansonsten geschieht das allozieren von
 * Speicher inline. FIXN_TAG und FIXN_FIELD eingefuehrt, die
 * vorzeichenbehaftete Zahlen in das Bitfeld fuer Groesse/Char/Fixnum
 * schreiben.
 *
 * Revision 1.10  1994/05/31  14:52:22  sma
 * CL_UNBOUND eingefuehrt, CL_LISTP verbessert (dazu Tagnummern
 * umgestellt) und TAG_BITS-Konstante in SYM_CONST_FLAG benutzt.
 *
 * Revision 1.9  1994/05/26  08:49:25  sma
 * Ein paar kosmetische Aenderungen das SIZE_TAG Makro betreffend.
 *
 * Revision 1.8  1994/05/25  12:45:56  uho
 * Aufruf des undefinierten Makros FNUM in SIZE_TAG entfernt.
 *
 * Revision 1.7  1994/05/24  14:04:15  sma
 * Tag-Größe mal probehalber von 8 auf 5 bits reduziert (macht die
 * fixnums größer).
 *
 * Revision 1.6  1994/05/22  15:00:08  sma
 * LOAD_SMALLFIXNUM-Makro eingefügt.
 *
 * Revision 1.5  1994/05/18  15:15:42  sma
 * Komplett neu geschrieben. Funktionsfähig bis auf
 * foreign-function-interface-Funktionen.
 *
 * Revision 1.4  1993/11/12  13:09:42  sma
 * Neue Konstante BITS_PER_FIXNUM.
 *
 * Revision 1.3  1993/11/04  14:10:26  sma
 * Neues internes Makro: LOAD_MASK
 *
 * Revision 1.2  1993/10/29  15:01:53  sma
 * Alle notwendigen Änderungen für korrektes Funktionieren.
 *
 * Revision 1.1  1993/10/14  15:55:12  sma
 * Initial revision
 *
 *----------------------------------------------------------------------------*/

/*------------------------------------------------------------------------------
 * Datenstruktur der LISP-Objekte
 *----------------------------------------------------------------------------*/

typedef union cl_form CL_FORM;
union cl_form {
   CL_FORM *form;
   long d;
   long *i;
   double *fl;
   char *str;
   CLOSURE_FUN *fun;
   DOWN_FUNARG *dfarg;
   GLOBAL_FUNARG *gfarg;
   FILE *file;
};

/*------------------------------------------------------------------------------
 * Werte für UNBOUND und NIL
 *----------------------------------------------------------------------------*/

extern CL_FORM nil_ob, unbound_ob;

#define UNBOUND_VALUE (&unbound_ob)
#define NIL_VALUE (&nil_ob)

/*------------------------------------------------------------------------------
 * Konstruktoren für Konstantendefinitionen
 *----------------------------------------------------------------------------*/

typedef CL_FORM CL_INIT;

#define END_SYMDEF  0           /* Endmarkierung einer Symboltabelle */
#define IS_END_SYMDEF(x)        ((x)->d == END_SYMDEF)

#define MAKE_NIL (CL_INIT *)NIL_VALUE
#define MAKE_UNBOUND (CL_INIT *)UNBOUND_VALUE
#define MAKE_FIXNUM(num) (CL_INIT *)FIXN_TAG(CL_FIXNUM, num)
#define MAKE_FLOAT(flptr) (CL_INIT *)SIZE_TAG(CL_FLOAT, 0), (CL_INIT *)(flptr)
#define MAKE_CHAR(chr) (CL_INIT *)&char_ob[(unsigned char)(chr)]
#define MAKE_STRING(len, str)\
   (CL_INIT *)SIZE_TAG(CL_SMSTR, len), (CL_INIT *)(str)
#define MAKE_SYMBOL(len, str, val, pkg)\
   (CL_INIT *)SIZE_TAG(CL_SYMBOL, 0), (CL_INIT *)(val), MAKE_NIL,\
   (CL_INIT *)(pkg), MAKE_STRING(len, str)
#define MAKE_CONST_SYMBOL(len, str, val, pkg)\
   (CL_INIT *)SIZE_TAG(CL_SYMBOL, 1), (CL_INIT *)(val), MAKE_NIL,\
   (CL_INIT *)(pkg), MAKE_STRING(len, str)
#define MAKE_CLASS(name, cpl, slotnum, slotinfo)\
   (CL_INIT *)SIZE_TAG(CL_INSTANCE, 5), MAKE_NIL, MAKE_SYMREF(name),\
   (CL_INIT *)(cpl), MAKE_SMALLFIXNUMREF(slotnum), (CL_INIT *)(slotinfo)
#define MAKE_CONS (CL_INIT *)SIZE_TAG(CL_CONS, 0)
#define MAKE_VECTOR(sz) (CL_INIT *)SIZE_TAG(CL_SMVEC_T, sz)
#define MAKE_STRUCT(sz, type) (CL_INIT *)SIZE_TAG(CL_STRUCT, sz), type

#define MAKE_SMALLFIXNUMREF(i) (CL_INIT *)&fixnum_ob[(i) + 1000]
#define MAKE_FIXNUMREF(iptr) (CL_INIT *)(iptr)
#define MAKE_FLOATREF(fptr) (CL_INIT *)(fptr)
#define MAKE_STRREF(str) (CL_INIT *)(str)
#define MAKE_SYMREF(sym) (CL_INIT *)(sym)
#define MAKE_CLASSREF(class) (CL_INIT *)(class)
#define MAKE_CONSREF(list) (CL_INIT *)(list)
#define MAKE_VECREF(vec) (CL_INIT *)(vec)
#define MAKE_STRUCTREF(str) (CL_INIT *)(str)
#define MAKE_GLOBFUN(fun) (CL_INIT *)(fun)

/*------------------------------------------------------------------------------
 * Vektortyp-Codes 
 *----------------------------------------------------------------------------*/

#define VT_T          0
#define VT_FIXNUM     1
#define VT_FLOAT      2
#define VT_CHARACTER  3
#define VT_BIT        4

#define GET_VECTOR_CODE(loc)  (TYPE_OF(loc) - CL_SMVEC)

/*------------------------------------------------------------------------------
 * Datentypen-Definition
 *----------------------------------------------------------------------------*/
/* Reihenfolge/Nummer von NIL und CONS wichtig fuer CL_LISTP */

#define CL_NIL               0  /* symbol, list, sequence */
#define CL_CONS              1  /*         list, sequence */
#define CL_FIXNUM            2  /* number */
#define CL_FLOAT             3  /* number */
#define CL_CHAR              4
#define CL_SYMBOL            5  /* symbol */

#define CL_SMVEC             6 
#define CL_SMVEC_T          (CL_SMVEC+VT_T)
#define CL_SMVEC_FIXNUM     (CL_SMVEC+VT_FIXNUM)
#define CL_SMVEC_FLOAT      (CL_SMVEC+VT_FLOAT)
#define CL_SMVEC_CHARACTER  (CL_SMVEC+VT_CHARACTER)
#define CL_SMVEC_BIT        (CL_SMVEC+VT_BIT)
#define CL_SMSTR            CL_SMVEC_CHARACTER

#define CL_CLOSURE          11  /* function */
#define CL_DOWNFUN          12  /* function */
#define CL_GLOBFUN          13  /* function */
#define CL_IND              14

#define CL_STRUCT           15  /* Structure */
#define CL_CFILE            16
#define CL_UNIQUE_TAG       17  /* fuer automatisch generierte CATCH-Tags */
#define CL_INSTANCE         18  /* CLOS Instanz */

#define CL_UNBOUND          19
/* foreign-Datentypen */
/* ... */

/*------------------------------------------------------------------------------
 * Zugriff auf tag und Größen/Char/Fixnum-Feld
 *----------------------------------------------------------------------------*/

#define TAG_BITS 5
#define TAG_MASK ((1 << TAG_BITS) - 1)
#define TAG_FIELD(form) ((int)((form)->d & TAG_MASK))
#define SIZE_FIELD(form) ((long)((form)->d >> TAG_BITS))
#define CHAR_FIELD(form) ((unsigned char)((form)->d >> TAG_BITS))
#define FIXN_FIELD(form) ((long)((form)->d / (1 << TAG_BITS)))
#define SIZE_TAG(tag,sz) (((long)(sz) << TAG_BITS) + (tag))
#define FIXN_TAG(tag,n) (((long)(n) << TAG_BITS) + (tag))

/*------------------------------------------------------------------------------
 * Typtest-Prädikate
 *----------------------------------------------------------------------------*/

#define TYPE_OF(loc) TAG_FIELD((loc)->form)

#define CL_NILP(loc) ((loc)->form == NIL_VALUE)
#define CL_TRUEP(loc) NOT(CL_NILP(loc))
#define CL_UNBOUNDP(loc) ((loc)->form == UNBOUND_VALUE)
#define CL_FIXNUMP(loc) (TYPE_OF(loc) == CL_FIXNUM)
#define CL_FLOATP(loc) (TYPE_OF(loc) == CL_FLOAT)
#define CL_CHARP(loc) (TYPE_OF(loc) == CL_CHAR)
#define CL_CONSP(loc) (TYPE_OF(loc) == CL_CONS)
#define CL_ATOMP(loc) NOT(CL_CONSP(loc))
#define CL_SYMBOLP(loc) (TYPE_OF(loc) == CL_SYMBOL)
#define CL_INSTANCEP(loc) (TYPE_OF(loc) == CL_INSTANCE)
#define CL_STRUCTP(loc) (TYPE_OF(loc) == CL_STRUCT)
#define CL_SMVECP(loc) \
(TYPE_OF(loc) >= CL_SMVEC_T && TYPE_OF(loc) <= CL_SMVEC_BIT)
#define CL_SMVEC_T_P(loc) (TYPE_OF(loc) == CL_SMVEC_T)
#define CL_SMVEC_BIT_P(loc) (TYPE_OF(loc) == CL_SMVEC_BIT)
#define CL_SMSTRP(loc) (TYPE_OF (loc) == CL_SMSTR)
#define CL_CLOSUREP(loc) (TYPE_OF(loc) == CL_CLOSURE)
#define CL_GLOBFUNP(loc) (TYPE_OF(loc) == CL_GLOBFUN)
#define CL_DOWNFUNP(loc) (TYPE_OF(loc) == CL_DOWNFUN)

/* foreign typtests */
/* ... */

#define CL_NUMBERP(loc) (CL_FIXNUMP(loc) || CL_FLOATP(loc))
#define CL_LISTP(loc) (TYPE_OF(loc) <= CL_CONS)
#define CL_FUNCTIONP(loc) \
(CL_CLOSUREP(loc) || CL_DOWNFUNP(loc) || CL_GLOBFUNP(loc))

/*------------------------------------------------------------------------------
 * GET-Makros
 *----------------------------------------------------------------------------*/

#define GET_FORM(loc) ((loc)->form)

#define GET_FIXNUM(loc) FIXN_FIELD((loc)->form)
#define GET_FLOAT(loc) (*((loc)->form[1].fl))
#define GET_CHAR(loc) CHAR_FIELD((loc)->form)
#define GET_CAR(loc) CAR((loc)->form)
#define GET_CDR(loc) CDR((loc)->form)
#define GET_SYMBOL(loc) GET_FORM(loc)
#define GET_FIXNUM_PTR(loc) ((loc)->i)
#define GET_FLOAT_PTR(loc) ((loc)->fl)
#define GET_CHAR_PTR(loc) ((loc)->str)
#define GET_BITS_PTR(loc) ((loc)->i)
#define GET_FUN(loc) ((loc)->fun)
#define GET_DFARG(loc) ((loc)->dfarg)
#define GET_GFARG(loc) ((loc)->gfarg) 
#define GET_CFILE(loc) ((loc)->form[1].file)
#define INDIRECT(loc) (&(loc)->form[1])

/* foreign-GETs */
/* ... */

/*------------------------------------------------------------------------------
 * Datum auf LISP-Laufzeitstack laden
 *----------------------------------------------------------------------------*/

#define LOAD_NIL(loc) ((loc)->form = NIL_VALUE)
#define LOAD_T(loc) LOAD_CHAR(loc,'T',loc)
#define LOAD_UNBOUND(loc) ((loc)->form = UNBOUND_VALUE)
#define LOAD_FIXNUM(top,i,loc) do { long _i = (i); \
  if (_i >= -1000 && _i < 1000) LOAD_SMALLFIXNUM(_i, loc); \
  else { if (((loc)->form = form_toh++) > form_eoh) \
  (loc)->form = form_alloc(top, 1); INIT_FIXNUM((loc)->form, _i); }} while(0)

#define LOAD_SMALLFIXNUM(i, loc) ((loc)->form = &fixnum_ob[(i) + 1000])
#define LOAD_FLOAT(top,fl,loc) ((loc)->form = make_flt(top, fl))
#define LOAD_CHAR(top,ch,loc) ((loc)->form = (&char_ob[(unsigned char)(ch)]))
#define LOAD_CONS(cons,loc) ((loc)->form = (cons))
#define LOAD_SYMBOL(sym,loc) ((loc)->form = (sym))
#define LOAD_CLASS(class,loc) ((loc)->form = (class))
#define LOAD_INSTANCE(inst,loc) ((loc)->form = (inst))
#define LOAD_STRUCT(st,loc) ((loc)->form = (st))
#define LOAD_FORM_PTR(ptr, loc) ((loc)->form = (ptr))
#define LOAD_FIXNUM_PTR(ptr, loc) ((loc)->i = (ptr))
#define LOAD_FLOAT_PTR(ptr, loc) ((loc)->fl = (ptr))
#define LOAD_CHAR_PTR(ptr, loc) ((loc)->str = (ptr))
#define LOAD_BITS_PTR(ptr, loc) ((loc)->i = (ptr))
#define LOAD_VEC_T(vec, loc) ((loc)->form = (vec))
#define LOAD_VEC_FIXNUM(vec, loc) ((loc)->form = (vec))
#define LOAD_VEC_FLOAT(vec, loc) ((loc)->form = (vec))
#define LOAD_VEC_CHAR(vec, loc) ((loc)->form = (vec))
#define LOAD_VEC_BIT(vec, loc) ((loc)->form = (vec))
#define LOAD_SMSTR(smstr, loc) ((loc)->form = (smstr))
#define LOAD_CLOSURE(fun, loc) ((loc)->form = (fun))
#define LOAD_GLOBFUN(fun, loc) ((loc)->gfarg = (fun))
#define LOAD_DOWNFUN(fun, loc) ((loc)->dfarg = (fun))
#define LOAD_CFILE(top,fp,loc) { \
   CL_FORM *fd = form_alloc(top, 2); \
   fd[0].d = SIZE_TAG(CL_CFILE, 0); \
   fd[1].file = fp; \
   (loc)->form = fd; \
}
#define LOAD_UNIQUE_TAG(loc) \
   (loc)->form = form_alloc(loc, 1); \
   (loc)->form->d = SIZE_TAG(CL_UNIQUE_TAG, tag_counter++)

/* foreign-loads */
/* ... */

/*------------------------------------------------------------------------------
 * Cons-Zellen
 *----------------------------------------------------------------------------*/

#define CONS_SIZE 3
#define CAR(cons) OFFSET(cons, 1)
#define CDR(cons) OFFSET(cons, 2)

#define INIT_CONS(cons)  ((cons)->d = SIZE_TAG(CL_CONS, 0))

#define ALLOC_CONS(top,car,cdr,loc) { \
   CL_FORM *cons; \
   if ((cons = form_toh) + CONS_SIZE > form_eoh) \
      cons = form_alloc(top, CONS_SIZE); \
   else form_toh += CONS_SIZE; \
   INIT_CONS(cons); \
   COPY(car, CAR(cons)); \
   COPY(cdr, CDR(cons)); \
   LOAD_CONS(cons, loc); \
}

/*------------------------------------------------------------------------------
 * Symbole
 *----------------------------------------------------------------------------*/

#define SYM_SIZE           6    /* Anzahl der CL_FORMs */

#define OFF_SYM_VALUE      1
#define OFF_SYM_PLIST      2
#define OFF_SYM_PACKAGE    3
#define OFF_SYM_NAME       4    /* eingebetteter simple-string */

#define SYM_VALUE(s)    OFFSET(GET_FORM(s), OFF_SYM_VALUE)
#define SYM_PLIST(s)    OFFSET(GET_FORM(s), OFF_SYM_PLIST)
#define SYM_PACKAGE(s)  OFFSET(GET_FORM(s), OFF_SYM_PACKAGE)
#define SYM_NAME(s)     OFFSET(GET_FORM(s), OFF_SYM_NAME)

#define SYM_SET_NAME(name,s)  COPY(OFFSET(name,0), OFFSET(SYM_NAME(s),0));\
                              COPY(OFFSET(name,1), OFFSET(SYM_NAME(s),1))

#define SYM_CONSTFLAG      (1 << TAG_BITS)
#define SYM_IS_CONST(s)    (GET_FORM(s)->d & SYM_CONSTFLAG)
#define SYM_SET_CONST(s)   (GET_FORM(s)->d |= SYM_CONSTFLAG)
#define SYM_SET_NORMAL(s)  (GET_FORM(s)->d &= ~SYM_CONSTFLAG)

#define SYMBOL(base,index)  ((CL_FORM *)&base[(index)*SYM_SIZE])
#define SYMVAL(base,index)  ((CL_FORM *)&base[(index)*SYM_SIZE] + OFF_SYM_VALUE)

#define INIT_SYMBOL(sym, name) \
   (sym)->d = SIZE_TAG(CL_SYMBOL, 0); \
   COPY(OFFSET(GET_FORM(name), 0), OFFSET(sym, OFF_SYM_NAME + 0)); \
   COPY(OFFSET(GET_FORM(name), 1), OFFSET(sym, OFF_SYM_NAME + 1))

/*------------------------------------------------------------------------------
 * Klassen
 *----------------------------------------------------------------------------*/

#define CLASS_SIZE  6           /* Anzahl der CL_FORMs */
#define CLASS(index)  ((CL_FORM *)&classes[CLASS_SIZE * index])

/*------------------------------------------------------------------------------
 * Vektoren
 *----------------------------------------------------------------------------*/

#define AR_SIZE(ar)          SIZE_FIELD(ar)
#define SET_AR_SIZE(sz, ar)  ((ar)->d = SIZE_TAG(TAG_FIELD(ar), sz))
#define AR_BASE(ar)          OFFSET(ar, 1)
#define AR_STRING(ar)        GET_CHAR_PTR(AR_BASE(ar))

#define FORM_AR(ar)          GET_FORM(AR_BASE(ar))
#define FIXNUM_AR(ar)        GET_FIXNUM_PTR(AR_BASE(ar))
#define FLOAT_AR(ar)         GET_FLOAT_PTR(AR_BASE(ar))
#define CHAR_AR(ar)          GET_CHAR_PTR(AR_BASE(ar))
#define BIT_AR(ar)           GET_BITS_PTR(AR_BASE(ar))

/*------------------------------------------------------------------------------
 * Makro für EQ und EQL-Funktion
 *----------------------------------------------------------------------------*/
#define EQ(x, y) (GET_FORM(x) == GET_FORM(y))

#define EQL(x, y) (EQ(x, y) \
|| (CL_FIXNUMP(x) && (GET_FORM(x)->d == GET_FORM(y)->d)) \
|| (CL_FLOATP(x) && (GET_FLOAT(x) == GET_FLOAT(y))))

/*------------------------------------------------------------------------------
 * Kopieren einer statischer Variablen in den Heap
 *----------------------------------------------------------------------------*/
#define GEN_HEAPVAR(var,top) { \
   CL_FORM *_hptr = form_alloc(top, 2); \
   _hptr[0].d = SIZE_TAG(CL_IND, 0); \
   _hptr[1].d = (var)->d; \
   (var)->form = _hptr; \
}

/*------------------------------------------------------------------------------
 * CLOSURE auf Heap erzeugen und Kopf ausfüllen
 *----------------------------------------------------------------------------*/
#define GEN_CLOSURE(ar,top,sz,code,ps) \
     CL_FORM *ar = form_alloc(top, sz); \
     ar[0].d = SIZE_TAG(CL_CLOSURE, (sz) - 1); \
     ar[1].fun = code; \
     ar[2].d = ps

/*------------------------------------------------------------------------------
 * Anzahl der Bits, die für Bitvektoren in einer Fixnum genutzt werden
 *----------------------------------------------------------------------------*/
#define BITS_PER_FIXNUM 24

/*------------------------------------------------------------------------------
 * INITs
 *----------------------------------------------------------------------------*/

#define INIT_VEC_T(v,sz) ((v)->d = SIZE_TAG(CL_SMVEC_T, sz))
#define INIT_VEC_FIXNUM(v,sz) ((v)->d = SIZE_TAG(CL_SMVEC_FIXNUM, sz))
#define INIT_VEC_FLOAT(v,sz) ((v)->d = SIZE_TAG(CL_SMVEC_FLOAT, sz))
#define INIT_VEC_CHAR(v,sz) ((v)->d = SIZE_TAG(CL_SMVEC_CHARACTER, sz))
#define INIT_VEC_BIT(v,sz) ((v)->d = SIZE_TAG(CL_SMVEC_BIT, sz))
#define INIT_INSTANCE(vec,sz)  ((vec)->d = SIZE_TAG(CL_INSTANCE, sz))
#define INIT_STRUCT(st,sz)  ((st)->d = SIZE_TAG(CL_STRUCT, sz))
#define INIT_FIXNUM(fob,n) ((fob)->d = FIXN_TAG(CL_FIXNUM, n))

/*------------------------------------------------------------------------------
 * Funktionen für Zugriff auf optimierte Restparameter
 *----------------------------------------------------------------------------*/
#define LOCAL(x)  STACK(local, x)
#define REST_LENGTH(r, loc)  LOAD_FIXNUM(loc, local - (r), loc)
#define REST_CAR(r, loc)  if ((r) == local) LOAD_NIL(loc); else COPY(r, loc)
#define REST_CDR(r)  (((r) != local) ? (r) + 1: (r))
#define REST_APPLY(b, n, r)  rest_apply(b, n, (local - (r)), (r))
#define REST_NOT_EMPTY(r)  (local != (r))

/*------------------------------------------------------------------------------
 * Garbage-Collector-Funktion
 *----------------------------------------------------------------------------*/
#define SAVE_FORM(form)  gc_relocate(form)

#define INIT_FUN init_ob()

/*------------------------------------------------------------------------------
 * Abstraktionen
 *----------------------------------------------------------------------------*/
#define GEN_FLOAT(top,fl,loc) { \
   static double _float = (fl); \
   LOAD_FLOAT(top, &_float, loc); \
}
#define GEN_SMSTR(len,str,loc) { \
   CL_INIT local_string [] = { \
   MAKE_STRING(len,str); \
   }; \
   LOAD_SMSTR((CL_FORM *)local_string, loc); \
}
#define GEN_GLOBAL_FUNARG(name,fun,par) \
   GLOBAL_FUNARG name = { SIZE_TAG(CL_GLOBFUN, par), fun }
#define GEN_STATIC_GLOBAL_FUNARG(name,fun,par) \
   static GLOBAL_FUNARG name = { SIZE_TAG(CL_GLOBFUN, par), fun }
#define INIT_DOWN_FUNARG(name,_fun,_par,_disp) \
   name.par_spec = SIZE_TAG(CL_DOWNFUN, _par); \
   name.fun = (_fun); \
   name.display = (_disp)
   
/*------------------------------------------------------------------------------
 * Definition und Zugriff auf funktionale Objekte
 *----------------------------------------------------------------------------*/
struct global_funarg
{
   long par_spec;
   GLOBAL_FUN *fun;
};

struct down_funarg
{
   long par_spec;
   LOCAL_FUN *fun;
   CL_FORM **display;
};

#define GET_GLOBFUN_CODE(ptr) (*(ptr->fun))
#define GET_GLOBFUN_PARSPEC(ptr) SIZE_FIELD((CL_FORM *)(ptr))

#define GET_DOWNFUN_CODE(ptr) (*(ptr->fun))
#define GET_DOWNFUN_PARSPEC(ptr) SIZE_FIELD((CL_FORM *)(ptr))
#define GET_DOWNFUN_DISPLAY(ptr) (ptr->display)

#define GET_CLOSURE_CODE(closure)  ((closure)->form[1].fun)
#define GET_CLOSURE_PARSPEC(closure) ((closure)->form[2].d)

/*------------------------------------------------------------------------------
 * Extern Deklarationen
 *----------------------------------------------------------------------------*/

extern CL_FORM fixnum_ob[];
extern CL_FORM char_ob[];
extern CL_FORM *make_flt();
extern CL_FORM *form_toh;
extern CL_FORM *form_eoh;
