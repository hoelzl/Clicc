/*------------------------------------------------------------------------------
 * Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel
 *-----------------------------------------------------------------------------
 * Projekt  : APPLY - A Practicable And Portable Lisp Implementation
 *            ------------------------------------------------------
 * Funktion : Laufzeitsystem: Funktionen des FFI
 *
 * $Revision: 1.20 $
 * $Log: foreign.c,v $
 * Revision 1.20  1994/05/17  08:38:18  pm
 * Anpassung an den Standard-C-Compiler:
 * - Headerfiles nur bedingt einlesen
 *
 * Fehler behoben.
 *
 * Revision 1.19  1994/04/28  09:58:55  pm
 * Stefans Fehler korrigiert.
 *
 * Revision 1.18  1994/04/28  09:46:06  sma
 * LOAD_FIXNUM, LOAD_CHAR und LOAD_FLOAT um 3. Argument ergänzt.
 *
 * Revision 1.17  1994/04/22  14:15:16  pm
 * Foreign Function Interface voellig ueberarbeitet.
 * - Fehler behoben.
 * - zwei weitere Hilfsfunktionen.
 *
 * Revision 1.16  1994/04/18  12:20:31  pm
 * Foreign Function Interface voellig ueberarbeitet.
 * - Funktionen zum Umgang mit Objekten vom 'Typ C` ueberarbeitet.
 *
 * Revision 1.15  1994/01/13  09:02:01  sma
 * Die bis jetzt unimplementierten Typtests c_struct_p, FFI_c_float_p,
 * FFI_c_double_p und FF_c_long_double_p geben jetzt NIL zurück.  (Damit
 * print bei (print #<unbound>) nicht versucht, ein c-float auszugeben)
 *
 * Revision 1.14  1994/01/05  12:48:02  sma
 * Namensänderung: Alle Laufzeitsystemfunktionen mit dem Präfix rt_
 * versehen und den Postfix _internal entfernt. STACK(base,x) -> ARG(x)
 *
 * Revision 1.13  1993/12/16  16:39:19  pm
 * Definition des strings IllegalType.
 *
 * Revision 1.12  1993/09/28  15:05:59  pm
 * C-Konstruktorfuntionen nach cginline geschoben.
 *
 * Revision 1.11  1993/08/27  11:52:57  sma
 * unsigned -> unsigned short  in Zeile 379.
 *
 * Revision 1.10  1993/08/27  10:26:16  pm
 * Mehr Castings eingebaut
 *
 * Revision 1.9  1993/08/24  11:21:00  pm
 * Erweiterungen um C-Pointer
 *
 * Revision 1.8  1993/07/22  09:13:35  pm
 * Funktionen zur Verwaltung von C-Strukturen
 *
 * Revision 1.7  1993/06/16  14:43:22  hk
 * Copyright Notiz eingefuegt.
 *
 * Revision 1.6  1993/06/04  13:45:45  pm
 * Globale Variable save_stack eingefuegt
 *
 * Revision 1.5  1993/05/31  17:08:07  pm
 * Schreibfehler beseitigt
 *
 * Revision 1.4  1993/05/23  17:53:54  pm
 * alle in C geschriebenen Konstruktor-, Test- und
 * Konvertierungsfunktionen fuer die primitiven Typen implementiert
 *
 * Revision 1.3  1993/05/21  13:55:24  pm
 * c-int in int umbenannt
 *
 * Revision 1.2  1993/05/05  09:10:54  pm
 * einige Funtionen zum Umgang mit c-ints eingebaut
 *
 * Revision 1.1  1993/04/28  14:23:23  pm
 * Initial revision
 *
 *----------------------------------------------------------------------------*/

#include <c_decl.h>

#if __STDC__
#include <stdlib.h>
#else
#include <malloc.h>
#endif

#include <string.h>
#include "sys.h"

/*------------------------------------------------------------------------------
 * globale Variable, in der die aktuelle Stackposition beim Aufruf
 * einer Call-Out-Funktion gemerkt wird, damit beim Aufruf einer
 * Call-In-Funktion diese Stelle verwendet werden kann.
 *----------------------------------------------------------------------------*/
CL_FORM *save_stack;

/*------------------------------------------------------------------------------
 * wird im erzeugten Code verwendet.
 *----------------------------------------------------------------------------*/
char IllegalType[] = "illegal Type in Call-Out";


/*******************************************************************************
 * Konstruktor Funktionen fuer Primitive.
 * Argument: object
 ******************************************************************************/
void rt_make_c_char (base)
CL_FORM *base;
{
   TYPE_OF(ARG(0)) = CL_C_CHAR;
}

void rt_make_c_short (base)
CL_FORM *base;
{
   TYPE_OF(ARG(0)) = CL_C_SHORT;
}

void rt_make_c_int (base)
CL_FORM *base;
{
   TYPE_OF(ARG(0)) = CL_C_INT;
}

void rt_make_c_long (base)
CL_FORM *base;
{
   TYPE_OF(ARG(0)) = CL_C_LONG;
}

void rt_make_c_unsigned_char (base)
CL_FORM *base;
{
   TYPE_OF(ARG(0)) = CL_C_UNSIGNED_CHAR;
}

void rt_make_c_unsigned_short (base)
CL_FORM *base;
{
   TYPE_OF(ARG(0)) = CL_C_UNSIGNED_SHORT;
}

void rt_make_c_unsigned_int (base)
CL_FORM *base;
{
   TYPE_OF(ARG(0)) = CL_C_UNSIGNED_INT;
}

void rt_make_c_unsigned_long (base)
CL_FORM *base;
{
   TYPE_OF(ARG(0)) = CL_C_UNSIGNED_LONG;
}

void rt_make_c_float (base)
CL_FORM *base;
{
   TYPE_OF(ARG(0)) = CL_C_FLOAT;
}

void rt_make_c_double (base)
CL_FORM *base;
{
   TYPE_OF(ARG(0)) = CL_C_DOUBLE;
}

void rt_make_c_long_double (base)
CL_FORM *base;
{
   TYPE_OF(ARG(0)) = CL_C_LONG_DOUBLE;
}

/*******************************************************************************
 * Cast Funktionen fuer Primitive.
 * Argument: object
 ******************************************************************************/
void rt_cast_c_char (base)
CL_FORM *base;
{
   TYPE_OF(ARG(0)) = CL_C_CHAR;
}

void rt_cast_c_short (base)
CL_FORM *base;
{
   TYPE_OF(ARG(0)) = CL_C_SHORT;
}

void rt_cast_c_int (base)
CL_FORM *base;
{
   TYPE_OF(ARG(0)) = CL_C_INT;
}

void rt_cast_c_long (base)
CL_FORM *base;
{
   TYPE_OF(ARG(0)) = CL_C_LONG;
}

void rt_cast_c_unsigned_char (base)
CL_FORM *base;
{
   TYPE_OF(ARG(0)) = CL_C_UNSIGNED_CHAR;
}

void rt_cast_c_unsigned_short (base)
CL_FORM *base;
{
   TYPE_OF(ARG(0)) = CL_C_UNSIGNED_SHORT;
}

void rt_cast_c_unsigned_int (base)
CL_FORM *base;
{
   TYPE_OF(ARG(0)) = CL_C_UNSIGNED_INT;
}

void rt_cast_c_unsigned_long (base)
CL_FORM *base;
{
   TYPE_OF(ARG(0)) = CL_C_UNSIGNED_LONG;
}

void rt_cast_c_float (base)
CL_FORM *base;
{
   TYPE_OF(ARG(0)) = CL_C_FLOAT;
}

void rt_cast_c_double (base)
CL_FORM *base;
{
   TYPE_OF(ARG(0)) = CL_C_DOUBLE;
}

void rt_cast_c_long_double (base)
CL_FORM *base;
{
   TYPE_OF(ARG(0)) = CL_C_LONG_DOUBLE;
}

/*******************************************************************************
 * Konvertierungsfunktionen: aus C mache Lisp
 * Argument: object
 ******************************************************************************/
void rt_make_lisp_character (base)
CL_FORM *base;
{
   TYPE_OF(ARG(0)) = CL_CHAR;
}

void rt_make_lisp_integer (base)
CL_FORM *base;
{
   TYPE_OF(ARG(0)) = CL_FIXNUM;
}

void rt_make_lisp_float (base)
CL_FORM *base;
{
   TYPE_OF(ARG(0)) = CL_FLOAT;
}

void rt_internal_make_lisp_string (base)
CL_FORM *base;
{
   char *loc = GET_C_STRING(ARG(0));
   make_string(base, loc);
}

/*------------------------------------------------------------------------------
 * 
 *----------------------------------------------------------------------------*/
void rt_internal_make_c_string (base)
CL_FORM *base;
{
   char *loc = get_c_string(ARG(0));
   size_t length = strlen(loc) + 1;
   char *ptr = (char *)malloc(length);
   (void)strcpy(ptr, loc);
   LOAD_C_STRING(ptr, ARG(0));
}

void rt_internal_copy_c_string (base)
CL_FORM *base;
{
   char *loc = GET_C_STRING(ARG(0));
   size_t length = strlen(loc) + 1;
   char *ptr = (char *)malloc(length);
   (void)strcpy(ptr, loc);
   LOAD_C_STRING(ptr, ARG(0));
}

/*******************************************************************************
 * Testfunktionen fuer Primitive
 * Argument: object
 ******************************************************************************/
void FFI_c_char_p (base)
CL_FORM *base;
{
   RET_BOOL(TYPE_OF(ARG(0)) == CL_C_CHAR);
}

void FFI_c_short_p (base)
CL_FORM *base;
{
   RET_BOOL(TYPE_OF(ARG(0)) == CL_C_SHORT ||
            TYPE_OF(ARG(0)) == CL_C_CHAR);
}

void FFI_c_int_p (base)
CL_FORM *base;
{
   RET_BOOL(TYPE_OF(ARG(0)) == CL_C_INT ||
            TYPE_OF(ARG(0)) == CL_C_SHORT ||
            TYPE_OF(ARG(0)) == CL_C_CHAR);
}

void FFI_c_long_p (base)
CL_FORM *base;
{
   RET_BOOL(TYPE_OF(ARG(0)) == CL_C_LONG ||
            TYPE_OF(ARG(0)) == CL_C_INT ||
            TYPE_OF(ARG(0)) == CL_C_SHORT ||
            TYPE_OF(ARG(0)) == CL_C_CHAR);
}

void FFI_c_unsigned_char_p (base)
CL_FORM *base;
{
   RET_BOOL(TYPE_OF(ARG(0)) == CL_C_UNSIGNED_CHAR);
}

void FFI_c_unsigned_short_p (base)
CL_FORM *base;
{
   RET_BOOL(TYPE_OF(ARG(0)) == CL_C_UNSIGNED_SHORT ||
            TYPE_OF(ARG(0)) == CL_C_UNSIGNED_CHAR);
}

void FFI_c_unsigned_int_p (base)
CL_FORM *base;
{
   RET_BOOL(TYPE_OF(ARG(0)) == CL_C_UNSIGNED_INT ||
            TYPE_OF(ARG(0)) == CL_C_UNSIGNED_SHORT ||
            TYPE_OF(ARG(0)) == CL_C_UNSIGNED_CHAR);
}

void FFI_c_unsigned_long_p (base)
CL_FORM *base;
{
   RET_BOOL(TYPE_OF(ARG(0)) == CL_C_UNSIGNED_LONG ||
            TYPE_OF(ARG(0)) == CL_C_UNSIGNED_INT ||
            TYPE_OF(ARG(0)) == CL_C_UNSIGNED_SHORT ||
            TYPE_OF(ARG(0)) == CL_C_UNSIGNED_CHAR);
}

void FFI_c_float_p (base)
CL_FORM *base;
{
   RET_BOOL(TYPE_OF(ARG(0)) == CL_C_FLOAT);
}

void FFI_c_double_p (base)
CL_FORM *base;
{
   RET_BOOL(TYPE_OF(ARG(0)) == CL_C_DOUBLE ||
            TYPE_OF(ARG(0)) == CL_C_FLOAT);
}

void FFI_c_long_double_p (base)
CL_FORM *base;
{
   RET_BOOL(TYPE_OF(ARG(0)) == CL_C_LONG_DOUBLE ||
            TYPE_OF(ARG(0)) == CL_C_DOUBLE ||
            TYPE_OF(ARG(0)) == CL_C_FLOAT);
}

void FFI_c_string_p (base)
CL_FORM *base;
{
   RET_BOOL(TYPE_OF(ARG(0)) == CL_C_STRING);
}

/*******************************************************************************
 * Testfunktionen fuer andere
 * Argumente: object typesymbol
 ******************************************************************************/
void rt_internal_c_struct_p (base)
CL_FORM *base;
{
   RET_BOOL((TYPE_OF(ARG(0)) == CL_C_STRUCT) && 
            (GET_SYMBOL(OFFSET(GET_FORM(ARG(0)), 0)) == 
             GET_SYMBOL(ARG(1))));
}

void rt_internal_c_union_p (base)
CL_FORM *base;
{
   RET_BOOL((TYPE_OF(ARG(0)) == CL_C_UNION) && 
            (GET_SYMBOL(OFFSET(GET_FORM(ARG(0)), 0)) == 
             GET_SYMBOL(ARG(1))));
}

void rt_internal_c_handle_p (base)
CL_FORM *base;
{
   RET_BOOL((TYPE_OF(ARG(0)) == CL_C_HANDLE) && 
            (GET_SYMBOL(OFFSET(GET_FORM(ARG(0)), 0)) == 
             GET_SYMBOL(ARG(1))));
}

void rt_internal_c_array_p (base)
CL_FORM *base;
{
   RET_BOOL((TYPE_OF(ARG(0)) == CL_C_ARRAY) && 
            (GET_SYMBOL(OFFSET(GET_FORM(ARG(0)), 0)) == 
             GET_SYMBOL(ARG(1))));
}

/*******************************************************************************
 * Konstruktorfunktionen
 * Argumente: typesymbol object
 ******************************************************************************/
void rt_internal_make_c_struct (base)
CL_FORM *base;
{
   CL_FORM *ptr;
   
   ptr = form_alloc(base, 3);
   LOAD_SYMBOL(GET_SYMBOL(ARG(0)), OFFSET(ptr, 0));
   LOAD_FIXNUM(ARG(2), 0, OFFSET(ptr,1));
   LOAD_CHAR_PTR(GET_C_STRUCT_PTR(ARG(1)), OFFSET(ptr, 2));
   LOAD_C_STRUCT(ptr, ARG(0));
}

void _make_c_struct_ptr (base, symbol, address)
CL_FORM *base;
CL_FORM *symbol;
char* address;
{
   CL_FORM *ptr;
   
   ptr = form_alloc(base, 3);
   LOAD_SYMBOL(symbol, OFFSET(ptr, 0));
   LOAD_FIXNUM(ARG(0), 0, OFFSET(ptr, 1));
   LOAD_CHAR_PTR(address, OFFSET(ptr, 2));
   LOAD_C_STRUCT(ptr, ARG(0));
}

void rt_internal_make_c_union (base)
CL_FORM *base;
{
   CL_FORM *ptr;
   
   ptr = form_alloc(base, 3);
   LOAD_SYMBOL(GET_SYMBOL(ARG(0)), OFFSET(ptr, 0));
   LOAD_FIXNUM(ARG(2), 0, OFFSET(ptr,1));
   LOAD_CHAR_PTR(GET_C_UNION_PTR(ARG(1)), OFFSET(ptr, 2));
   LOAD_C_UNION(ptr, ARG(0));
}

void _make_c_union_ptr (base, symbol, address)
CL_FORM *base;
CL_FORM *symbol;
char* address;
{
   CL_FORM *ptr;
   
   ptr = form_alloc(base, 3);
   LOAD_SYMBOL(symbol, OFFSET(ptr, 0));
   LOAD_FIXNUM(ARG(0), 0, OFFSET(ptr, 1));
   LOAD_CHAR_PTR(address, OFFSET(ptr, 2));
   LOAD_C_UNION(ptr, ARG(0));
}

void rt_internal_make_c_array (base)
CL_FORM *base;
{
   CL_FORM *ptr;
   
   ptr = form_alloc(base, 3);
   LOAD_SYMBOL(GET_SYMBOL(ARG(0)), OFFSET(ptr, 0));
   LOAD_FIXNUM(ARG(2), 0, OFFSET(ptr,1));
   LOAD_CHAR_PTR(GET_C_ARRAY_PTR(ARG(1)), OFFSET(ptr, 2));
   LOAD_C_ARRAY(ptr, ARG(0));
}

void _make_c_array_ptr (base, symbol, address)
CL_FORM *base;
CL_FORM *symbol;
char* address;
{
   CL_FORM *ptr;
   
   ptr = form_alloc(base, 3);
   LOAD_SYMBOL(symbol, OFFSET(ptr, 0));
   LOAD_FIXNUM(ARG(0), 0, OFFSET(ptr, 1));
   LOAD_CHAR_PTR(address, OFFSET(ptr, 2));
   LOAD_C_ARRAY(ptr, ARG(0));
}

void _make_c_handle (base, symbol, address)
CL_FORM *base;
CL_FORM *symbol;
char * address;
{
   CL_FORM *ptr;
   
   ptr = form_alloc(base, 3);
   LOAD_SYMBOL(symbol, OFFSET(ptr, 0));
   LOAD_FIXNUM(ARG(0), 0, OFFSET(ptr, 1));
   LOAD_CHAR_PTR(address, OFFSET(ptr, 2));
   LOAD_C_HANDLE(ptr, ARG(0));
}

/*******************************************************************************
 * Kopierfunktionen
 * Argumente: typesymbol object old-object size
 ******************************************************************************/
void rt_internal_copy_c_struct (base)
CL_FORM *base;
{
   CL_FORM *ptr;
   
   ptr = form_alloc(base, 3);
   LOAD_SYMBOL(GET_SYMBOL(ARG(0)), OFFSET(ptr, 0));
   LOAD_FIXNUM(ARG(4), 0, OFFSET(ptr,1));
   LOAD_CHAR_PTR(GET_C_STRUCT_PTR(ARG(1)), OFFSET(ptr, 2));
   memcpy(GET_C_STRUCT_PTR(ARG(1)), 
          GET_CHAR_PTR(OFFSET(ARG(2), 2)), 
          GET_C_INT(ARG(3)));
   LOAD_C_STRUCT(ptr, ARG(0));
}

void rt_internal_copy_c_union (base)
CL_FORM *base;
{
   CL_FORM *ptr;
   
   ptr = form_alloc(base, 3);
   LOAD_SYMBOL(GET_SYMBOL(ARG(0)), OFFSET(ptr, 0));
   LOAD_FIXNUM(ARG(4), 0, OFFSET(ptr,1));
   LOAD_CHAR_PTR(GET_C_UNION_PTR(ARG(1)), OFFSET(ptr, 2));
   memcpy(GET_C_UNION_PTR(ARG(1)), 
          GET_CHAR_PTR(OFFSET(ARG(2), 2)), 
          GET_C_INT(ARG(3)));
   LOAD_C_UNION(ptr, ARG(0));
}

void rt_internal_copy_c_array (base)
CL_FORM *base;
{
   CL_FORM *ptr;
   
   ptr = form_alloc(base, 3);
   LOAD_SYMBOL(GET_SYMBOL(ARG(0)), OFFSET(ptr, 0));
   LOAD_FIXNUM(ARG(4), 0, OFFSET(ptr,1));
   LOAD_CHAR_PTR(GET_C_ARRAY_PTR(ARG(1)), OFFSET(ptr, 2));
   memcpy(GET_C_ARRAY_PTR(ARG(1)), 
          GET_CHAR_PTR(OFFSET(ARG(2), 2)), 
          GET_C_INT(ARG(3)));
   LOAD_C_ARRAY(ptr, ARG(0));
}

/*******************************************************************************
 * Zeiger holen
 * Argument: typesymbol object
 ******************************************************************************/
void rt_internal_get_struct_pointer (base)
CL_FORM *base;
{
   CL_FORM *ptr;
   
   ptr = form_alloc(base, 3);
   LOAD_SYMBOL(GET_SYMBOL(ARG(0)), OFFSET(ptr, 0));
   LOAD_FIXNUM(ARG(2), 0, OFFSET(ptr,1));
   LOAD_CHAR_PTR(GET_C_STRUCT_PTR(ARG(1)), OFFSET(ptr, 2));
   LOAD_C_STRUCT(ptr, ARG(0));
}

void rt_internal_get_union_pointer (base)
CL_FORM *base;
{
   CL_FORM *ptr;
   
   ptr = form_alloc(base, 3);
   LOAD_SYMBOL(GET_SYMBOL(ARG(0)), OFFSET(ptr, 0));
   LOAD_FIXNUM(ARG(2), 0, OFFSET(ptr,1));
   LOAD_CHAR_PTR(GET_C_UNION_PTR(ARG(1)), OFFSET(ptr, 2));
   LOAD_C_UNION(ptr, ARG(0));
}

void rt_internal_get_array_pointer (base)
CL_FORM *base;
{
   CL_FORM *ptr;
   
   ptr = form_alloc(base, 3);
   LOAD_SYMBOL(GET_SYMBOL(ARG(0)), OFFSET(ptr, 0));
   LOAD_FIXNUM(ARG(2), 0, OFFSET(ptr,1));
   LOAD_CHAR_PTR(GET_C_ARRAY_PTR(ARG(1)), OFFSET(ptr, 2));
   LOAD_C_ARRAY(ptr, ARG(0));
}

/*******************************************************************************
 * Fuer die Ausgabe eine Einfache Form
 * Argument: object
 ******************************************************************************/
void c_struct_p (base)
CL_FORM *base;
{
   RET_BOOL(TYPE_OF(ARG(0)) == CL_C_STRUCT);
}

void c_union_p (base)
CL_FORM *base;
{
   RET_BOOL(TYPE_OF(ARG(0)) == CL_C_UNION);
}

void c_array_p (base)
CL_FORM *base;
{
   RET_BOOL(TYPE_OF(ARG(0)) == CL_C_ARRAY);
}

void c_handle_p (base)
CL_FORM *base;
{
   RET_BOOL(TYPE_OF(ARG(0)) == CL_C_HANDLE);
}

/*------------------------------------------------------------------------------
 * 
 *----------------------------------------------------------------------------*/
void rt_internal_get_symbol (base)
CL_FORM *base;
{
   LOAD_SYMBOL(GET_SYMBOL(OFFSET(GET_FORM(ARG(0)), 0)), ARG(0));
}

void rt_internal_get_address (base)
CL_FORM *base;
{
   LOAD_FIXNUM(ARG(1), (long)GET_CHAR_PTR(OFFSET(GET_FORM(ARG(0)), 2)), ARG(0));
}

/*------------------------------------------------------------------------------
 * 
 *----------------------------------------------------------------------------*/
void FFI_free (base)
CL_FORM *base;
{
   int type = TYPE_OF(ARG(0));
   
   switch (type) 
   {
   case CL_C_STRUCT:
   case CL_C_UNION:
   case CL_C_ARRAY:
      free((void *)GET_CHAR_PTR(OFFSET(GET_FORM(ARG(0)), 2)));
      LOAD_NIL(ARG(0));
      break;
   case CL_C_STRING:
      free((void*)GET_CHAR_PTR(ARG(0)));
      LOAD_NIL(ARG(0));
      break;
   }
}
