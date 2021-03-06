/* This file was generated by CLiCC 0.6.4 [obrep 1] */
#define __OBREP 1
#include <c_decl.h>
#include "sys.h"
#include "lisp.h"

CL_INIT KClisp[] =
{
	MAKE_STRING(41, "The value of OTHER, ~S, should be a FLOAT"),	/* 0 */
	MAKE_STRING(26, "undefined Stream Operation"),	/* 2 */
	MAKE_STRING(50, "The evaluated value ~S is not of type c-<integer>."),	/* 4 */
	MAKE_STRING(12, "LISP-INTEGER"),	/* 6 */
	MAKE_STRING(47, "The evaluated value ~S is not of type c-<char>."),	/* 8 */
	MAKE_STRING(14, "LISP-CHARACTER"),	/* 10 */
	MAKE_STRING(47, "The evaluated value ~S is not of type c-string."),	/* 12 */
	MAKE_STRING(13, "MAKE-C-STRING"),	/* 14 */
	MAKE_STRING(45, "The evaluated value ~S is not of type string."),	/* 16 */
	MAKE_STRING(13, "MAKE-C-STRING"),	/* 18 */
	MAKE_STRING(44, "The evaluated value ~S is not of type float."),	/* 20 */
	MAKE_STRING(13, "C-LONG-DOUBLE"),	/* 22 */
	MAKE_STRING(44, "The evaluated value ~S is not of type float."),	/* 24 */
	MAKE_STRING(8, "C-DOUBLE"),	/* 26 */
	MAKE_STRING(44, "The evaluated value ~S is not of type float."),	/* 28 */
	MAKE_STRING(7, "C-FLOAT"),	/* 30 */
	MAKE_STRING(45, "The evaluated value ~S is not of type fixnum."),	/* 32 */
	MAKE_STRING(15, "C-UNSIGNED-LONG"),	/* 34 */
	MAKE_STRING(45, "The evaluated value ~S is not of type fixnum."),	/* 36 */
	MAKE_STRING(14, "C-UNSIGNED-INT"),	/* 38 */
	MAKE_STRING(45, "The evaluated value ~S is not of type fixnum."),	/* 40 */
	MAKE_STRING(16, "C-UNSIGNED-SHORT"),	/* 42 */
	MAKE_STRING(48, "The evaluated value ~S is not of type character."),	/* 44 */
	MAKE_STRING(15, "C-UNSIGNED-CHAR"),	/* 46 */
	MAKE_STRING(45, "The evaluated value ~S is not of type fixnum."),	/* 48 */
	MAKE_STRING(6, "C-LONG"),	/* 50 */
	MAKE_STRING(45, "The evaluated value ~S is not of type fixnum."),	/* 52 */
	MAKE_STRING(5, "C-INT"),	/* 54 */
	MAKE_STRING(45, "The evaluated value ~S is not of type fixnum."),	/* 56 */
	MAKE_STRING(7, "C-SHORT"),	/* 58 */
	MAKE_STRING(48, "The evaluated value ~S is not of type character."),	/* 60 */
	MAKE_STRING(6, "C-CHAR"),	/* 62 */
	MAKE_STRING(1, "T"),	/* 64 */
	MAKE_STRING(42, "It could not be generated a string from ~S"),	/* 66 */
	MAKE_STRING(0, ""),	/* 68 */
	MAKE_STRING(0, ""),	/* 70 */
	MAKE_STRING(0, ""),	/* 72 */
	MAKE_STRING(44, "etypecase: the value ~a is not a legal value"),	/* 74 */
	MAKE_STRING(16, "~a is not a list"),	/* 76 */
	MAKE_STRING(15, "~&Warning: ~?~%"),	/* 78 */
	MAKE_STRING(13, "~&Error: ~?~%"),	/* 80 */
	MAKE_STRING(33, "illegal destination ~S for format"),	/* 82 */
	MAKE_STRING(35, "(SUBTYPEP ~S ~S) is not implemented"),	/* 84 */
	MAKE_STRING(32, "(TYPEP ~S ~S) is not implemented"),	/* 86 */
	MAKE_SYMREF(SYMBOL(Slisp, 384)),	/* 88 CONS(88) INTERNAL */
	MAKE_CONSREF(&KClisp[90]),
	MAKE_SYMREF(SYMBOL(Slisp, 385)),	/* CONS(90) EXTERNAL */
	MAKE_NIL,
	MAKE_STRING(48, "Isqrt: ~S argument must be a nonnegative integer"),	/* 92 */
	MAKE_STRING(2, "./"),	/* 94 */
	MAKE_STRING(2, "./"),	/* 96 */
	MAKE_STRING(4, "/../"),	/* 98 */
	MAKE_STRING(18, "Unknown error [~d]"),	/* 100 */
	MAKE_STRING(25, "Error reading link ~S: ~S"),	/* 102 */
	MAKE_STRING(1, "/"),	/* 104 */
	MAKE_STRING(1, "/"),	/* 106 */
	MAKE_STRING(44, "etypecase: the value ~a is not a legal value"),	/* 108 */
	MAKE_STRING(44, "etypecase: the value ~a is not a legal value"),	/* 110 */
	MAKE_STRING(65, "Cannot determine the namestring for pathnames with no host:~%  ~S"),	/* 112 */
	MAKE_STRING(44, "etypecase: the value ~a is not a legal value"),	/* 114 */
	MAKE_STRING(65, "Cannot determine the namestring for pathnames with no host:~%  ~S"),	/* 116 */
	MAKE_STRING(44, "etypecase: the value ~a is not a legal value"),	/* 118 */
	MAKE_STRING(65, "Cannot determine the namestring for pathnames with no host:~%  ~S"),	/* 120 */
	MAKE_STRING(44, "etypecase: the value ~a is not a legal value"),	/* 122 */
	MAKE_STRING(65, "Cannot determine the namestring for pathnames with no host:~%  ~S"),	/* 124 */
	MAKE_STRING(44, "etypecase: the value ~a is not a legal value"),	/* 126 */
	MAKE_STRING(44, "etypecase: the value ~a is not a legal value"),	/* 128 */
	MAKE_STRING(44, "etypecase: the value ~a is not a legal value"),	/* 130 */
	MAKE_STRING(17, "illegal option ~s"),	/* 132 */
	MAKE_STRING(19, "cannot coerce to ~S"),	/* 134 */
	MAKE_STRING(58, "No applicable method for generic function ~s with args ~s."),	/* 136 */
	MAKE_STRING(15, "No next method."),	/* 138 */
	MAKE_SYMREF(SYMBOL(Slisp, 210)),	/* 140 CONS(140) SETF */
	MAKE_CONSREF(&KClisp[142]),
	MAKE_SYMREF(SYMBOL(Slisp, 200)),	/* CONS(142) SLOT-VALUE */
	MAKE_NIL,
	MAKE_STRING(22, "~S is not an instance."),	/* 144 */
	MAKE_STRING(33, "~S is not a valid class-argument."),	/* 146 */
	MAKE_STRING(113, "The computed symbol ~S is not a valid class-argument, ~\n                   these have been restricted to classes."),	/* 148 */
	MAKE_STRING(6, "Rubout"),	/* 150 */
	MAKE_STRING(6, "Return"),	/* 152 */
	MAKE_STRING(4, "Page"),	/* 154 */
	MAKE_STRING(8, "Linefeed"),	/* 156 */
	MAKE_STRING(3, "Tab"),	/* 158 */
	MAKE_STRING(9, "Backspace"),	/* 160 */
	MAKE_STRING(7, "Newline"),	/* 162 */
	MAKE_STRING(5, "Space"),	/* 164 */
	MAKE_STRING(6, "Rubout"),	/* 166 */
	MAKE_STRING(6, "Return"),	/* 168 */
	MAKE_STRING(4, "Page"),	/* 170 */
	MAKE_STRING(8, "Linefeed"),	/* 172 */
	MAKE_STRING(3, "Tab"),	/* 174 */
	MAKE_STRING(9, "Backspace"),	/* 176 */
	MAKE_STRING(7, "Newline"),	/* 178 */
	MAKE_STRING(5, "Space"),	/* 180 */
	MAKE_STRING(34, "cannot coerce ~S to type character"),	/* 182 */
	MAKE_STRING(42, "The fill pointer of the vector ~A is zero."),	/* 184 */
	MAKE_STRING(101, "The arg ~S given to SETF of FILL-POINTER is not in ~\n            range for an array of total size ~S."),	/* 186 */
	MAKE_STRING(39, "Wrong number of subscripts for array ~a"),	/* 188 */
	MAKE_STRING(16, "Unexpected error"),	/* 190 */
	MAKE_STRING(86, "The value ~A is not suitable as an axis-number ~\n            for an array with rank ~A"),	/* 192 */
	MAKE_STRING(15, "stream expected"),	/* 194 */
	MAKE_SYMREF(SYMBOL(Slisp, 91)),	/* 196 CONS(196) FILE-OUTPUT */
	MAKE_CONSREF(&KClisp[198]),
	MAKE_SYMREF(SYMBOL(Slisp, 92)),	/* CONS(198) FILE-IO */
	MAKE_CONSREF(&KClisp[200]),
	MAKE_SYMREF(SYMBOL(Slisp, 94)),	/* CONS(200) BROADCAST */
	MAKE_CONSREF(&KClisp[202]),
	MAKE_SYMREF(SYMBOL(Slisp, 96)),	/* CONS(202) TWO-WAY */
	MAKE_CONSREF(&KClisp[204]),
	MAKE_SYMREF(SYMBOL(Slisp, 97)),	/* CONS(204) ECHO */
	MAKE_CONSREF(&KClisp[206]),
	MAKE_SYMREF(SYMBOL(Slisp, 102)),	/* CONS(206) STRING-OUTPUT */
	MAKE_NIL,
	MAKE_SYMREF(SYMBOL(Slisp, 90)),	/* 208 CONS(208) FILE-INPUT */
	MAKE_CONSREF(&KClisp[210]),
	MAKE_SYMREF(SYMBOL(Slisp, 92)),	/* CONS(210) FILE-IO */
	MAKE_CONSREF(&KClisp[212]),
	MAKE_SYMREF(SYMBOL(Slisp, 95)),	/* CONS(212) CONCATENATED */
	MAKE_CONSREF(&KClisp[214]),
	MAKE_SYMREF(SYMBOL(Slisp, 96)),	/* CONS(214) TWO-WAY */
	MAKE_CONSREF(&KClisp[216]),
	MAKE_SYMREF(SYMBOL(Slisp, 97)),	/* CONS(216) ECHO */
	MAKE_CONSREF(&KClisp[218]),
	MAKE_SYMREF(SYMBOL(Slisp, 98)),	/* CONS(218) STRING-INPUT */
	MAKE_NIL,
	MAKE_STRING(47, "The evaluated value ~S is not of type c-string."),	/* 220 */
	MAKE_STRING(16, "MAKE-LISP-STRING"),	/* 222 */
	MAKE_STRING(48, "The evaluated value ~S is not of type c-<float>."),	/* 224 */
	MAKE_STRING(10, "LISP-FLOAT"),	/* 226 */
	MAKE_STRING(16, "~a is not a list"),	/* 228 */
	MAKE_STRING(22, "extra argument for #~S"),	/* 230 */
	MAKE_STRING(39, "~S cannot be represented relative to ~S"),	/* 232 */
	MAKE_STRING(58, "~S: The slot ~s is missing from the object ~s of class ~s."),	/* 234 */
	MAKE_STRING(52, "The slot ~s is unbound in the object ~s of class ~s."),	/* 236 */
	MAKE_STRING(122, "~S ist not a valid argument for CLASS-OF, ~\n              these have been restricted to instances of user-defined-classes."),	/* 238 */
	MAKE_STRING(19, "~&Error in ~A: ~?~%"),	/* 240 */
	MAKE_STRING(27, "The file ~S does not exist."),	/* 242 */
	MAKE_STRING(32, "type error: ~S is not of type ~S"),	/* 244 */
	MAKE_STRING(0, ""),	/* 246 */
	MAKE_STRREF(&KClisp[250]),	/* 248 CONS(248) */
	MAKE_NIL,
	MAKE_STRING(4, "LISP"),
	MAKE_STRING(16, "~a is not a cons"),	/* 252 */
	MAKE_STRING(16, "~a is not a cons"),	/* 254 */
	MAKE_STRING(60, "~S should be an INTEGER at least 0 and no more than 16777214"),	/* 256 */
	MAKE_STRING(16, "~a is not a cons"),	/* 258 */
	MAKE_STRING(16, "~a is not a cons"),	/* 260 */
	MAKE_STRING(16, "~a is not a list"),	/* 262 */
	MAKE_STRING(16, "~a is not a list"),	/* 264 */
	MAKE_STRING(3, "NIL"),	/* 266 */
	MAKE_STRING(29, "string-output-stream expected"),	/* 268 */
};
