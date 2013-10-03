;;;-----------------------------------------------------------------------------
;;; Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel, Germany
;;;-----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Funktion : Konstante benannte Strings
;;;
;;; $Revision: 1.7 $
;;; $Log: strconst.lisp,v $
;;; Revision 1.7  1994/02/21  09:26:02  kl
;;; Neue Konstante TOO_MUCH_ARGS eingeführt.
;;;
;;; Revision 1.6  1993/06/17  08:00:09  hk
;;; Copright Notiz eingefuegt
;;;
;;; Revision 1.5  1993/02/16  16:11:41  hk
;;; Revision Keyword eingefuegt.
;;;
;;; Revision 1.4  1992/09/08  13:26:49  ft
;;; Fehlermeldung fuer Klasses-Redefinition eingefuegt.
;;;
;;; Revision 1.3  1992/08/07  11:34:35  hk
;;; Dateikopf verschoenert.
;;;
;;; Revision 1.2  1992/06/17  08:03:37  ft
;;; Fehlermeldungen um REDEF_GEN-FUN (Neudefinition einer bestehenden
;;; (importierten) generischen Funktion) erweitert.
;;;
;;; Revision 1.1  1992/03/24  16:54:56  hk
;;; Initial revision
;;;-----------------------------------------------------------------------------

(in-package "CLICC")

;;------------------------------------------------------------------------------
;; Konstanten des Programms
;;------------------------------------------------------------------------------

;;------------------------------------------------------------------------------
;; Fehlermeldungen
;;------------------------------------------------------------------------------
(defconstant NOT_IMPLEMENTED    "~S not implemented.")
(defconstant TYPE_NOT_IMPL      "Data type for ~S not implemented.")
(defconstant NOT_ALLOWED        "Expression not allowed at this position")

(defconstant NO_OPERATOR        "Unknown operator ~S")
(defconstant ILLEGAL_CALL       "The argument list of a call to ~A does not ~
                                 match the lambda list ~A")
(defconstant ILL_FORMED         "The ~S ~S is not of the form ~S")
(defconstant IFD_BODY           "~S is an ill-formed body.")
(defconstant NOT_ENOUGH_ARGS    "A call to ~A needs at least ~D arguments.")
(defconstant TOO_MUCH_ARGS      "A call to ~A should have at most ~D ~
                                 arguments.")

;;------------------------------------------------------------------------------
;; Fehlermeldungen: Top-Level-Forms
;;------------------------------------------------------------------------------
(defconstant TLF_IGNORED        "Top-level-form ~S ignored.")
(defconstant NO_GLOB_VAL        "~S has no global value")
(defconstant NO_GLOB_FUNDEF     "Function ~30S is undefined.")

;;------------------------------------------------------------------------------
;; Fehlermeldungen: Definition einer Funktion/ eines Makros /einer Klasse
;;------------------------------------------------------------------------------
(defconstant ILLEGAL_L_EXPR     "The argument list of a LAMBDA-EXPRESSION does ~
                                 not match ~
                                 the lambda list ((&REST ARGLIST) &BODY BODY)")
(defconstant NO_NAME            "The name ~S in ~S must be a symbol.")
(defconstant REDEF_SPF          "It is illegal to redefine the special form ~S")
(defconstant REDEF_MACRO        "Redefining function ~S which used ~
                                 to be a macro")
(defconstant REDEF_FUN          "Redefining build-in function ~S")
(defconstant REDEF              "Redefining ~S which was defined before.")
(defconstant REDEF_GEN-FUN      "It is illegal to redefine the generic ~
                                 function ~S")
(defconstant REDEF_CLASS        "It is illegal to redefine the class ~S")
                                 
;;------------------------------------------------------------------------------
;; Fehlermeldungen: Analyse der LAMBDA-Liste
;;------------------------------------------------------------------------------
(defconstant IFD_LAMBDA_LIST    "~S is an ill-formed lambda-list~%")
(defconstant IFD_LL_ELEM        "The lambda list element ~S is ill-formed.~%~
                                 It must be of the form: ~S")
(defconstant ILLEGAL_LLKW       "~S is an illegal lambda-list keyword.")

(defconstant DECLARED_TWICE     "The variable ~S occurs more than once in it.")
(defconstant DECLARED_CONSTANT  "~S was declared with DEFCONSTANT.")
(defconstant IS_KEYWORD         "~S is keyword, no variable.")
(defconstant NO_VAR             "Expected to see a variable instead of ~S")

(defconstant NO_OPTIONALS       "No optionals specified after &OPTIONAL")
(defconstant POS_ERR_OPTIONAL   "&OPTIONAL seen after optionals already ~
                                 processed.")

(defconstant NO_REST_VAR        "No variable follows &REST keyword.")
(defconstant SEC_REST_VAR       "Second variable seen after &REST.")
(defconstant POS_ERR_REST       "&REST seen after rest argument already ~
                                 processed.")

(defconstant NO_KEYS            "No keys specified after &KEY")
(defconstant POS_ERR_KEY        "&KEY seen after key arguments already ~
                                 processed.")

(defconstant POS_ERR_AOK        "&ALLOW-OTHER-KEYS has to immediately follow ~
                                 key arguments.")
(defconstant ILLEGAL_AOKP       "After &ALLOW-OTHER-KEYS no parameter allowed.
                                 Expected to see a keyword instead of ~S")

(defconstant NO_AUX             "No aux-parameter specified after &AUX")

(defconstant SPECIAL_DECL_TWICE "Variable ~S is declared SPECIAL twice")

;;------------------------------------------------------------------------------
;; Fehlermeldungen: Special-Forms
;;------------------------------------------------------------------------------
(defconstant NO_MATCH_SF        "The argument list does not match lambda list ~
                                 ~A of ~A")

(defconstant CONST_VAL          "~S has constant value and cannot be changed ~
                                 to ~A")
(defconstant CLOS_OF_MACRO      "The macro ~S is not a valid argument to ~
                                 FUNCTION.")
(defconstant IFD_BINDING        "The binding ~S is ill-formed.~%~
                                 It must be of the form: (VAR | (VAR VALUE))")
(defconstant NO_LEGAL_BLOCKNAME "~S should be a legal block name")
(defconstant TAG_NOT_USED       "Tag ~S in TAGBODY not used")

;;------------------------------------------------------------------------------
;; Fehlermeldungen: Makro-Expansion
;;------------------------------------------------------------------------------
(defconstant NO_LIST_END        "~S is not a legal list end")
(defconstant NO_LEGAL_CLAUSE    "~S is not a legal clause for a ~S form")
(defconstant ODD_NARGS          "Odd number of args to ~S")
(defconstant NO_GET-SETF-METHOD "~S illegal atomic form for GET-SETF-METHOD")
(defconstant NO_LOC_SPEC        "~S is not a known location specifier for SETF")
(defconstant TOO_FEW_ARGS       "Too few argument forms to ~S")

(defconstant ILLEGAL_KEY        "Expected one of the keywords ~S instead of ~S ~
                                 in the keyword portion of a call")
(defconstant ODD_LEN_KEYLIST    "Unpaired item in keyword portion of an ~
                                 argument list")

;;------------------------------------------------------------------------------
;; Fehlermeldungen: System-Funktionen
;;------------------------------------------------------------------------------
(defconstant NARGS_NC           "~S cannot be called with ~D arguments")
(defconstant NO_SYMBOL          "~S should be of type SYMBOL")
(defconstant NO_KEYWORD         "~S should be of type KEYWORD")
(defconstant NO_LIST            "~S should be of type LIST")
(defconstant NO_STRING          "~S should be of type STRING")

(defconstant NOT_A_SYMBOL       "The value of ~A, ~S, should be a symbol.")
(defconstant IFD_DECL_SPEC      "~S is an ill-formed declaration specification")

;;------------------------------------------------------------------------------
(provide "strconst")
