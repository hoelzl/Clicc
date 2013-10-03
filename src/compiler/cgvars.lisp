;;;-----------------------------------------------------------------------------
;;; Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel, Germany
;;;-----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Funktion : Codegenerierung
;;;            - Lambda Listen
;;;            - Variablenzugriffe
;;;            - Binden von Variablen
;;;            - Restaurieren von Special Variablen
;;;            - SETQ
;;;
;;; $Revision: 1.36 $
;;; $Log: cgvars.lisp,v $
;;; Revision 1.36  1994/05/25  14:06:18  sma
;;; Aufruf der Restlistenoptimierung aus cg-params herausgezogen.
;;;
;;; Revision 1.35  1994/05/24  09:34:43  sma
;;; Berechnung der local-Variable bei closures korrigiert.
;;;
;;; Revision 1.34  1994/05/19  13:27:17  hk
;;; Klammerfehler behoben.
;;;
;;; Revision 1.33  1994/05/19  13:13:26  sma
;;; Closures-Argument bei Closures im Zusammenhang mit Restlisten wird
;;; nicht mehr von Restargument verdeckt.
;;;
;;; Revision 1.32  1994/04/14  17:01:44  sma
;;; Fehler in gc-form Methode für let*-form beim gemischten Vorkommen von
;;; Rest-Variablen und normalen Variablen behoben.
;;;
;;; Revision 1.31  1994/04/11  12:44:38  sma
;;; Korrektur für rest-variablen, die auf Prädikatsposition stehen.
;;;
;;; Revision 1.30  1994/03/03  13:47:14  jh
;;; defined- und imported-named-consts werden jetzt unterschieden.
;;;
;;; Revision 1.29  1994/02/10  16:00:30  sma
;;; Korrektur für closures; cdr-rest-funcall-p -> rlo-rest-form;
;;; Erweiterung für (setq rest-var (let () ... (progn restform))).
;;;
;;; Revision 1.28  1994/02/08  15:39:24  sma
;;; Statistik verändert und Test auf Benutzung der rest-Variable in
;;; rest-optimization-p verschoben.
;;;
;;; Revision 1.27  1994/02/08  13:58:55  sma
;;; Diverse Änderungen für rest-Parameter-Optimierungen.
;;;
;;; Revision 1.26  1994/02/03  17:33:50  sma
;;; Änderungen für Optimierung von &rest-Paramtern. Wenn möglich, wird das
;;; Erzeugen einer Restliste mittels Flist unterdrückt und die Restliste
;;; direkt auf dem LISP-Stack verwaltet.
;;;
;;; Revision 1.25  1994/01/07  12:51:06  hk
;;; Unbenutzte local-static haben die Annotation write = 1, wenn sie nicht
;;; benutzt werden. Das wird nun berücksichtigt, um unbenutzte &rest
;;; Parameter zu erkennen.
;;;
;;; Revision 1.24  1993/09/10  15:33:24  hk
;;; Fehler in cg-form (var-ref) behoben
;;;
;;; Revision 1.23  1993/09/10  12:16:14  hk
;;; Vorhergehende Korrektur korrigiert.
;;;
;;; Revision 1.22  1993/09/10  10:05:41  hk
;;; Fehler behoben: Bearbeitung optionaler Parameter hat vorausgesetzt,
;;; daß die Argumentliste immer mit Offset 0 im Activation-Record liegt.
;;; Das ist bei der Bearbeitung von mv-lambda nicht unbedingt gegeben.
;;;
;;; Revision 1.21  1993/09/06  14:33:00  hk
;;; Fehler in (cg-form var-ref) behoben: bei Zugriff auf dynamisch
;;; gebundene Variablen auf Prädikatsposition eines if.
;;;
;;; Revision 1.20  1993/07/20  15:46:27  hk
;;; (cg-form named-const) ist nun definiert, falls der Wert ein Literal
;;; oder ein Symbol ist.
;;;
;;; Revision 1.19  1993/06/28  15:29:18  hk
;;; Fehler in CC-heapenv behoben und die Funktion vereinfacht.
;;;
;;; Revision 1.18  1993/06/17  08:00:09  hk
;;; Copright Notiz eingefuegt
;;;
;;; Revision 1.17  1993/05/12  15:01:33  hk
;;; Sonderbehandlung fuer Vorwaertsreferenzen auf Funktionen entfernt.
;;;
;;; Revision 1.16  1993/03/22  17:36:22  hk
;;; Keywords in LZS Slots.
;;;
;;; Revision 1.15  1993/02/16  15:48:45  hk
;;; Revision Keyword eingefuegt.
;;;
;;; Revision 1.14  1992/12/21  11:29:41  hk
;;; Schreibfehler in der letzten Aenderung.
;;;
;;; Revision 1.13  1992/12/21  11:25:48  hk
;;; ?read von nicht aufgeloesten Referenzen auf undefinierte Funktionen
;;; wird auf -1 gesetzt, um anzuzeigen, dass keine erneute Fehlermeldung
;;; ausgegeben zu werden braucht.
;;;
;;; Revision 1.12  1992/10/02  14:07:22  hk
;;; Fehlerbehandlung jetzt lokal
;;;
;;; Revision 1.11  1992/09/24  15:27:46  hk
;;; Fehler bei der Bearbeitung von Special-Parametern behoben.
;;;
;;; Revision 1.10  1992/09/23  12:31:59  hk
;;; Fehler bei der Bearbeitung von Key-supplied-p Variablen behoben.
;;;
;;; Revision 1.9  1992/09/21  11:18:52  hk
;;; Die eigentliche C-Codegenerierung uebersichtlicher gestaltet
;;;
;;; Revision 1.8  1992/08/11  12:47:53  hk
;;; C-Ln --> C-Decl, fuer Deklarationen.
;;;
;;; Revision 1.7  1992/08/10  12:04:25  hk
;;; *mv-spec* und *mv-produced* gestrichen, da Analyse komplett in Pass3.
;;;
;;; Revision 1.6  1992/07/09  16:48:34  hk
;;; Vernuenftige Fehlermeldung bei nicht aufgeloesten Forwarts-Referenzen.
;;;
;;; Revision 1.5  1992/07/06  15:00:47  hk
;;; In cg-var-bind (dynamic) wird die variable nur noch dann mit
;;; *result-spec* verglichen, wenn es ungleich nil ist.
;;;
;;; Revision 1.4  1992/06/04  14:44:34  hk
;;; Schreibfehler in Bearbeitung von setq-form.
;;;
;;; Revision 1.3  1992/06/04  12:59:17  hk
;;; Auf Rumpf von Let* wird mittels ?body und nicht mehr ?form zugegriffen.
;;;
;;; Revision 1.2  1992/06/04  07:11:20  hk
;;; Nach Umstellung auf die Lisp nahe Zwischensprache, Syntax-Fehler
;;; sind schon beseitigt
;;;
;;; Revision 1.1  1992/03/24  16:54:56  hk
;;; Initial revision
;;;-----------------------------------------------------------------------------

(in-package "CLICC")

;;------------------------------------------------------------------------------
;; Codegenerierung fuer Lambda-Listen von Funktionen und von mv-lambda
;;------------------------------------------------------------------------------
;; 
;; Lage der Parameter im Activation-Record:
;; 1. normale Parameter, 
;; 2. opt. supplied Parameter, 
;; 3. optionale Parameter, 
;; 4. Key-Parameter, 
;; 5. Rest-Parameter,
;; 6. Key supplied Parameter
;;------------------------------------------------------------------------------
;; EINSCHRAENKUNG: kein Identifikator darf in einer Lambda-Liste mehrfach
;; gebunden werden.
;; ACHTUNG: Bei Closures beginnt der 1. Parameter nicht an der Position 0 im
;; Aktivation Record, sondern dort befindet sich Zeiger auf die Closure.
;;------------------------------------------------------------------------------
;; Bedeutung von Offset bei Parametern und lokalen Variablen (local-statics)
;; bei Rest-Optimierung: In Offset wird ein Typ kodiert, der theoretisch auch
;; explizit in jede local-static eingetragen werden könnte.
;;   .----------------------.
;;   | Typ | Level | Offset |
;;   `----------------------'
;;      |     n>=0    n>=0 
;;      V
;;    ARG      Argumente der Funktion
;;    LOCAL    Lokale Variablen/Zwischenergebnisse (nur bei rest-optimierung)
;;    REST     Rest-Variablen
;;    FRESULT  Funktionsergebnis
;;
;; + Das Funktionsergebnis ist immer an Position 0 (Konvention).
;; + Hat die Funktion mindestens ein Argument, welches keine restvariable ist,
;;   fällt das Funktionsergebnis mit diesem Argument zusammen.
;;
;; Folgende Kodierung wird eingesetzt:
;;   rest_opt := MAX(1, nsimpopts)
;;   ARG      >= 0 & < rest_opt
;;   LOCAL    >= rest_opt
;;   REST     < 0
;;   FRESULT  = 0
;;------------------------------------------------------------------------------
(defun cg-params (params known-number-of-args rlo)
  (let* ((nsimple       (length (?var-list params)))
         (nopt          (length (?opt-list params)))
         (nkey          (length (?key-list params)))
         (nsimpopt      (+ nsimple nopt))
         (rest          (?rest params))
         nopt-supplied
         suppl-index
         first-key 
         C-stacktop
         (closure-offset *stack-top*))

    ;; Während der Parametererstellung ausschalten
    ;;--------------------------------------------
    (setq *rest-optimization* nil)
    (when rlo
      (setq *rest-var-count* 0)
      (cg-var-bind rest (decf *rest-var-count*))
      (C-PtrDecl "CL_FORM" (CC-restvar (?offset rest)))
      (C-PtrDecl "CL_FORM" C-local))
    
    (setq C-stacktop
          (CC-Stack (if (not known-number-of-args)
                        
                        ;; Stack-Top wird erst zur Laufzeit bekannt, 
                        ;;------------------------------------------
                        (CC-op+ *stack-top* C-nargs)

                        ;; Stack-Top ist zur Uebersetzungszeit bekannt.
                        ;;---------------------------------------------
                        (+ nsimple *stack-top*))))

    ;; normale Parameter bearbeiten
    ;;-----------------------------
    (dolist (simple (?var-list params))
      (cg-var-bind simple *stack-top* C-stacktop)
      (incf *stack-top*))     

    ;; optionale und opt-suppliedp Parameter bearbeiten
    ;;-------------------------------------------------
    (when (> nopt 0)
        
      ;; Anzahl der opt-suppliedp Parameter bestimmen
      ;;---------------------------------------------
      (setq nopt-supplied
            (count-if-not #'null (?opt-list params)
                          :key #'?suppl))
        
      ;; Die optionalen und Rest-Parameter soweit im Stack verschieben, dass
      ;; zwischen den normalen und den optionalen Parametern Platz fuer die
      ;; opt-suppliedp Parameter entsteht.
      ;;--------------------------------------------------------------------
      (when (> nopt-supplied 0)

        ;; (C-MemUpMove SrcHigh SrcLow MoveDiff)
        ;;--------------------------------------
        (C-MemUpMove C-stacktop (CC-StackTop) nopt-supplied)

        ;; opt-supplied-Parameter mit NIL initialisieren
        ;;----------------------------------------------
        (dotimes (i nopt-supplied)
          (C-nil (CC-StackTop))
          (incf *stack-top*))

        ;; opt-supplied-Parameter wie simple-Parameter behandeln
        ;;------------------------------------------------------
        (C-Assign C-nargs (CC-op+ C-nargs nopt-supplied))
        (incf nsimple nopt-supplied)
        (incf nsimpopt nopt-supplied))

      ;; Hier werden die angegebenen optionalen Parameter gebunden
      ;; und die zugehoerigen supplied-p Variablen an T gebunden.
      ;; ACHTUNG: Die Variablen werden hier von RECHTS nach LINKS gebunden.
      ;; Dies fuehrt nur deshalb nicht zu Fehlern, weil jeder Identifikator
      ;; hoechstens einmal gebunden wird und weil waehrend des Bindens
      ;; keine Ausdruecke ausgewertet werden,
      ;; die diese Identifikatoren enthalten.
      ;; Es wird nur dann Code erzeugt, wenn opt-supplied-p Parameter
      ;; vorhanden sind oder wenn mindestens ein optionaler Parameter
      ;; special gebunden wird oder frei in einer Closure vorkommt.
      ;;-------------------------------------------------------------
      (when (or (> nopt-supplied 0)
                (find-if #'(lambda (x) (or (dynamic-p x)
                                           (and (local-static-p x)
                                                (?closure x))))
                         (?opt-list params)
                         :key #'?var))

        (C-if (CC-op> C-nargs nsimpopt))
        (C-blockstart)
        (C-goto C-ALL_OPT_SUPPLIED)
        (C-blockend)

        ;; initialisierte optionale Parameter bearbeiten
        ;;----------------------------------------------
        (C-switch C-nargs)
        (C-blockstart)
        (setq suppl-index *stack-top*)
        (let ((first-CASE t)
              (i nopt)

              ;; Weil cg-var-bind 2x f"ur special und closure Variablen
              ;; aufgerufen wird, soll der erste Aufruf keinen Effekt auf
              ;; folgende special Variablen haben.
              ;;----------------------------------
              (*special-count* *special-count*)
              (*do-not-restore* *do-not-restore*))
          (dolist (opt-spec (reverse (?opt-list params)))

            ;; erzeugt :
            ;;----------
            ;; switch (nargs)
            ;; {
            ;; case <nsimple+nopt>:
            ;; ALL_OPT_SUPPLIED   : <hdl-opt(nopt-1)>
            ;;   ....
            ;; case <nsimple+1   >: <hdl-opt(0)>
            ;; }

            ;; CASE nur einfuegen, wenn ueberhaupt etwas mit
            ;; dem i. Parameter geschieht.
            ;;----------------------------------------------
            (when (or (?suppl opt-spec)
                      (dynamic-p (?var opt-spec))
                      (and (local-static-p (?var opt-spec))
                           (?closure (?var opt-spec))))
              (C-Case (+ nsimple i))
              (when first-CASE
                (setq first-CASE nil)
                (C-label C-ALL_OPT_SUPPLIED))
              (cg-var-bind
               (?var opt-spec) (+ *stack-top* i -1) C-stacktop)
              (when (?suppl opt-spec)
                (decf suppl-index)
                (C-T (CC-Stack suppl-index))
                (cg-var-bind (?suppl opt-spec) suppl-index C-stacktop)))
            (decf i)))
        
        (C-blockend))

      ;; Hier werden die nicht belegten optionalen Parameter
      ;; mit ihrem Defaultwert initialisiert.
      ;; Wenn kein optionaler Parameter angegeben ist, dann liegt
      ;; Stack-Top direkt hinter dem ersten normalen Parameter.
      ;;---------------------------------------------------------
      (C-switch C-nargs)
      (C-blockstart)
      (let ((*result-spec* (stacktop-result-location))
            (i 0))
        (dolist (opt-spec (?opt-list params))
          (C-case (+ nsimple i))
          (cg-form (?init opt-spec))
          (cg-var-bind (?var opt-spec) *stack-top*)
          (incf *stack-top*)
          (incf (?offset *result-spec*))
          (when (?suppl opt-spec)

            ;; supplied-p Parameter wurden schon mit NIL initialisiert
            ;;--------------------------------------------------------
            (cg-var-bind (?suppl opt-spec) suppl-index (CC-StackTop))
            (incf suppl-index))
          (incf i)))

      (cond

        ;; Waehrend der Bearbeitung von &rest und &key sieht es so aus,
        ;; als ob alle optionalen Parameter angegeben worden seien. 
        ;;---------------------------------------------------------
        ((or (> nkey 0) rest)
         (C-Assign C-nargs nsimpopt))
                
        ;; auf ueberschuessige Argumente pruefen
        ;;--------------------------------------
        (T
         (C-case (+ nsimple nopt))
         (C-break)
         (C-defaultcase)
         (C-Abort "TOO_MANY_ARGS")))

      (C-blockend))

    (when (> nkey 0)

      ;; Achtung, erzeugt nicht geschlossenen C-Block !
      ;; Wird erst nach der Bearbeitung der Key-Parameter geschlossen.
      ;;--------------------------------------------------------------
      (C-blockstart)
      
      ;; wird von 'keysort' initialisiert
      ;;---------------------------------
      (C-ArrayDecl "BOOL" C-supl_flags nkey) 

      ;; ein Array mit Zeigern auf die moeglichen Keywords anlegen.
      ;;-----------------------------------------------------------
      (C-StaticArrayInitDecl "CL_FORM *" C-keylist)
      (C-initstart)
      (dolist (key-spec (?key-list params))
        (C-init (CC-symbol (?sym key-spec))))
      (C-initend)
      
      ;; verschiebt Rest-Liste im Stack, so dass genuegend Platz fuer die
      ;; Key-Werte entsteht und sortiert die Key-Werte in die entstandene
      ;; Luecke. Nicht angegebene Key-Werte werden mit NIL belegt.
      ;;-----------------------------------------------------------------
      (C-Call "keysort" (CC-StackTop) (CC-op- C-nargs nsimpopt)
              nkey C-keylist C-supl_flags (CC-bool (?allow-other-keys params)))
      (setq first-key *stack-top*)
      (incf *stack-top* nkey))
    
    ;; Pruefen, ob die Rest-Variable nur angegeben wurde, damit beliebig viele
    ;; Argumente angegeben werden koennen, ohne dass die Rest-Variable selbst
    ;; verwendet wird.
    ;;----------------
    (if rlo
        (when (plusp (?read rest))
          (C-Assign (CC-restvar (?offset rest)) 
                    (CC-stack (+ nsimpopt closure-offset)))
          (incf *rlo-statistics-rest-funs*)
          (incf *rlo-statistics-rest-opt*)
          (when (= nsimpopt 0)
            (incf nsimpopt) (incf *stack-top*)))

        ;; Erzeugt die Rest-Liste im Heap, Zeiger auf diese Liste wird hinter
        ;; den Key-Parametern abgelegt.
        ;;-----------------------------
        (when (and rest (or (> (?write rest) 1) (plusp (?read rest))))
          (when (plusp (?read rest))
            (C-Lispcall "Flist" (CC-StackTop) (CC-op- C-nargs nsimpopt)))
          (incf *rlo-statistics-rest-funs*)
          (cg-var-bind rest *stack-top*)
          (incf *stack-top*)))

    (when (> nkey 0)
      (let ((*result-spec* (stack-location first-key *level*))
            (i 0))
        (dolist (key-spec (?key-list params))
          
          ;; nur etwas zu tun, wenn Supplied-Variable gesetzt
          ;; oder Keyword-Parameter mit Wert != NIL initialisiert wird.
          ;;--------------------------------------------------------------
          (when (or (?init key-spec) (?suppl key-spec))

            (C-if (CC-MacroCall "NOT" (CC-arraykomp C-supl_flags i)))
            (C-blockstart)

            ;; wenn nicht angegeben und der Defaultwert ungleich
            ;; NIL, dann mit Defaultwert initialisieren.
            ;;--------------------------------------------------
            (when (?init key-spec)
              (cg-form (?init key-spec)))
            
            (when (?suppl key-spec)

              ;; supplied Parameter vorhanden, auf nil setzen
              ;;---------------------------------------------
              (C-nil (CC-StackTop))
              (C-blockend)
              (C-else)
              (C-blockstart)

              ;; auf t setzen
              ;;-------------
              (C-t (CC-StackTop)))
            
            (C-blockend)
            
            (when (?suppl key-spec)
              
              ;; ACHTUNG: hier wird nicht streng von links nach rechts gebunden.
              ;; Dies fuehrt nur deshalb nicht zu Fehlern, weil jeder
              ;; Identifikator hoechstens einmal gebunden wird und
              ;; weil waehrend des Bindens keine Ausdruecke ausgewertet werden,
              ;; die diese Identifikatoren enthalten.
              ;;---------------------------------------------------------------
              (cg-var-bind (?suppl key-spec) *stack-top*)
              (incf *stack-top*)))
          
          (cg-var-bind (?var key-spec) (?offset *result-spec*) (CC-StackTop))
          (incf (?offset *result-spec*))
          (incf i)))
      
      ;; den C-Block schliessen, in dem supl_flags definiert wurde.
      ;;-----------------------------------------------------------
      (C-blockend))

    ;; Restlistenoptimierung einschalten?
    ;;-----------------------------------
    (setq *rest-optimization* 
          (when rlo 
            (C-Assign C-local 
                      (CC-stack (if (plusp closure-offset)
                                    (CC-op+ C-nargs closure-offset)
                                    C-nargs)))
            (+ nsimpopt closure-offset)))))

;;------------------------------------------------------------------------------
;; let*
;;------------------------------------------------------------------------------
(defmethod cg-form ((form let*-form))
  (let ((old-stack *stack-top*)
        (old-special *special-count*)
        (vars (?var-list form))
        (forms (?init-list form)))

    ;; spezielle `rest'-Variablen deklarieren
    (when *rest-optimization*
      (C-blockstart)
      (dolist (var vars)
        (when (rest-p var)
          (cg-var-bind var (decf *rest-var-count*))
          (C-PtrDecl "CL_FORM" (CC-restvar (?offset var))))))
    
    (dolist (var vars)
      (if (and *rest-optimization* (rest-p var))
          (let ((*result-spec* var))
            (cg-form (pop forms)))
          (let ((*result-spec* (stacktop-result-location)))
            (cg-form (pop forms))
            (cg-var-bind var *stack-top*)
            (incf *stack-top*))))
    
    (cg-form (?body form))
    (when *rest-optimization*
      (C-blockend))
    (setq *stack-top* old-stack)
    (C-restore-special old-special)))

;;------------------------------------------------------------------------------
;; Variablenzugriff
;;------------------------------------------------------------------------------
(defmethod cg-form ((form var-ref))
  (let ((var (?var form)))
    (cond
      ((and (eq *result-spec* 'C-bool) (dynamic-p var) (> *special-count* 0))
       (C-Assign C-bool_result (CC-make-bool (CC-special var)))
       (setq *C-bool* C-bool_result))
      (t (to-result-loc var)))))

;;------------------------------------------------------------------------------
;; setq
;;------------------------------------------------------------------------------
(defmethod cg-form ((form setq-form))
  (let ((var (?var (?location form))))
    (let ((*result-spec*
           (if (and (local-static-p var)
                    (eql (?offset var) (1- *stack-top*))
                    (eql (?level var) *level*)
                    (not (?closure var)))
               
               ;; Zuweisung an eine lokale Variable, die direkt unterhalb
               ;; von *stacktop* liegt; (nicht im Heap !)
               ;;----------------------------------------
               (stacktop-result-location (?offset var))
               var)))
      
      (cg-form (?form form)))
    (to-result-loc var)))

;;------------------------------------------------------------------------------
;; Named Constant
;;------------------------------------------------------------------------------
(defmethod cg-form ((form defined-named-const))
  (let ((value (?value form)))
    (if (eq :unknown value)
        (internal-error
         'cg-form
         "Named constants with unknown values are not implemented: ~s"
         (?symbol form))
        (etypecase value
          ((or simple-literal sym structured-literal) (cg-form value))))))

(defmethod cg-form ((form imported-named-const))
  (case *result-spec*
    ((nil))
    (C-BOOL (setq *C-bool* C-TRUE))
    (t (C-MacroCall (CC-NameConc "LOAD_"
                                 (case (?value-zs-type form)
                                   (:cons "CONS")
                                   (:string "SMSTR")
                                   (:vector "SMVEC_T")
                                   (:array "SMAR_T")
                                   (:literal-instance "STRUCT")))
                    (CC-cast "CL_FORM *" (?adr form))
                    (CC-dest *result-spec*)))))

;;------------------------------------------------------------------------------
;; Bindet die Variable 'var' neu. 'var' wird im Stack oder im Heap oder in
;; der Value-Cell eines Symbols angelegt. 'var' wird initialisiert mit dem
;; Wert im aktuellen Activation-Record an der Position 'offset'.
;; C-stacktop enthaelt C-Code, der die naechste freie Position im Stack angibt.
;;------------------------------------------------------------------------------
(defmethod cg-var-bind ((var local-static) offset &optional C-stacktop) 

  ;; Offset eintragen fuer spaetere Zugriffe
  ;;----------------------------------------
  (setf (?offset var) offset)
  (when (?closure var)

    ;; Wenn nichts anderes angeben ist, dann liegt der Wert der zu bindenden
    ;; Variablen auf der Spitze des Stacks. Die naechste freie Position liegt
    ;; also um 1 hoeher.
    ;;------------------
    (unless C-stacktop (setq C-stacktop (CC-Stack (1+ offset))))

    ;; neue Zelle im Heap anlegen, Wert dorthin kopieren und
    ;; auf dem LISP-Stack einen Zeiger hinterlassen
    ;;---------------------------------------------
    (C-MacroCall "GEN_HEAPVAR" (CC-Stack offset) C-stacktop)))

;;------------------------------------------------------------------------------
(defmethod cg-var-bind ((var dynamic) offset &optional C-stacktop)
  (declare (ignore C-stacktop))
  (C-MacroCall "BIND_SPECIAL" (CC-symbol (?sym var)) (CC-Stack offset))

  ;; SONDERFALL feststellen:
  ;; Eine special Variable wird gebunden und das Resultat des aktuellen
  ;; Ausdrucks (Funktion, Let*) wird genau dieser Variablen zugewiesen.
  ;; Dann darf diese Variable nicht restauriert werden.
  ;;---------------------------------------------------
  (when (and *result-spec* (equal-loc *result-spec* var))
    (push *special-count* *do-not-restore*))
  (incf *special-count*))

;;------------------------------------------------------------------------------
;; Eintraege vom Binding-Stack entfernen, und die Special Variablen
;; restaurieren, die nicht in *do-not-restore* eingetragen sind.
;; Setzt *special-count* und *do-not-resore* zurueck.
;;------------------------------------------------------------------------------
(defun C-restore-special (old-special)
  (do ()
      ((= *special-count* old-special))
    (decf *special-count*)
    (cond
      ((eql (car *do-not-restore*) *special-count*)
       (pop *do-not-restore*)
       (C-MacroCmd "POP_SPECIAL"))
      (t (C-MacroCmd "RESTORE_SPECIAL")))))

;;------------------------------------------------------------------------------
;; Wie oben, aber veraendert *special-count* und *do-not-restore* nicht.
;;------------------------------------------------------------------------------
(defun C-restore-special2 (old-special)
  (do ((i *special-count*)
       (l *do-not-restore*))
      ((= i old-special))
    (decf i)
    (cond
      ((eql (car l) i)
       (pop l)
       (C-MacroCmd "POP_SPECIAL"))
      (t (C-MacroCmd "RESTORE_SPECIAL")))))

;;------------------------------------------------------------------------------
;; Resultat: C-Code (als String) fuer den Zugriff auf eine statisch gebundene
;; Variable.
;; Es koennen 3 Faelle eintreten:
;; - Die Variable liegt direkt im Stack.
;; - Die Variable liegt im Heap, da sie aus einer Closure referenziert
;;   (werden) wird. Es existiert aber auch (noch) eine Referenz auf diese 
;;   Variable im Activation Record der Funktion, aus der der Zugriff erfolgt.
;; - Die Variable liegt im Heap. Der Zugriff erfolgt innerhalb einer Closure. 
;;   Die Adresse der Variablen wird der CLOSURE-Datenstruktur entnommen,
;;   die an Position 0 im Activation Record der Closure liegt.
;;------------------------------------------------------------------------------
(defun CC-static (static)
  (let* ((level (?level static))
         (offset (?offset static)))
    
    (cond
      ;; liegt direkt im Stack
      ;;----------------------
      ((not (?closure static)) (CC-frame-access offset level))

      ;; Zugriff innerhalb einer Closure auf eine freie Variable der Closure
      ;;--------------------------------------------------------------------
      ((< level *cl-level*) (CC-clos-heap (closure-var-offset static)))

      ;; Zeiger liegt im Stack, Variable im Heap
      ;;----------------------------------------
      (t (CC-stack-heap offset level)))))

;;------------------------------------------------------------------------------
;; Resultat: C-Code (als String), fuer den Zugriff auf einen Offset in einem
;; Activation Record.
;;------------------------------------------------------------------------------
(defun CC-frame-access (offset level)
  (if (= *level* level)
      
      ;; liegt im aktuellen Activation Record.
      ;;--------------------------------------
      (CC-Stack offset)
      
      (CC-arrayptr (CC-arraykomp C-display (- level *cl-level*)) offset)))

;;------------------------------------------------------------------------------
(defun CC-StackTop ()
  (CC-Stack *stack-top*))

;;------------------------------------------------------------------------------
;; Resultat: C-Code (als String) fuer den Zugriff auf eine lokale Variable im
;; Heap ueber eine Indirection-Zelle im Stack.
;;------------------------------------------------------------------------------
(defun CC-stack-heap (offset level)
  (CC-MacroCall "INDIRECT" (CC-frame-access offset level)))

;;------------------------------------------------------------------------------
;; Resultat: C-Code (als String) fuer den Zugriff auf eine lokale Variable im
;; Heap, auf die ein Zeiger in einer Closure-Datenstruktur zeigt.
;;------------------------------------------------------------------------------
(defun  CC-clos-heap (index)
  (CC-MacroCall "INDIRECT" (CC-heapenv index)))

;;------------------------------------------------------------------------------
;; Resultat: C-Code (als String) fuer den Zugriff auf eine Komponenete einer
;; Closure-Datenstruktur
;;------------------------------------------------------------------------------
(defun CC-closure-access (closure index)
  (CC-op+ (CC-MacroCall "GET_FORM" closure) index))

;;------------------------------------------------------------------------------
;; Resultat: C-Code (als String) fuer den Zugriff auf den n-ten Eintrag der
;; CLOSURE-Datenstruktur der aktuellen Closure.  Das 1. Argument einer Closure
;; ist seine CLOSURE-Datenstruktur und wird an der Position 0 im aktuellen
;; Activation-Record uebergeben.
;;------------------------------------------------------------------------------
(defun CC-heapenv (n)
  (CC-closure-access (CC-frame-access 0 *cl-level*) n))

;;------------------------------------------------------------------------------
;; Location fuer *result-spec* erzeugen.
;;------------------------------------------------------------------------------
(defun stacktop-location ()
  (stack-location *stack-top* *level*))

;;------------------------------------------------------------------------------
(defun stack-location (offset level)
  (make-instance 'local-static :offset offset :level level))

;;------------------------------------------------------------------------------
;; Eine Location von diesem Typ wird in *result-spec* eingetragen, wenn das
;; Resultat auf der Spitze des Stack generiert werden soll. Diese Information
;; wird bei der Parameteruebergabe an eine Funktion, die das Resultat
;; berechnet, verwendet, um das aktuelle Activation-Record fuer die
;; aufzurufende Funktion teilweise wiederzuverwenden.
;;------------------------------------------------------------------------------
(defun stacktop-result-location (&optional (offset *stack-top*))
  (make-instance 'stacktop-result :offset offset :level *level*))

;;------------------------------------------------------------------------------
(defun stacktop-to-result-loc ()
  (to-result-loc (stacktop-location)))
  
;;------------------------------------------------------------------------------
(defun to-result-loc (loc)
  (case *result-spec*
    
    ;; Resultat wird nicht benoetigt
    ;;------------------------------
    ((NIL))
    
    ;; Boolesches Resultat gewuenscht
    ;;-------------------------------
    (C-BOOL 
     (setq *C-bool* (if (rest-p loc)
                        (CC-MacroCall "REST_NOT_EMPTY" (CC-dest loc))
                        (CC-make-bool (CC-dest loc)))))

    ;; Normales Resultat erzeugen
    ;;---------------------------
    (T (cg-copy loc *result-spec*))))

;;------------------------------------------------------------------------------
(defun CC-dest (loc)
  (typecase loc
    (static (CC-static loc))
    (dynamic (CC-special loc))
    (mv-buf (CC-mv_buf (?offset loc)))
    (T (error "unexpected location: ~S" loc))))

;;------------------------------------------------------------------------------
(defun equal-loc (loc1 loc2)
  (typecase loc1
    (local-static
     (and (local-static-p loc2)
          (eql (?offset loc1) (?offset loc2))
          (eql (?level loc1) (?level loc2))
          (eql (?closure loc1) (?closure loc2))))
    ((or global-static imported-static dynamic) (eq loc1 loc2))
    (mv-buf (and (mv-buf-p loc2)
                 (eql (?offset loc1) (?offset loc2))))
    (T (error "unexpected location: ~S" loc1))))

;;------------------------------------------------------------------------------
(defun cg-copy (source dest)
  (unless (or (equal-loc source dest)
              (and *copy-source*
                   (equal-loc source *copy-dest*)
                   (equal-loc dest *copy-source*)))
    (if (and (rest-p source) (rest-p dest))
        (C-Assign (CC-dest dest) (CC-dest source))
        (C-copy (CC-dest source) (CC-dest dest)))
    (setq *copy-source* source *copy-dest* dest)))

;;------------------------------------------------------------------------------
(defun rest-p (loc)
  (and (local-static-p loc) (minusp (?offset loc))))

;;------------------------------------------------------------------------------
;; Test, ob &rest-Liste optimiert werden kann. Dazu muß eine &rest-Variable
;; aber keine &key-Variablen existieren. Außerdem muß die &rest-Variable
;; überhaupt benutzt werden.  `rest' darf nur in bestimmten
;; Funktionensaufrufen und Zuweisungen vorkommen und nicht frei in den lokalen
;; Funktionen von "fun".
;;------------------------------------------------------------------------------
(defun rest-optimization-p (params body local-funs)
  (when (and *optimize*
             (local-static-p (?rest params))
             (or (> (?write (?rest params)) 1) (plusp (?read (?rest params))))
             (not (?key-list params)))
    ;; Restvariable kennzeichnen
    ;;--------------------------
    (dolist (v (?all-vars params)) (setf (?offset v) 0))
    (setf (?offset (?rest params)) -1)
    
    ;; Rumpf überprüfen und freie Variablen auf Rest-Variablen überprüfen.
    ;;--------------------------------------------------------------------
    (and (rlo-form body)
         (notany #'(lambda (lfun) 
                     (some #'(lambda (var) (minusp (?offset var)))
                           (?free-lex-vars lfun)))
                 local-funs))))

;;------------------------------------------------------------------------------
;; Entspricht `form' einem "var-ref" auf eine &rest-Variable?
;;------------------------------------------------------------------------------
(defun rest-var-p (form)
  (and (var-ref-p form) 
       (local-static-p (?var form)) 
       (minusp (?offset (?var form)))))

;;------------------------------------------------------------------------------
;; Entspricht `form' einem Zugriff auf `rest' oder (L::CDR form), wobei `form'
;; wieder von der gleichen Art sein muß ?
;;------------------------------------------------------------------------------
(defun rlo-rest-form (form)
  (or (rest-var-p form)
      (and (app-p form) (fun-p (?form form))
           (eq (?my-last-arg-may-be-rest-var (?form form)) :cdr)
           (rlo-rest-form (car (?arg-list form))))
      (and (let*-form-p form)
           (every #'(lambda (v i)       ;Variablen binden
                      (if (local-static-p v)
                          (if (rlo-rest-form i)
                              (setf (?offset v) -1)
                              (if (rlo-form i)
                                  (setf (?offset v) 0)
                                  nil))
                          (rlo-form i)))
                  (?var-list form)
                  (?init-list form))
           (or (rlo-rest-form (?body form))
               (and (progn-form-p (?body form))
                    (rlo-form-list (butlast (?form-list (?body form))))
                    (rlo-rest-form 
                     (car (last (?form-list (?body form))))))))))

;;------------------------------------------------------------------------------
;; Untersuche eine Liste von "form"s.
;;------------------------------------------------------------------------------
(defun rlo-form-list (form-list)
  (every #'rlo-form form-list))

;;------------------------------------------------------------------------------
;; APP: Wenn `rest' als Argument in einer Funktion vorkommt, die entweder
;; erst zur Laufzeit evaluiert wird oder keine der speziell zu behandelnden
;; Funktionen ist -> verloren.
;;------------------------------------------------------------------------------
(defmethod rlo-form ((an-app app))
  (if (and (fun-p (?form an-app))
           (?my-last-arg-may-be-rest-var (?form an-app))
           (rlo-rest-form (car (last (?arg-list an-app)))))
      (case (?my-last-arg-may-be-rest-var (?form an-app))
        ((:length
          :car
          :apply
          :atom
          :consp) (rlo-form-list (butlast (?arg-list an-app))))
        (T nil))
      (and (rlo-form (?form an-app))
           (rlo-form-list (?arg-list an-app)))))

;;------------------------------------------------------------------------------
;; SETQ-FORM: Zuweisung an `rest', die nicht (L::CDR rest) ist -> verloren
;; Sonst untersuche "form".
;;------------------------------------------------------------------------------
(defmethod rlo-form ((a-setq-form setq-form))
  (if (rest-var-p (?location a-setq-form))
      (rlo-rest-form (?form a-setq-form))
      (rlo-form (?form a-setq-form))))

;;------------------------------------------------------------------------------
;; PROGN-FORM: Untersuche "form-list".
;;------------------------------------------------------------------------------
(defmethod rlo-form ((a-progn-form progn-form))
  (rlo-form-list (?form-list a-progn-form)))

;;------------------------------------------------------------------------------
;; IF-FORM: Untersuche "pred", "then" und "else".
;;------------------------------------------------------------------------------
(defmethod rlo-form ((an-if-form if-form))
  (and (or (rlo-rest-form (?pred an-if-form))
           (rlo-form (?pred an-if-form)))
       (rlo-form (?then an-if-form))
       (rlo-form (?else an-if-form))))

;;------------------------------------------------------------------------------
;; SWITCH-FORM: Untersuche "form" und alle "cases".
;;------------------------------------------------------------------------------
(defmethod rlo-form ((a-switch-form switch-form))
  (and (rlo-form (?form a-switch-form))
       (rlo-form-list (?case-list a-switch-form))
       (rlo-form (?otherwise a-switch-form))))

;;------------------------------------------------------------------------------
;; LABELED-FORM: Untersuche "form".
;;------------------------------------------------------------------------------
(defmethod rlo-form ((a-labeled-form labeled-form))
  (rlo-form (?form a-labeled-form)))

;;------------------------------------------------------------------------------
;; LET*-FORM: Prüfe, ob neue &rest-Variablen gebunden werden sollen.
;; Untersuche "init-list" und "body". 
;;------------------------------------------------------------------------------
(defmethod rlo-form ((a-let*-form let*-form))
  (and (every #'(lambda (v i)
                  (if (local-static-p v)
                      (if (rlo-rest-form i)
                          (setf (?offset v) -1)
                          (if (rlo-form i)
                              (setf (?offset v) 0)
                              nil))
                      (rlo-form i)))
              (?var-list a-let*-form)
              (?init-list a-let*-form))
       (rlo-form (?body a-let*-form))))

;;------------------------------------------------------------------------------
;; LABELS-FORM: Untersuche "fun-list" und "body".
;;------------------------------------------------------------------------------
(defmethod rlo-form ((a-labels-form labels-form))
  (and (rlo-form-list (?fun-list a-labels-form))
       (rlo-form (?body a-labels-form))))

;;------------------------------------------------------------------------------
;; LET/CC-FORM: Untersuche "body".
;;------------------------------------------------------------------------------
(defmethod rlo-form ((a-let/cc-form let/cc-form))
  (rlo-form (?body a-let/cc-form)))

;;------------------------------------------------------------------------------
;; TAGBODY-FORM: Untersuche "first-form" und "tagged-form-list".
;;------------------------------------------------------------------------------
(defmethod rlo-form ((a-tagbody-form tagbody-form))
  (and (rlo-form (?first-form a-tagbody-form))
       (every #'(lambda (a-tagged-form) (rlo-form (?form a-tagged-form)))
              (?tagged-form-list a-tagbody-form))))

;;------------------------------------------------------------------------------
;; MV-LAMBDA: Wenn ebenfalls `rest'-Parameter vorhanden -> verloren.
;;------------------------------------------------------------------------------
(defmethod rlo-form ((a-mv-lambda mv-lambda))
  (unless (plusp (?read (?rest (?params a-mv-lambda))))
    (and (rlo-form (?body a-mv-lambda))
         (rlo-form (?arg a-mv-lambda)))))

;;------------------------------------------------------------------------------
;; VAR-REF: Zugriff auf `rest' -> verloren.  Die Benutzung von `rest' innerhalb
;; der erlaubten Applikationen wird bereits in der Methode für "app" überprüft. 
;;------------------------------------------------------------------------------
(defmethod rlo-form ((a-var-ref var-ref))
  (not (rest-var-p a-var-ref)))

;;------------------------------------------------------------------------------
;; DEFAULT: nix zu tun. Alles ok.
;;------------------------------------------------------------------------------
(defmethod rlo-form ((a-form form))
  T)

(defmethod rlo-form ((a-cont cont))
  T)

;;------------------------------------------------------------------------------
(provide "cgvars")
