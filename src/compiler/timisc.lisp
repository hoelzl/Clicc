;;;-----------------------------------------------------------------------------
;;; Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel, Germany
;;;-----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Funktion : Hilfsfunktionen, Makros und Fehlerbehandlung zur Typinferenz
;;;
;;; $Revision: 1.45 $
;;; $Log: timisc.lisp,v $
;;; Revision 1.45  1993/11/21  22:15:45  kl
;;; In den Typumgebungen treten jetzt keine doppelten Elemente mehr auf.
;;; Referenz auf *ti-level* entfernt.
;;;
;;; Revision 1.44  1993/10/22  16:18:12  kl
;;; Umstellung von has-no-side-effect auf is-side-effect-free.
;;;
;;; Revision 1.43  1993/10/12  19:53:06  kl
;;; Die Funktion not-destructive von Anour wird nun benutzt.
;;;
;;; Revision 1.42  1993/10/10  18:00:53  kl
;;; Veraendern von Vorgaengertypumgebungen geaendert.
;;;
;;; Revision 1.41  1993/10/08  22:38:19  kl
;;; Praezisere Behandlung von Seiteneffekten eingebaut. Einige Funktionen zur
;;; Handhabung von Typumgebungen arbeiten nun destruktiv.
;;;
;;; Revision 1.40  1993/10/05  15:42:09  hk
;;; Fehler in join-pred-type-env-with-type-env (vorläufig ?) behoben.
;;;
;;; Revision 1.39  1993/09/17  11:45:57  kl
;;; Aufruf von assoc geschieht jetzt mit einem eq-Test.
;;;
;;; Revision 1.38  1993/09/12  16:10:06  kl
;;; Anpassung an die neue Einteilung der Typinferenz-Stufen.
;;;
;;; Revision 1.37  1993/09/12  15:21:08  kl
;;; Ruecksetzen dynamisch gebundener Variablen in let*-Ausdruecken korrigiert.
;;;
;;; Revision 1.36  1993/06/24  14:05:01  kl
;;; Debug-Funktion nach timain.lisp verlegt. Ruecksetzen der Typbindungen nach
;;; einem destruktiven Seiteneffekt aendert nur noch die strukturiert
;;; modellierten Typen (zur Zeit nur Listen).
;;;
;;; Revision 1.35  1993/06/17  08:00:09  hk
;;; Copright Notiz eingefuegt
;;;
;;; Revision 1.34  1993/06/10  10:32:08  kl
;;; Binden und Setzen von Funktionsparametern vereinheitlicht.
;;;
;;; Revision 1.33  1993/06/07  10:10:57  kl
;;; Binden aktueller Parameter vereinheitlicht.
;;;
;;; Revision 1.32  1993/05/18  16:16:53  kl
;;; Umstellung auf die neue Implementierung des Typverbands.
;;;
;;; Revision 1.31  1993/05/15  13:41:13  kl
;;; Behandlung der Initialisierungsausdruecke umgestellt.
;;;
;;; Revision 1.30  1993/05/11  11:22:54  kl
;;; Bindung der supplied-Parameter geaendert.
;;;
;;; Revision 1.29  1993/05/03  10:20:05  kl
;;; Debugfunktionen verbessert.
;;;
;;; Revision 1.28  1993/04/20  15:04:56  kl
;;; Ausgabemeldung geaendert.
;;;
;;; Revision 1.27  1993/04/19  12:30:15  kl
;;; Umstellung auf die neue Handhabung der besonderen Aufrufer.
;;;
;;; Revision 1.26  1993/02/16  16:10:38  hk
;;; Revision Keyword eingefuegt.
;;;
;;; Revision 1.25  1993/02/15  14:43:22  kl
;;; Operationen auf Typumgebungen beschleunigt.
;;;
;;; Revision 1.24  1993/02/03  09:56:51  kl
;;; Aufruf von get-decoded-time entfernt.
;;;
;;; Revision 1.23  1993/02/02  10:12:24  kl
;;; join-pred-type-env-with-type-env als Spezialfall der Vereinigung zweier
;;; Typumgebungen eingefuegt.
;;;
;;; Revision 1.22  1993/02/02  09:49:04  kl
;;; Sicherheitsabfrage in get-type verbessert. Ausgabe der Groesse der
;;; *ti-fun-workset* um eine Uhrzeitangabe erweitert.
;;;
;;; Revision 1.21  1993/01/27  13:03:06  kl
;;; Makrodefinitionen umgestellt.
;;;
;;; Revision 1.20  1993/01/26  18:38:32  kl
;;; Die Suche nach funktionalen Objekten nach tipass1.lisp verlegt.
;;;
;;; Revision 1.19  1993/01/25  13:13:58  kl
;;; Fehler von Joerg korrigiert und Algorithmus zum Erstellen der
;;; called-by-Komponenten der definierten Funktionen eingebaut.
;;;
;;; Revision 1.18  1993/01/21  14:24:25  kl
;;; Typfehler und -warnungen werden nun nur noch einmal ausgegeben.
;;;
;;; Revision 1.17  1993/01/19  11:32:18  kl
;;; Anwendung von list-type in bind-parameter-types korrigiert.
;;;
;;; Revision 1.16  1993/01/12  12:48:49  kl
;;; Binden der Parametertypen geaendert. Bei Parametern wird jetzt der
;;; Vereinigungstyp ueber die entsprechenden Argumenttypen gebildet.
;;;
;;; Revision 1.15  1993/01/06  18:00:09  kl
;;; assert-type in ein Makro umgewandelt.
;;;
;;; Revision 1.14  1993/01/06  13:32:49  kl
;;; Ruecksetzen der Typumgebungen verbessert.
;;;
;;; Revision 1.13  1993/01/05  15:00:00  kl
;;; join-type-environments erweitert.
;;;
;;; Revision 1.12  1992/12/10  10:17:57  kl
;;; Umstellung auf den neuen Ort der Funktionsbeschreibungen.
;;;
;;; Revision 1.11  1992/12/08  14:35:04  kl
;;; Zur Zeit werden gar keine Typwarnungen mehr ausgegeben.
;;;
;;; Revision 1.10  1992/12/02  09:41:26  kl
;;; Zugriffe auf Funktionsbeschreibungen hierher verlegt und verbessert.
;;;
;;; Revision 1.9  1992/12/01  15:49:56  kl
;;; ti-warning implementiert. Dabei *ti-errors* und *ti-warnings* verwendet.
;;;
;;; Revision 1.8  1992/11/26  11:40:23  kl
;;; Beim Abfragen einer Typbindung wird getestet, ob sie vorhanden ist.
;;; wrong-argument-types um den Parameter
;;;
;;; Revision 1.7  1992/11/24  16:38:04  kl
;;; initialize-function-descriptions beruecksichtigt nun lokal definierte
;;; Funktionen. list-type nach titypes.lisp verlegt. Kommentare erweitert.
;;;
;;; Revision 1.6  1992/11/23  13:33:53  kl
;;; Typen globaler Variablen werden an die Variable und nicht mehr an das
;;; zugehoerige Symbol gebunden.
;;; In bind-parameter-types werden jetzt alle Parameterarten gebunden.
;;;
;;; Revision 1.5  1992/11/05  14:37:23  kl
;;; Update-type-f eingefuehrt und Typbindungen verbessert.
;;;
;;; Revision 1.4  1992/11/04  13:25:48  kl
;;; bind-type erweitert und Kommentare korrigiert.
;;;
;;; Revision 1.3  1992/11/02  12:13:18  kl
;;; Dynamische Variablen haben jetzt eine eigene Typumgebung.
;;;
;;; Revision 1.2  1992/10/15  19:04:11  kl
;;; Funktionen und Makros zu den Typbindungen umgestellt.
;;;
;;; Revision 1.1  1992/10/13  18:26:13  kl
;;; Initial revision
;;;
;;;-----------------------------------------------------------------------------

(in-package "CLICC")

;;------------------------------------------------------------------------------

(require "titypes")
(require "tidef")


;;------------------------------------------------------------------------------
;; Liefert eine leere Typumgebung.
;;------------------------------------------------------------------------------
(defun empty-environment () `())


;;------------------------------------------------------------------------------
;; Liefert die Bindung zu einer location.
;;------------------------------------------------------------------------------
(defun get-type-binding (location &optional (env *type-environment*))
  (assoc location env :test #'eq))


;;------------------------------------------------------------------------------
;; Liefert die Bindung zu einer location.
;;------------------------------------------------------------------------------
(defun get-type (location env)
  (let ((type-binding (get-type-binding location env)))
    (if type-binding 
        (rest type-binding)
        bottom-t)))


;;------------------------------------------------------------------------------
;; Setzt eine Typbindung.
;;------------------------------------------------------------------------------
(defmacro bind-type (location type env)
  `(let ((type-binding (get-type-binding ,location ,env)))
    (if type-binding
        (setf (rest type-binding) ,type)
        (push (cons ,location ,type) ,env))))


;;------------------------------------------------------------------------------
(defun bind-and-set-type (location type)
  (bind-type location type *type-environment*)
  (setf (?type location) type))


;;------------------------------------------------------------------------------
(defun bind-and-update-type (location type)
  (bind-type location type *type-environment*)
  (update-type-f (?type location) type))


;;------------------------------------------------------------------------------
;; assert-type sichert einer location in einer Umgebung einen Typ zu. Als 
;; Resultat wird die aktualisierte Bindung geliefert.
;;------------------------------------------------------------------------------
(defmacro assert-type (location new-type environment)
  `(let* ((old-type (get-type ,location ,environment))
          (type-meet (type-meet old-type ,new-type)))
    (unless (type-eq old-type type-meet)
      (bind-type ,location type-meet ,environment))))


;;------------------------------------------------------------------------------
;; simple-set-parameter-types bindet an alle Parameter einer Funktion den Typ 
;; `argument-type', der mit top-t vorbesetzt ist.
;;------------------------------------------------------------------------------
(defun simple-set-parameter-types (params &optional (argument-type top-t))
  (let* ((old-param-types (mapcar #'?type (?all-vars params))))

    ;; Zuerst werden die benoetigten Parameter gebunden.
    ;;--------------------------------------------------
    (dolist (a-var (?var-list params))
      (update-type-f (?type a-var) argument-type))

      
    ;; Dann werden die optionalen und Schluesselwortparameter gebunden.
    ;;-----------------------------------------------------------------
    (dolist (a-param (append (?opt-list params) (?key-list params)))
      (update-type-f (?type (?var a-param)) argument-type))      


    ;; Der Restparameter ist - wenn vorhanden - eine Liste.
    ;;-----------------------------------------------------
    (when (?rest params)
      (update-type-f (?type (?rest params)) (list-of argument-type)))

    
    ;; Liefere als Ergebnis, ob die Parametertypen gleich geblieben sind.
    ;;-------------------------------------------------------------------
    (every #'type-eq old-param-types (mapcar #'?type (?all-vars params)))))


;;------------------------------------------------------------------------------
;; set-param-types bindet Argumenttypen von getypten Argumenten an Parameter.
;; An supplied-Komponenten der Schluesselwort- und optionalen Parameter wird
;; entweder null-t oder not-null-t gebunden.
;;------------------------------------------------------------------------------
(defun set-parameter-types (params argument-types)
  (let ((old-param-types (mapcar #'?type (?all-vars params))))
  
    ;; Zuerst werden die benoetigten Parameter gebunden.
    ;;--------------------------------------------------
    (dolist (a-var (?var-list params))
      (update-type-f (?type a-var) (pop argument-types)))
      

    ;; Dann werden die optionalen Parameter gebunden.
    ;;-----------------------------------------------
    (dolist (an-opt (?opt-list params))
      (let ((type (pop argument-types)))
        (when type
          (update-type-f (?type (?var an-opt)) type))))
      

    ;; Nun werden die Schluesselwortparameter gebunden.
    ;;-------------------------------------------------
    (let ((union-type (apply #'multiple-type-join argument-types)))
      (dolist (a-key (?key-list params))
        (update-type-f (?type (?var a-key)) union-type)))      
      

    ;; Zuletzt wird - wenn vorhanden - der Restparameter gebunden.
    ;;----------------------------------------------------------------
    (when (?rest params)
      (update-type-f (?type (?rest params))
                     (apply #'list-cons-of argument-types)))


    ;; Liefere als Ergebnis, ob die Parametertypen gleich geblieben sind.
    ;;-------------------------------------------------------------------
    (every #'type-eq old-param-types (mapcar #'?type (?all-vars params)))))
  
  

;;------------------------------------------------------------------------------
;; bind-parameter-types bindet Parameter und beruecksichtigt initiale Werte und
;; supplied-Variablen.
;;------------------------------------------------------------------------------
(defun bind-parameter-types (params)
  (dolist (var (?var-list params))
    (bind-type var (?type var) *type-environment*))

  (dolist (param (append (?opt-list params) (?key-list params)))
    (let* ((var       (?var   param))
           (suppl     (?suppl param))
           (init      (?init  param))
           (init-type (if (is-side-effect-free init)
                          (analyse-types init)
                          (let* ((old-env (copy-type-env *type-environment*))
                                 (result  (analyse-types init)))
                            (setf *type-environment*
                                  (join-type-environments old-env
                                                          *type-environment*))
                            result))))

      (bind-and-update-type var (type-join (?type var) init-type))
      (when suppl
        (bind-and-update-type suppl bool-t))))

  (let ((rest (?rest params)))
    (when rest
      (bind-type rest (?type rest) *type-environment*))))


;;------------------------------------------------------------------------------
;; Setze alle Typbindungen auf das angegebene Element des Typverbandes.
;;------------------------------------------------------------------------------
(defun set-all-type-bindings-to (type)
  (dolist (pair *type-environment*)
    (setf (rest pair) type)))


;;------------------------------------------------------------------------------
;; Setzt Typbindungen nach einem Seiteneffekt geeignet zurueck.
;; Wenn `write-effect' eine Liste ist, dann werden die enthaltenen Variablen auf
;; Top gesetzt. Andernfalls wird `write-effect' als statische Tiefe derjenigen
;; Variablen angesehen, die zurueckzusetzen sind.
;; Zudem werden bei einem aufgetretenen destruktiven Seiteneffekt Listentypen 
;; zurueckgesetzt.
;;------------------------------------------------------------------------------
(defun reset-type-bindings (write-effect not-destructive)
  (cond ((listp write-effect)
         (unless not-destructive
           (reset-type-bindings-after-destructive-effect))
         (dolist (location write-effect)
           (bind-type location top-t *type-environment*)))
        
        (T
         (dolist (pair *type-environment*)
           (let ((location (first pair))
                 (type     (rest  pair)))
             (cond ((or (dynamic-p location)
                        (<= (?level location) write-effect))
                    (setf (rest pair) top-t))
                   
                   ((and (not not-destructive)
                         (zs-subtypep list-cons-of-bottom-t type))
                    (setf (rest pair) (type-join type list-component-top-t))))
             )))))

                  
;;------------------------------------------------------------------------------
;; Setzt Typbindungen nach einem destruktiven Seiteneffekt geeignet zurueck.
;; In dem jetzigen Typverband werden Listentypen auf list-of(Top) gesetzt, alle
;; anderen Typen bleiben erhalten.
;;------------------------------------------------------------------------------
(defun reset-type-bindings-after-destructive-effect ()
  (dolist (pair *type-environment*)
    (let ((type (rest pair)))
      (when (zs-subtypep list-cons-of-bottom-t type)
        (setf (rest pair) (type-join type list-component-top-t))))))
                             

;;------------------------------------------------------------------------------
;; Liefert eine Kopie der uebergebenen Typumgebung.
;;------------------------------------------------------------------------------
(defun copy-type-env (type-environment)
  (copy-alist type-environment))


;;------------------------------------------------------------------------------
;; Liefert die Vereinigungsumgebung zweier Typumgebungen. Als zweiter Wert wird
;; geliefert, ob die Vereinigungsumgebung echt schaerfere Typbindungen enthaelt
;; als Typumgebung1.
;;------------------------------------------------------------------------------
(defun join-type-environments (environment1 environment2)
  (let ((result ())
        (result-is-superenv-of-env1 nil))
    (dolist (assoc1 environment1 (values (append result environment2) 
                                         (or result-is-superenv-of-env1
                                             (not (endp environment2)))))
      (let* ((key1   (first assoc1))
             (data1  (rest  assoc1))
             (assoc2 (get-type-binding key1 environment2)))
        (cond (assoc2
               (let ((new-data (type-join data1 (rest assoc2))))
                 (push (cons key1 new-data) result)
                 (unless (type-eq new-data data1)
                   (setf result-is-superenv-of-env1 T))
                 (setf environment2 (remove key1 environment2 :key #'first))))
              (T 
               (push assoc1 result)))))))


;;------------------------------------------------------------------------------
;; Liefert zur Vorgaengertypumgebung einer Funktion und einer beliebigen 
;; Typumgebung die durch Vereinigung entstandene Vorgaengertypumgebung.
;;------------------------------------------------------------------------------
(defun join-pred-type-env-with-type-env (pred-type-env environment)
  (let ((result-is-superenv-of-pred-type-env nil))
    (values
     (dolist (an-entry pred-type-env pred-type-env)
       (let* ((location (first an-entry))
              (old-type (rest  an-entry))
              (binding  (get-type-binding location environment))
              (new-type (if binding 
                            (type-join (rest binding) old-type)
                            top-t)))
         (unless (type-eq old-type new-type)
           (setf result-is-superenv-of-pred-type-env t)
           (setf (rest an-entry) new-type))))
     result-is-superenv-of-pred-type-env)))


;;------------------------------------------------------------------------------
;; Aktualisiert alle Typbindungen aus `env1' mit den in `env2' vorliegenden 
;; Typbindungen.
;;------------------------------------------------------------------------------
(defun update-type-env (env1 env2)
  (let ((environment-changed nil))
    (values
     (mapcar #'(lambda (an-entry)
                 (let* ((location (first an-entry))
                        (old-type (rest  an-entry))
                        (binding  (get-type-binding location env2)))
                   (if binding
                       (let ((new-type (rest binding)))
                         (unless (type-eq old-type new-type)
                           (setq environment-changed t))
                         (cons location new-type))
                       (cons location old-type))))
             env1)
     environment-changed)))


;;------------------------------------------------------------------------------
;; Aktualisiert alle Typbindungen aus `old-env', deren locations nicht in 
;; `bound-vars-env' liegen, mit den in `new-env' vorliegenden Typbindungen.
;;------------------------------------------------------------------------------
(defun reset-let-type-env (old-env new-env bound-vars-env)
  (mapcar #'(lambda (an-entry)
              (let* ((var   (first an-entry))
                     (bound (get-type-binding var bound-vars-env)))
                (if bound
                    (if (dynamic-p var)
                        (cons var (rest bound))
                        an-entry)
                    (cons var (get-type var new-env)))))
          old-env))


;;------------------------------------------------------------------------------
;; Gibt die Anzahl der in der Menge der zu bearbeitenden Funktionen *work-set*
;; verbliebenen Funktionen aus.
;;------------------------------------------------------------------------------
(defun write-size-of-workset (workset iterations)
  (when (and (> *ti-verbosity* 2) 
             (= 0 (mod iterations *ti-write-size-of-workset-interval*)))

    (clicc-message "~4D elements remain in the analyzation workset." 
                   (length workset))))


;;------------------------------------------------------------------------------
(provide "timisc")



