;;;-----------------------------------------------------------------------------
;;; Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel, Germany
;;;-----------------------------------------------------------------------------
;;; Projekt  : APPLY - A Practicable And Portable Lisp Implementation
;;;            ------------------------------------------------------
;;; Funktion : Definition der Function do-clicc zum Aufruf von CLICC
;;;            Laden der uebrigen Module
;;;
;;; $Revision: 1.51 $
;;; $Log: clcmain.lisp,v $
;;; Revision 1.51  1994/06/07  17:29:54  jh
;;; prepare-export-write und simplify-params eingebaut.
;;;
;;; Revision 1.50  1994/05/01  23:10:02  hk
;;; "Preparing Module Interface Specification" nur wenn *OPTIMIZE*
;;;
;;; Revision 1.49  1994/04/05  15:02:22  jh
;;; prepare-export-write eingebaut.
;;;
;;; Revision 1.48  1994/02/18  14:03:28  hk
;;; look-for-type-errors wird auch ausgef"uhrt, wenn Inlining angeschaltet ist.
;;; Bei der ersten Optimierung wird *optimize-verbosity* auf 2 erh"oht, da
;;; es verd"achtig ist, wenn bereits so fr"uh wesentliches optimiert
;;; werden kann.
;;;
;;; Revision 1.47  1994/02/08  11:05:37  sma
;;; Neue Funktion clicc-message-line zeichnet die übliche Trennline.
;;;
;;; Revision 1.46  1994/01/24  16:30:11  sma
;;; Während der Kompilation des inline-modules wird nicht versucht, dem
;;; Symbol den Typ T_SYMBOL_T zuzuweisen.
;;;
;;; Revision 1.45  1993/12/30  09:27:32  hk
;;; Tail-Rekursions-Eliminierung nur, wenn (= *NERRORS* 0).
;;;
;;; Revision 1.44  1993/12/28  13:23:44  jh
;;; Weitere Aufrufe von search-and-delete-unused-objects eingebaut und
;;; den Aufruf von pass_3 vor die Seiteneffektanalyse verschoben.
;;;
;;; Revision 1.43  1993/12/22  11:20:24  atr
;;; Der Aufruf der Tail-Rekursion vor Pass_3 eingefügt.
;;;
;;; Revision 1.42  1993/12/16  15:21:21  hk
;;; Rücksetzen des gensym-counter von clcload nach clcmain, damit es auch
;;; bei dem standalone CLiCC wirkt und man möglichst gleiche Symbole
;;; erhält.
;;;
;;; Revision 1.41  1993/11/05  14:20:50  hk
;;; require an den Dateianfang verschoben, um Probleme mit akcl zu vermeiden.
;;;
;;; Revision 1.40  1993/10/18  12:49:24  jh
;;; Der Typ des Symbols T wird vor den Optimierungen von SYMBOL auf
;;; T-SYMBOL verbessert.
;;;
;;; Revision 1.39  1993/09/20  14:14:42  jh
;;; An die neue Version des Inlinings angepasst (wiederholter Aufruf von pss_3).
;;;
;;; Revision 1.38  1993/09/09  09:53:08  jh
;;; Wegen eines Fehlers in der Typinferenz *no-to* auf t gesetzt.
;;;
;;; Revision 1.37  1993/09/06  10:02:05  jh
;;; *OPTIMIZE* schaltet jetzt die gesamten Optimierungen und die hierfuer
;;; notwendigen Analysen an und aus.
;;;
;;; Revision 1.36  1993/07/23  13:37:43  hk
;;; *ti-verbosity* bei der zusaetzlichen ersten Typinferenz auf
;;; (min *ti-verbosity* 1)
;;;
;;; Revision 1.35  1993/07/23  13:35:35  hk
;;; *ti-verbosity* bei der zusaetzlichen ersten Typinferenz auf 1
;;;
;;; Revision 1.34  1993/07/22  09:53:11  jh
;;; Einfache Typinference (*ti-level* 0) den ersten Optimierungen vorgeschaltet.
;;;
;;; Revision 1.33  1993/07/11  13:32:46  kl
;;; Iteration der interagierenden Analysen und Codeverbesserungen auf der
;;; Zwischensprache eingebaut. Die Anzahl der Iterationsschritte wird ueber
;;; die globale Variable *ITERATIONS* gesteuert.
;;;
;;; Revision 1.32  1993/07/08  10:47:28  jh
;;; Einfache Optimierungen, die auf der Seiteneffektanalyse beruhen, eingebaut.
;;;
;;; Revision 1.31  1993/06/30  15:25:03  jh
;;; An die neue Version der Optimierungen angepasst (optimain.lisp). Dadurch
;;; wird p2main.lisp ueberfluessig.
;;;
;;; Revision 1.30  1993/06/19  21:15:48  hk
;;; *clicc-version* wird bei show-version mit angezeigt.
;;;
;;; Revision 1.29  1993/06/19  21:06:53  hk
;;; Copyright bei show-verion.
;;;
;;; Revision 1.28  1993/06/17  08:00:09  hk
;;; Copright Notiz eingefuegt
;;;
;;; Revision 1.27  1993/06/10  10:35:20  kl
;;; Die Annotation fuer die Codegenerierung wird nun wieder direkt vor der
;;; Codegenerierung durchgefuehrt.
;;;
;;; Revision 1.26  1993/05/18  08:58:15  kl
;;; Bei eingeschaltetem Inlining werden keine Typwarnungen mehr ausgegeben.
;;;
;;; Revision 1.25  1993/05/06  13:32:16  kl
;;; Typwarnungen werden jetzt erst nach der zweiten Optimierungsphase erzeugt.
;;;
;;; Revision 1.24  1993/05/03  14:28:44  hk
;;; (require static-effect) eingefuegt.
;;;
;;; Revision 1.23  1993/04/30  09:15:19  jh
;;; search-fun-calls eingebaut.
;;;
;;; Revision 1.22  1993/04/22  12:31:10  hk
;;; *CLICC-LISP-PROGRAM* gestrichen, keine def Datei fuer das Inline Modul.
;;;
;;; Revision 1.21  1993/04/21  11:34:15  jh
;;; (require tomain) eingetragen.
;;;
;;; Revision 1.20  1993/04/21  11:32:37  jh
;;; Aufruf von do-to eingebaut.
;;;
;;; Revision 1.19  1993/04/03  09:57:11  hk
;;; Bei Modulkompilation wird ein .def File geschrieben.
;;;
;;; Revision 1.18  1993/03/22  10:15:55  ft
;;; Versions-Meldung des Compilers gekuerzt.
;;;
;;; Revision 1.17  1993/03/18  15:14:25  ft
;;; Neuer Schalter *NO-SIDE-EFFECT-ANALYSIS*.
;;;
;;; Revision 1.16  1993/03/12  10:58:39  ft
;;; Aufruf der Seiteneffektanalyse nach do-clicc verlagert.
;;;
;;; Revision 1.15  1993/03/10  09:08:38  ft
;;; Einige Meldungen in do-clicc von format auf clicc-message umgestellt.
;;;
;;; Revision 1.14  1993/03/05  16:34:03  kl
;;; Aufruf der Typinferenz eingebaut.
;;;
;;; Revision 1.13  1993/02/16  15:40:45  hk
;;; Revision Keyword, (require delete) eingefuegt, Packg. RUNTIME gestrichen.
;;;
;;; Revision 1.12  1993/01/26  18:36:04  kl
;;; Typinferenz und Traversierungsalgorithmus eingebunden.
;;;
;;; Revision 1.11  1993/01/02  12:55:51  kl
;;; *NO-CODEGEN* zum Abschalten der Codegenerierung eingebaut.
;;;
;;; Revision 1.10  1992/11/17  12:11:54  ft
;;; (require p1env) verschoben.
;;;
;;; Revision 1.9  1992/10/12  09:04:06  hk
;;; Meldungen zu den Passes vereinheitlicht.
;;;
;;; Revision 1.8  1992/10/07  17:37:51  hk
;;; Neue Variable *OPTIMIZE* bestimmt, optimiert wird.
;;;
;;; Revision 1.7  1992/09/29  21:08:04  hk
;;; catch 'ABORT entfernt, Ausgabe von Meldungen ueber die Passes zentralisiert,
;;; Fehlerzaehlen zentralisiert.
;;;
;;; Revision 1.6  1992/08/14  08:43:30  kl
;;; *CLICC-Version*
;;;
;;; Revision 1.5  1992/08/07  11:38:16  hk
;;; Dateikopf verschoenert.
;;;
;;; Revision 1.4  1992/08/06  17:13:13  kl
;;; (p0-init) wird jetzt im umgebenden (catch 'ABORT ...) ausgefuehrt.
;;;
;;; Revision 1.3  1992/08/05  13:12:04  hk
;;; Versionsnummer erhoeht.
;;;
;;; Revision 1.2  1992/06/04  07:11:20  hk
;;; Nach Umstellung auf die Lisp nahe Zwischensprache, Syntax-Fehler
;;; sind schon beseitigt
;;;
;;; Revision 1.1  1992/03/24  16:54:56  hk
;;; Initial revision
;;;-----------------------------------------------------------------------------

(in-package "CLICC")

(require "strconst")
(require "clcdef")
(require "clcmisc")
(require "zsdef")
(require "ffzsdef")
(require "zsops")
(require "clcmisc")
(require "config")
(require "appfuns")
(require "p1env")
(require "p0init")
(require "p1main")
(require "traverse")
(require "inline")
(require "delete")
(require "static-effect")
(require "p3main")
(require "timain")
(require "titypes")
(require "optimain")
(require "cgmain")
(require "deffile")


;;------------------------------------------------------------------------------
;; Der Aufruf des Compilers.
;; Resultat: T, wenn Fehler auftraten, NIL sonst.
;;------------------------------------------------------------------------------
(defun do-clicc ()
  
  ;; Damit die Zahlen klein bleiben.
  ;;--------------------------------
  (gensym 0)                            ; für CltL1
  (setq *gensym-counter* 1)             ; für CLtL2

  (multiple-value-setq (*FILENAME* *EXTENSION*)
    (split-name-ext *FILENAME*))
  (unless *OUT-FILENAME*
    (setq *OUT-FILENAME* *FILENAME*))

  (when *SHOW-VERSION*
    (format t "~&~
;;; CLiCC, the Common Lisp to C Compiler --- Version ~a~%~
;;; Copyright (C) 1993 Christian-Albrechts-Universitaet zu Kiel~%~%"
            *CLICC-VERSION*))

  (setq *NERRORS* 0 *NWARNINGS* 0)
  (p0-init)
    
  (when (= *NERRORS* 0)
    (clicc-message "Syntactic and semantic analysis")
    (pass_1))

  (unless *ONLY-PASS1*

    ;; Das Symbol T hat zunaechst den Typ SYMBOL. Wenn es von irgendwelchen
    ;; Optimierungen ins Programm eingebaut wird, sollte es den genaueren Typ
    ;; T-SYMBOL haben, um gleich weitere Optimierungen zu ermoeglichen.
    (unless *INLINE-MODULE*
      (setf (?type (get-symbol-bind 'L::T)) T-SYMBOL-T))

    (if *OPTIMIZE*
        (progn
          (when (= *NERRORS* 0)
            (search-and-delete-unused-objects))

          (when (and (= *NERRORS* 0)
                     (= *ITERATIONS* 1)
                     (not *no-inlining*))
            (simplify-params #'used-dynamic-vars-simple))
          
          ;; Die Tail-Rekursion. 
          ;; ACHTUNG Die Tail-Rekursion soll vor PASS_3 laufen,
          ;; da hier continuations erzeugt werden, die noch in 
          ;; PASS_3 attributiert werden müssen.
          (when (= *NERRORS* 0)
            (unless *no-tail-recursion*
              (tail-rec-module)))
          
          ;; Aufruf einer sehr einfachen Typinferenz
          (when (= *NERRORS* 0)
            (let ((*ti-level* 0)
                  (*ti-verbosity* (min *ti-verbosity* 1)))
              (do-ti)))
          
          (when (= *NERRORS* 0)
            (pass_3))                   ; level-, mv-Slots etc. setzen.
          
          (when (= *NERRORS* 0)
            (let ((*no-seo* t)

                  ;; Wenn so fr"uh bereits wesentliches optimiert werden
                  ;; kann, dann ist es verd"achtig
                  ;;------------------------------
                  (*optimize-verbosity* (max *optimize-verbosity* 2)))
              (do-optimization)))

          (when (= *NERRORS* 0)
            (search-and-delete-unused-objects))
        
          (dotimes (iteration *ITERATIONS*)
            (when (> *ITERATIONS* 1)
              (clicc-message-line 28)
              (clicc-message "~D. iteration step" (1+ iteration))
              (clicc-message-line 28)
              (clicc-message " "))
        
            (when (= *NERRORS* 0)
              (do-inline))
        
            (when (= *NERRORS* 0)
              (search-and-delete-unused-objects))
        
            (when (= *NERRORS* 0)
              (search-fun-calls))
        
            (when (= *NERRORS* 0)
              (pass_3))
            
            ;; Aufruf der Seiteneffektanalyse
            (unless *NO-SIDE-EFFECT-ANALYSIS*
              (when (= *NERRORS* 0)
                (analyse-module)))
        
            ;; Aufruf der Typinferenz
            (when (= *NERRORS* 0)
              (reset-type-annotations)
              (do-ti))
            
            (when (= *NERRORS* 0)
              (do-optimization))
            
            (when (= *NERRORS* 0)
              (search-and-delete-unused-objects))

            (when (and (= *NERRORS* 0)
                       (> *ITERATIONS* 1)
                       (= iteration 0)
                       (not *no-inlining*))
              (simplify-params #'used-dynamic-vars-with-side-effects)
              (pass_3)))
        
          ;; Suche nach Typfehlern
          (when (= *NERRORS* 0)
            (look-for-type-errors *module*))

          (when (and *module-compiler*
                     (not *inline-module*)
                     (not *no-inlining*)
                     (= 0 *NERRORS*))
              (clicc-message "Preparing Module Interface Specification")
              (prepare-export-write)))

        ;; Wenn nicht optimiert wird, muessen zumindest die named-consts,
        ;; die als Referenzen auf Funktionen gedient haben, ersetzt werden.
        ;; Ausserdem muessen noch die angewandten Vorkommen gezaehlt werden.
        (let ((*no-to* t)
              (*no-seo* t)
              (*no-simp* t))
          (set-used-slots-for-cg)
          (pass_3)
          (do-optimization)))
    
    (when (= *NERRORS* 0)
      (clicc-message "Annotation for code generation")
      (pass_3))

    (unless *NO-CODEGEN*
      (when (= *NERRORS* 0)
        (clicc-message "Code Generation")
        (codegen)))

    (when (and *module-compiler* (not *inline-module*))
      (when (= *NERRORS* 0)
        (clicc-message "Writing Module Interface Specification")
        (export-write))))

  (when (or (> *NERRORS* 0) (> *NWARNINGS* 0))
    (format t ";;; ~D Errors, ~D Warnings~%" *NERRORS*  *NWARNINGS*))
  (terpri)

  (> *NERRORS* 0))

;;------------------------------------------------------------------------------
(provide "clcmain")
