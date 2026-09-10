;;; ============================================================
;;; extraction.lsp — Äèñïåò÷åð EXTRACTION / ÝÊÑÒÐÀÊÖÈß
;;;
;;; Âîçìîæíîñòè:
;;;   - êíîïêè "Âûáðàòü âñå" / "Ñíÿòü âûäåëåíèå" äëÿ ñëî¸â;
;;;   - ôèëüòðû Ôàñàä / Âèòðàæ / Ôîíàðü / Àíîíèìíûå áëîêè;
;;;   - ñèíõðîíèçàöèÿ ÷åêáîêñîâ Ïîäñèñòåìû;
;;;   - ñîõðàíåíèå ïðåäâàðèòåëüíîãî âûáîðà;
;;;   - çàäà÷è: Ôàñîíêà, Ïîäñèñòåìà, Îáëèöîâêà, Âèòðàæ,
;;;     Çàïîëíåíèå;
;;;   - ìîäóëü "Ïåðåèìåíîâàòåëü" (blockrename).
;;;
;;; Ïåðåèìåíîâàíèå áëîêîâ — òîëüêî ïî êíîïêå "Ïåðåèìåíîâàòü".
;;; Enter â ïîëå "Íîâîå èìÿ" ÍÅ âûçûâàåò ïåðåèìåíîâàíèå.
;;; ============================================================

(vl-load-com)

;; ============================================================
;; ÏÎÑËÅÄÍÈÅ ÍÀÑÒÐÎÉÊÈ
;; ============================================================

(if (not (boundp '*EXTRACTION-LAST-TASK*))
  (setq *EXTRACTION-LAST-TASK* 'FASONKA)
)

(if (not (boundp '*EXTRACTION-LAST-REPORT-MODE*))
  (setq *EXTRACTION-LAST-REPORT-MODE* "DETAIL")
)

(if (not (boundp '*EXTRACTION-LAST-EXPORT-EXCEL*))
  (setq *EXTRACTION-LAST-EXPORT-EXCEL* nil)
)

(if (not (boundp '*EXTRACTION-LAST-EXPORT-TXT*))
  (setq *EXTRACTION-LAST-EXPORT-TXT* nil)
)

(if (not (boundp '*EXTRACTION-LAST-CREATE-TABLE*))
  (setq *EXTRACTION-LAST-CREATE-TABLE* T)
)

;; Ïîñëåäíèå ñëîè Ôàñîíêè
(if (not (boundp '*EXTRACTION-LAST-FASONKA-LAYERS*))
  (setq *EXTRACTION-LAST-FASONKA-LAYERS* nil)
)

;; Ïîñëåäíèå ñëîè Ïîäñèñòåìû
(if (not (boundp '*EXTRACTION-LAST-SUBSYSTEM-LAYERS*))
  (setq
    *EXTRACTION-LAST-SUBSYSTEM-LAYERS*
    '("Ïîäñèñòåìà"
      "Ïîäñèñòåìà àëþìèíèåâàÿ"
      "Ïîäñèñòåìà îöèíêîâàííàÿ")
  )
)

;; Ñîñòîÿíèå òð¸õ ÷åêáîêñîâ Ïîäñèñòåìû
(if (not (boundp '*EXTRACTION-LAST-SUBSYSTEM-CHECKS*))
  (setq *EXTRACTION-LAST-SUBSYSTEM-CHECKS* '(T T T))
)

;; Ïîñëåäíèå ôèëüòðû
(if (not (boundp '*EXTRACTION-LAST-FILTER-FACADES*))
  (setq *EXTRACTION-LAST-FILTER-FACADES* nil)
)

(if (not (boundp '*EXTRACTION-LAST-FILTER-VITRAZH*))
  (setq *EXTRACTION-LAST-FILTER-VITRAZH* nil)
)

(if (not (boundp '*EXTRACTION-LAST-FILTER-FONAR*))
  (setq *EXTRACTION-LAST-FILTER-FONAR* nil)
)

;; Ïåðåìåííàÿ äëÿ ñîõðàíåíèÿ ïðåäâàðèòåëüíîãî âûáîðà
(if (not (boundp '*extraction-preselected-set*))
  (setq *extraction-preselected-set* nil)
)

;; Ôëàã: èä¸ò ñèíõðîíèçàöèÿ ÷åêáîêñîâ èç îáðàáîò÷èêà ÷åêáîêñà
(if (not (boundp '*extraction-syncing-checks*))
  (setq *extraction-syncing-checks* nil)
)


;; ============================================================
;; ÐÀÁÎ×ÅÅ ÑÎÑÒÎßÍÈÅ
;; ============================================================

(setq *EXTRACTION-DCL-ID* nil)
(setq *EXTRACTION-ALL-LAYERS* nil)
(setq *EXTRACTION-VISIBLE-LAYERS* nil)
(setq *EXTRACTION-SELECTED-LAYERS* nil)
(setq *EXTRACTION-SELECTED-INDICES* nil)

(setq *EXTRACTION-FILTER-FACADES* nil)
(setq *EXTRACTION-FILTER-VITRAZH* nil)
(setq *EXTRACTION-FILTER-FONAR* nil)

(setq *EXTRACTION-TASK-ID* *EXTRACTION-LAST-TASK*)
(setq *EXTRACTION-REPORT-MODE* *EXTRACTION-LAST-REPORT-MODE*)
(setq *EXTRACTION-EXPORT-EXCEL* *EXTRACTION-LAST-EXPORT-EXCEL*)
(setq *EXTRACTION-EXPORT-TXT* *EXTRACTION-LAST-EXPORT-TXT*)
(setq *EXTRACTION-CREATE-TABLE* *EXTRACTION-LAST-CREATE-TABLE*)

(setq *EXTRACTION-ACTION* 'CANCEL)


;; ============================================================
;; ÓÍÈÊÀËÜÍÛÅ ÑÒÐÎÊÈ
;; ============================================================

(defun extraction-unique-ci (lst / out x key)
  (setq out '())
  (if (listp lst)
    (foreach x lst
      (if (= (type x) 'STR)
        (progn
          (setq key (strcase x))
          (if (not (vl-some '(lambda (y) (= (strcase y) key)) out))
            (setq out (append out (list x)))
          )
        )
      )
    )
  )
  out
)


;; ============================================================
;; ÏÎËÓ×ÅÍÈÅ ÂÑÅÕ ÑËÎ¨Â ×ÅÐÒÅÆÀ
;; ============================================================

(defun extraction-layer-names ( / acad doc layers out name)
  (setq out '())
  (setq acad (vl-catch-all-apply 'vlax-get-acad-object '()))

  (if (and (not (vl-catch-all-error-p acad)) acad)
    (progn
      (setq doc
        (vl-catch-all-apply 'vla-get-ActiveDocument (list acad)))

      (if (and (not (vl-catch-all-error-p doc)) doc)
        (progn
          (setq layers
            (vl-catch-all-apply 'vla-get-Layers (list doc)))

          (if (and (not (vl-catch-all-error-p layers)) layers)
            (vlax-for lay layers
              (setq name
                (vl-catch-all-apply 'vla-get-Name (list lay)))

              (if (and
                    (not (vl-catch-all-error-p name))
                    (= (type name) 'STR)
                    (> (strlen name) 0))
                (setq out (cons name out))
              )
            )
          )
        )
      )
    )
  )

  (setq out
    (vl-remove-if-not '(lambda (x) (= (type x) 'STR)) out))

  (setq out
    (vl-sort out '(lambda (a b) (< (strcase a) (strcase b)))))

  (extraction-unique-ci out)
)


;; ============================================================
;; ÇÀÃÐÓÇÊÀ ÌÎÄÓËÅÉ
;; ============================================================

(defun extraction-project-root ( / dsp)
  (setq dsp (findfile "extraction.lsp"))
  (if dsp
    (vl-filename-directory (vl-filename-directory dsp))
    nil
  )
)


(defun extraction-modules-dir ( / dsp)
  (setq dsp (findfile "extraction.lsp"))
  (if dsp
    (vl-filename-directory dsp)
    nil
  )
)


(defun extraction-load-all ( / root common f path)
  (setq root (extraction-project-root))

  (if root
    (progn

      ;; --------------------------------------------------------
      ;; COMMON
      ;; --------------------------------------------------------

      (setq common (strcat root "\\common\\"))

      (foreach f
        '("task-utils.lsp"
          "layer-utils.lsp"
          "select-utils.lsp"
          "excel-utils.lsp"
          "table-utils.lsp"
          "txt-utils.lsp")

        (setq path (strcat common f))

        (if (findfile path)
          (load path)
          (princ (strcat "\n[EXTRACTION] Íå íàéäåí: " path))
        )
      )

      ;; --------------------------------------------------------
      ;; EXTRACTION / FASONKA
      ;; --------------------------------------------------------

      (setq path (strcat root "\\Extraction\\fasonka.lsp"))
      (if (findfile path)
        (load path)
        (princ (strcat "\n[EXTRACTION] Íå íàéäåí: " path)))

      ;; --------------------------------------------------------
      ;; EXTRACTION / SUBSYSTEM
      ;; --------------------------------------------------------

      (setq path (strcat root "\\Extraction\\subsystem.lsp"))
      (if (findfile path)
        (load path)
        (princ (strcat "\n[EXTRACTION] Íå íàéäåí: " path)))

      ;; --------------------------------------------------------
      ;; EXTRACTION / CUTLINE
      ;; --------------------------------------------------------

      (setq path (strcat root "\\Extraction\\cutline.lsp"))
      (if (findfile path)
        (load path)
        (princ (strcat "\n[EXTRACTION] Íå íàéäåí: " path)))

      ;; --------------------------------------------------------
      ;; EXTRACTION / CUTSHEET
      ;; --------------------------------------------------------

      (setq path (strcat root "\\Extraction\\cutsheet.lsp"))
      (if (findfile path)
        (load path)
        (princ (strcat "\n[EXTRACTION] Íå íàéäåí: " path)))

      ;; --------------------------------------------------------
      ;; EXTRACTION / ZAPOLNENIE
      ;; --------------------------------------------------------

      (setq path (strcat root "\\Extraction\\zapolnenie.lsp"))
      (if (findfile path)
        (load path)
        (princ (strcat "\n[EXTRACTION] Íå íàéäåí: " path)))

      ;; --------------------------------------------------------
      ;; EXTRACTION / BLOCKRENAME
      ;; --------------------------------------------------------

      (setq path (strcat root "\\Extraction\\blockrename.lsp"))
      (if (findfile path)
        (load path)
        (princ (strcat "\n[EXTRACTION] Íå íàéäåí: " path)))
    )

    (princ "\n[EXTRACTION] Êîðåíü ïðîåêòà íå íàéäåí.")
  )

  T
)


;; ============================================================
;; ÐÀÇÁÎÐ ÈÍÄÅÊÑÎÂ DCL
;; ============================================================

(defun extraction-parse-indices (s / x)
  (if (and (= (type s) 'STR) (/= s ""))
    (progn
      (setq x (read (strcat "(" s ")")))
      (if (= (type x) 'LIST) x nil)
    )
    nil
  )
)


;; ============================================================
;; ÏÎËÓ×ÅÍÈÅ ÂÛÁÐÀÍÍÛÕ ÑËÎ¨Â
;; ============================================================

(defun extraction-selected-names ( / s indices out i n)
  (setq s (get_tile "lst_layers"))

  (if (or (null s) (/= (type s) 'STR) (= s ""))
    nil
    (progn
      (setq indices (extraction-parse-indices s))
      (setq out '())
      (setq n (length *EXTRACTION-VISIBLE-LAYERS*))

      (foreach i indices
        (if (and (numberp i) (>= i 0) (< i n))
          (setq out (cons (nth i *EXTRACTION-VISIBLE-LAYERS*) out))
        )
      )

      (extraction-unique-ci out)
    )
  )
)


;; ============================================================
;; ÔÈËÜÒÐÀÖÈß ÑËÎ¨Â ÏÎ ÊËÞ×ÅÂÛÌ ÑËÎÂÀÌ
;; ============================================================

(defun extraction-filter-layers-by-keywords (keywords / filters result f fname)
  (setq filters
    (vl-catch-all-apply 'tu-group-filter-names-and-layers '()))

  (if (vl-catch-all-error-p filters)
    (setq filters nil))

  (setq result '())

  (if (listp filters)
    (foreach f filters
      (setq fname (car f))

      (if (and
            (= (type fname) 'STR)
            (vl-some
              '(lambda (k)
                 (and
                   (= (type k) 'STR)
                   (vl-string-search (strcase k) (strcase fname))))
              keywords))
        (setq result (append result (cadr f)))
      )
    )
  )

  (extraction-unique-ci result)
)


;; ============================================================
;; ÏÅÐÅÑÒÐÎÅÍÈÅ ÑÏÈÑÊÀ ÑËÎ¨Â
;; ============================================================

(defun extraction-rebuild-layer-list ( / vis keywords)
  (setq *EXTRACTION-SELECTED-INDICES* '())
  (setq keywords '())

  (if *EXTRACTION-FILTER-FACADES*
    (setq keywords (cons "ôàñàä" keywords)))

  (if *EXTRACTION-FILTER-VITRAZH*
    (setq keywords (cons "âèòðàæ" keywords)))

  (if *EXTRACTION-FILTER-FONAR*
    (setq keywords (cons "ôîíàð" keywords)))

  (if keywords
    (progn
      (setq vis (extraction-filter-layers-by-keywords keywords))
      (if (or (null vis) (not (listp vis)))
        (setq vis *EXTRACTION-ALL-LAYERS*))
      (setq *EXTRACTION-VISIBLE-LAYERS* vis)
    )
    (setq *EXTRACTION-VISIBLE-LAYERS* *EXTRACTION-ALL-LAYERS*)
  )

  (setq *EXTRACTION-VISIBLE-LAYERS*
    (vl-sort
      (vl-remove-if-not
        '(lambda (x) (= (type x) 'STR))
        (if (listp *EXTRACTION-VISIBLE-LAYERS*)
          *EXTRACTION-VISIBLE-LAYERS*
          '()))
      '(lambda (a b) (< (strcase a) (strcase b)))))

  (start_list "lst_layers")
  (mapcar 'add_list *EXTRACTION-VISIBLE-LAYERS*)
  (end_list)
  (set_tile "lst_layers" "")
  (extraction-update-select-buttons)
)


;; ============================================================
;; ÑÏÅÖÈÀËÜÍÛÉ ÑÏÈÑÎÊ ÏÎÄÑÈÑÒÅÌÛ
;; ============================================================

(defun extraction-subsystem-layer-list ()
  (setq *EXTRACTION-VISIBLE-LAYERS*
    '("Ïîäñèñòåìà"
      "Ïîäñèñòåìà àëþìèíèåâàÿ"
      "Ïîäñèñòåìà îöèíêîâàííàÿ"))

  (start_list "lst_layers")
  (mapcar 'add_list *EXTRACTION-VISIBLE-LAYERS*)
  (end_list)
  (set_tile "lst_layers" "")
  (extraction-update-select-buttons)
)


;; ============================================================
;; Î×ÈÑÒÊÀ ÂÛÁÎÐÀ
;; ============================================================

(defun extraction-clear-layer-selection ()
  (set_tile "lst_layers" "")
  (extraction-layer-selection-changed)
)


;; ============================================================
;; ÓÑÒÀÍÎÂÊÀ ÂÛÄÅËÅÍÈß ÑËÎ¨Â
;; ============================================================

(defun extraction-select-layers-in-list
       (layers-to-select / i item selected str after-set)

  (setq selected '())
  (setq i 0)

  (foreach item *EXTRACTION-VISIBLE-LAYERS*
    (if (vl-some
          '(lambda (x) (= (strcase x) (strcase item)))
          layers-to-select)
      (setq selected (cons i selected)))
    (setq i (1+ i))
  )

  (setq selected (reverse selected))
  (setq str "")

  (foreach i selected
    (setq str
      (if (= str "")
        (itoa i)
        (strcat str " " (itoa i))))
  )

  (set_tile "lst_layers" str)
  (setq after-set (get_tile "lst_layers"))

  (if (/= str after-set)
    (progn
      (mode_tile "lst_layers" 1)
      (mode_tile "lst_layers" 0)
      (set_tile "lst_layers" str)
      (setq after-set (get_tile "lst_layers"))
    )
  )

  (if (/= str after-set)
    (progn
      (start_list "lst_layers")
      (mapcar 'add_list *EXTRACTION-VISIBLE-LAYERS*)
      (end_list)
      (set_tile "lst_layers" str)
      (setq after-set (get_tile "lst_layers"))
    )
  )

  (extraction-layer-selection-changed)
  (extraction-update-select-buttons)
)


;; ============================================================
;; ÏÐÎÂÅÐÊÀ, ÂÑÅ ËÈ ÂÈÄÈÌÛÅ ÑËÎÈ ÂÛÁÐÀÍÛ
;; ============================================================

(defun extraction-all-layers-selected-p ( / selected total)
  (setq selected (extraction-selected-names))
  (setq total (length *EXTRACTION-VISIBLE-LAYERS*))
  (and selected (= (length selected) total))
)


;; ============================================================
;; ÎÁÍÎÂËÅÍÈÅ ÀÊÒÈÂÍÎÑÒÈ ÊÍÎÏÎÊ ÂÛÁÎÐÀ
;; ============================================================

(defun extraction-update-select-buttons ()
  (if (extraction-all-layers-selected-p)
    (progn
      (mode_tile "btn_select_all" 1)
      (mode_tile "btn_clear_all" 0)
    )
    (progn
      (mode_tile "btn_select_all" 0)
      (mode_tile "btn_clear_all" 1)
    )
  )
)


;; ============================================================
;; ÂÛÁÐÀÒÜ ÂÑÅ ÂÈÄÈÌÛÅ ÑËÎÈ
;; ============================================================

(defun extraction-select-all-layers ( / str i)
  (setq str "")
  (setq i 0)

  (foreach item *EXTRACTION-VISIBLE-LAYERS*
    (setq str
      (if (= str "")
        (itoa i)
        (strcat str " " (itoa i))))
    (setq i (1+ i))
  )

  (extraction-select-layers-in-list *EXTRACTION-VISIBLE-LAYERS*)
)


;; ============================================================
;; ÑÍßÒÜ ÂÛÄÅËÅÍÈÅ ÑÎ ÂÑÅÕ ÑËÎ¨Â
;; ============================================================

(defun extraction-clear-all-layers ()
  (set_tile "lst_layers" "")
  (extraction-layer-selection-changed)
  (extraction-update-select-buttons)
)


;; ============================================================
;; ÑÈÍÕÐÎÍÈÇÀÖÈß ×ÅÊÁÎÊÑÎÂ ÏÎÄÑÈÑÒÅÌÛ
;; ============================================================

(defun extraction-sync-checks-from-layers ( / selected)
  (if *extraction-syncing-checks*
    nil
    (progn
      (setq selected (extraction-selected-names))

      (set_tile "chk_subsystem_1"
        (if (member "Ïîäñèñòåìà" selected) "1" "0"))

      (set_tile "chk_subsystem_2"
        (if (member "Ïîäñèñòåìà àëþìèíèåâàÿ" selected) "1" "0"))

      (set_tile "chk_subsystem_3"
        (if (member "Ïîäñèñòåìà îöèíêîâàííàÿ" selected) "1" "0"))
    )
  )
)


;; ============================================================
;; ÈÇÌÅÍÅÍÈÅ ×ÅÊÁÎÊÑÀ ÏÎÄÑÈÑÒÅÌÛ
;; ============================================================

(defun extraction-subsystem-check-changed
       (key / val layers-to-select current-selection)

  (setq *extraction-syncing-checks* T)

  (setq val
    (= (get_tile
         (strcat "chk_subsystem_" (itoa key))) "1"))

  (setq *EXTRACTION-LAST-SUBSYSTEM-CHECKS*
    (list
      (if (= key 1) val (car *EXTRACTION-LAST-SUBSYSTEM-CHECKS*))
      (if (= key 2) val (cadr *EXTRACTION-LAST-SUBSYSTEM-CHECKS*))
      (if (= key 3) val (caddr *EXTRACTION-LAST-SUBSYSTEM-CHECKS*))))

  (setq layers-to-select '())

  (if (car *EXTRACTION-LAST-SUBSYSTEM-CHECKS*)
    (setq layers-to-select (cons "Ïîäñèñòåìà" layers-to-select)))

  (if (cadr *EXTRACTION-LAST-SUBSYSTEM-CHECKS*)
    (setq layers-to-select (cons "Ïîäñèñòåìà àëþìèíèåâàÿ" layers-to-select)))

  (if (caddr *EXTRACTION-LAST-SUBSYSTEM-CHECKS*)
    (setq layers-to-select (cons "Ïîäñèñòåìà îöèíêîâàííàÿ" layers-to-select)))

  (setq current-selection (extraction-selected-names))

  (setq current-selection
    (vl-remove-if
      '(lambda (x)
         (or (= (strcase x) "ÏÎÄÑÈÑÒÅÌÀ")
             (= (strcase x) "ÏÎÄÑÈÑÒÅÌÀ ÀËÞÌÈÍÈÅÂÀß")
             (= (strcase x) "ÏÎÄÑÈÑÒÅÌÀ ÎÖÈÍÊÎÂÀÍÍÀß")))
      current-selection))

  (setq current-selection
    (append current-selection layers-to-select))

  (extraction-select-layers-in-list current-selection)

  (setq *extraction-syncing-checks* nil)
)


;; ============================================================
;; ÏÅÐÅÊËÞ×ÅÍÈÅ ÇÀÄÀ×È
;; ============================================================

(defun extraction-toggle-subsystem-layers ( / layers-to-select)
  (if (= (get_tile "rb_task_subsystem") "1")
    (progn
      (mode_tile "box_subsystem_layers" 0)
      (mode_tile "chk_subsystem_1" 0)
      (mode_tile "chk_subsystem_2" 0)
      (mode_tile "chk_subsystem_3" 0)

      (extraction-rebuild-layer-list)

      (setq layers-to-select *EXTRACTION-LAST-SUBSYSTEM-LAYERS*)

      (if (null layers-to-select)
        (progn
          (setq layers-to-select '())

          (if (car *EXTRACTION-LAST-SUBSYSTEM-CHECKS*)
            (setq layers-to-select (cons "Ïîäñèñòåìà" layers-to-select)))

          (if (cadr *EXTRACTION-LAST-SUBSYSTEM-CHECKS*)
            (setq layers-to-select (cons "Ïîäñèñòåìà àëþìèíèåâàÿ" layers-to-select)))

          (if (caddr *EXTRACTION-LAST-SUBSYSTEM-CHECKS*)
            (setq layers-to-select (cons "Ïîäñèñòåìà îöèíêîâàííàÿ" layers-to-select)))
        )
      )

      (extraction-select-layers-in-list layers-to-select)
      (extraction-sync-checks-from-layers)
    )
    (progn
      (mode_tile "box_subsystem_layers" 1)
      (mode_tile "chk_subsystem_1" 1)
      (mode_tile "chk_subsystem_2" 1)
      (mode_tile "chk_subsystem_3" 1)

      (extraction-rebuild-layer-list)

      (if *EXTRACTION-LAST-FASONKA-LAYERS*
        (extraction-select-layers-in-list *EXTRACTION-LAST-FASONKA-LAYERS*))
    )
  )
)


;; ============================================================
;; ÈÇÌÅÍÅÍÈÅ ÂÛÁÎÐÀ Â ÑÏÈÑÊÅ
;; ============================================================

(defun extraction-layer-selection-changed ( / selected)
  (setq selected (extraction-selected-names))
  (setq *EXTRACTION-SELECTED-LAYERS* selected)

  (if (eq *EXTRACTION-TASK-ID* 'SUBSYSTEM)
    (progn
      (setq *EXTRACTION-LAST-SUBSYSTEM-LAYERS* selected)
      (extraction-sync-checks-from-layers)
    )
    (setq *EXTRACTION-LAST-FASONKA-LAYERS* selected)
  )

  (extraction-update-select-buttons)
)


(defun extraction-layer-selection ()
  (extraction-layer-selection-changed)
)


;; ============================================================
;; ×ÒÅÍÈÅ ÏÀÐÀÌÅÒÐÎÂ
;; ============================================================

(defun extraction-read-params ( / selected)

  (cond
    ((= (get_tile "rb_task_fasonka") "1")
     (setq *EXTRACTION-TASK-ID* 'FASONKA))

    ((= (get_tile "rb_task_subsystem") "1")
     (setq *EXTRACTION-TASK-ID* 'SUBSYSTEM))

    ((= (get_tile "rb_task_cladding") "1")
     (setq *EXTRACTION-TASK-ID* 'CLADDING))

    ((= (get_tile "rb_task_vitrazh") "1")
     (setq *EXTRACTION-TASK-ID* 'VITRAZH))

    ((= (get_tile "rb_task_zapolnenie") "1")
     (setq *EXTRACTION-TASK-ID* 'ZAPOLNENIE))

    (T
     (setq *EXTRACTION-TASK-ID* 'FASONKA))
  )

  (setq *EXTRACTION-LAST-TASK* *EXTRACTION-TASK-ID*)
  (setq selected (extraction-selected-names))

  (if (and (null selected)
           (or *EXTRACTION-FILTER-FACADES*
               *EXTRACTION-FILTER-VITRAZH*
               *EXTRACTION-FILTER-FONAR*))
    (setq selected *EXTRACTION-VISIBLE-LAYERS*)
  )

  (setq *EXTRACTION-SELECTED-LAYERS* selected)

  (if (eq *EXTRACTION-TASK-ID* 'SUBSYSTEM)
    (progn
      (setq *EXTRACTION-LAST-SUBSYSTEM-LAYERS* selected)

      (if (null selected)
        (progn
          (alert "Íå âûáðàíî íè îäíîãî ñëîÿ ïîäñèñòåìû.")
          nil
        )
        T
      )
    )
    (progn
      (setq *EXTRACTION-LAST-FASONKA-LAYERS* selected)
      T
    )
  )

  (setq *EXTRACTION-REPORT-MODE*
    (if (= (get_tile "rb_summary") "1") "SUMMARY" "DETAIL"))

  (setq *EXTRACTION-LAST-REPORT-MODE* *EXTRACTION-REPORT-MODE*)

  (setq *EXTRACTION-EXPORT-EXCEL*
    (= (get_tile "chk_xls") "1"))

  (setq *EXTRACTION-LAST-EXPORT-EXCEL* *EXTRACTION-EXPORT-EXCEL*)

  (setq *EXTRACTION-EXPORT-TXT*
    (= (get_tile "chk_txt") "1"))

  (setq *EXTRACTION-LAST-EXPORT-TXT* *EXTRACTION-EXPORT-TXT*)

  (setq *EXTRACTION-CREATE-TABLE*
    (= (get_tile "chk_acad") "1"))

  (setq *EXTRACTION-LAST-CREATE-TABLE* *EXTRACTION-CREATE-TABLE*)

  T
)


;; ============================================================
;; ÇÀÏÓÑÊ ÇÀÄÀ×È
;; ============================================================

(defun run-task
       (task-id layers report-mode export-excel export-txt
                create-table save-base / r)

  (cond

    ((eq task-id 'FASONKA)
     (setq r
       (vl-catch-all-apply 'fasonka-main
         (list layers report-mode export-excel export-txt
               create-table save-base)))

     (if (vl-catch-all-error-p r)
       (princ "\nÌîäóëü Ôàñîíêà íå çàãðóæåí èëè îøèáêà âûïîëíåíèÿ."))
    )

    ((eq task-id 'SUBSYSTEM)
     (setq r
       (vl-catch-all-apply 'subsystem-main
         (list layers report-mode export-excel export-txt
               create-table save-base)))

     (if (vl-catch-all-error-p r)
       (princ "\nÌîäóëü Ïîäñèñòåìà íå çàãðóæåí èëè îøèáêà âûïîëíåíèÿ."))
    )

    ((eq task-id 'CLADDING)
     (setq r
       (vl-catch-all-apply 'cladding-main
         (list layers report-mode export-excel export-txt
               create-table save-base)))

     (if (vl-catch-all-error-p r)
       (princ "\nÌîäóëü Îáëèöîâêà íå çàãðóæåí èëè îøèáêà âûïîëíåíèÿ."))
    )

    ((eq task-id 'VITRAZH)
     (setq r
       (vl-catch-all-apply 'vitrazh-main
         (list layers report-mode export-excel export-txt
               create-table save-base)))

     (if (vl-catch-all-error-p r)
       (princ "\nÌîäóëü Âèòðàæ íå çàãðóæåí èëè îøèáêà âûïîëíåíèÿ."))
    )

    ((eq task-id 'ZAPOLNENIE)
     (setq r
       (vl-catch-all-apply 'zapolnenie-main
         (list layers report-mode export-excel export-txt
               create-table save-base)))

     (if (vl-catch-all-error-p r)
       (princ "\nÌîäóëü Çàïîëíåíèå íå çàãðóæåí èëè îøèáêà âûïîëíåíèÿ."))
    )

    (T
     (princ "\nÍåèçâåñòíàÿ çàäà÷à."))
  )
)


;; ============================================================
;; ÏÎÌÎÙÜ
;; ============================================================

(defun extraction-help ()
  (alert
    (strcat
      "Îêíî çàïóñêà çàäà÷.\n\n"
      "Âûáîð îäíîé çàäà÷è, ñëî¸â, ðåæèìà îò÷¸òà "
      "è ôîðìàòîâ âûâîäà.\n"
      "Êíîïêè Ðàñêðîé õëûñòà / Ðàñêðîé ëèñòà "
      "çàïóñêàþò ìîäóëè ðàñêðîÿ.\n\n"
      "Ñîõðàíèòü — èìÿ ôàéëà ïî óìîë÷àíèþ.\n"
      "Ñîõðàíèòü êàê... — âûáîð ïóòè è èìåíè.\n"
      "Äëÿ Ïîäñèñòåìû äîñòóïíû âñå ñëîè; "
      "òðè ÷åêáîêñà — áûñòðûé âûáîð òèïîâûõ ñëî¸â.\n"
      "Êíîïêà \"Âûáðàòü âñå\" âûäåëÿåò âñå ñëîè, "
      "îòîáðàæàåìûå ñ ó÷¸òîì ôèëüòðîâ.\n\n"
      "Áëîêè: ôèëüòð \"Àíîíèìíûå áëîêè\" ïîêàçûâàåò "
      "òîëüêî PASTEBLOCK-áëîêè (A$C...). "
      "Ïåðåèìåíîâàíèå — ïî êíîïêå."
    )
  )
)


;; ============================================================
;; ÊÍÎÏÊÈ
;; ============================================================

(defun extraction-save ()
  (if (extraction-read-params)
    (progn
      (setq *EXTRACTION-ACTION* 'SAVE)
      (done_dialog 1)
    )
  )
)


(defun extraction-saveas ()
  (if (extraction-read-params)
    (progn
      (setq *EXTRACTION-ACTION* 'SAVEAS)
      (done_dialog 1)
    )
  )
)


(defun extraction-close ()
  (setq *EXTRACTION-ACTION* 'CANCEL)
  (done_dialog 0)
)


(defun extraction-cutline ()
  (setq *CUTLINE-CREATE-TABLE*
    (= (get_tile "chk_acad") "1"))

  (setq *CUTLINE-CREATE-XLS*
    (= (get_tile "chk_xls") "1"))

  (setq *EXTRACTION-ACTION* 'CUTLINE)
  (done_dialog 1)
)


(defun extraction-cutsheet ()
  (setq *CUTSHEET-CREATE-TABLE*
    (= (get_tile "chk_acad") "1"))

  (setq *CUTSHEET-CREATE-XLS*
    (= (get_tile "chk_xls") "1"))

  (setq *EXTRACTION-ACTION* 'CUTSHEET)
  (done_dialog 1)
)


;; ============================================================
;; ÔÈËÜÒÐÛ
;; ============================================================

(defun extraction-filter-facades ()
  (setq *EXTRACTION-FILTER-FACADES*
    (= (get_tile "chk_filter_facades") "1"))

  (setq *EXTRACTION-LAST-FILTER-FACADES*
    *EXTRACTION-FILTER-FACADES*)

  (extraction-rebuild-layer-list)

  (if (eq *EXTRACTION-TASK-ID* 'SUBSYSTEM)
    (if *EXTRACTION-LAST-SUBSYSTEM-LAYERS*
      (extraction-select-layers-in-list
        *EXTRACTION-LAST-SUBSYSTEM-LAYERS*))
    (if *EXTRACTION-LAST-FASONKA-LAYERS*
      (extraction-select-layers-in-list
        *EXTRACTION-LAST-FASONKA-LAYERS*))
  )
)


(defun extraction-filter-vitrazh ()
  (setq *EXTRACTION-FILTER-VITRAZH*
    (= (get_tile "chk_filter_vitrazh") "1"))

  (setq *EXTRACTION-LAST-FILTER-VITRAZH*
    *EXTRACTION-FILTER-VITRAZH*)

  (extraction-rebuild-layer-list)

  (if (eq *EXTRACTION-TASK-ID* 'SUBSYSTEM)
    (if *EXTRACTION-LAST-SUBSYSTEM-LAYERS*
      (extraction-select-layers-in-list
        *EXTRACTION-LAST-SUBSYSTEM-LAYERS*))
    (if *EXTRACTION-LAST-FASONKA-LAYERS*
      (extraction-select-layers-in-list
        *EXTRACTION-LAST-FASONKA-LAYERS*))
  )
)


(defun extraction-filter-fonar ()
  (setq *EXTRACTION-FILTER-FONAR*
    (= (get_tile "chk_filter_fonar") "1"))

  (setq *EXTRACTION-LAST-FILTER-FONAR*
    *EXTRACTION-FILTER-FONAR*)

  (extraction-rebuild-layer-list)

  (if (eq *EXTRACTION-TASK-ID* 'SUBSYSTEM)
    (if *EXTRACTION-LAST-SUBSYSTEM-LAYERS*
      (extraction-select-layers-in-list
        *EXTRACTION-LAST-SUBSYSTEM-LAYERS*))
    (if *EXTRACTION-LAST-FASONKA-LAYERS*
      (extraction-select-layers-in-list
        *EXTRACTION-LAST-FASONKA-LAYERS*))
  )
)


;; ============================================================
;; ÎÑÍÎÂÍÀß ÊÎÌÀÍÄÀ
;; ============================================================

(defun c:extraction
       ( / dcl-file save-base modules-dir r)

  (vl-load-com)

  ;; ----------------------------------------------------------
  ;; Çàãðóçêà âñåõ ìîäóëåé
  ;; ----------------------------------------------------------

  (extraction-load-all)

  ;; ----------------------------------------------------------
  ;; Ñîõðàíÿåì ïðåäâàðèòåëüíûé âûáîð
  ;; ----------------------------------------------------------

  (setq *extraction-preselected-set* (ssget "_I"))

  ;; ----------------------------------------------------------
  ;; Ïîèñê DCL
  ;; ----------------------------------------------------------

  (setq dcl-file nil)

  (setq modules-dir (extraction-modules-dir))

  (if modules-dir
    (setq dcl-file
      (findfile (strcat modules-dir "\\extraction.dcl"))))

  (if (null dcl-file)
    (setq dcl-file (findfile "extraction.dcl")))

  ;; ----------------------------------------------------------
  ;; Ïðîâåðêà DCL
  ;; ----------------------------------------------------------

  (if (null dcl-file)
    (progn
      (alert "Íå íàéäåí ôàéë extraction.dcl.")
      (princ)
    )
    (progn

      ;; ------------------------------------------------------
      ;; Ïîëó÷åíèå ñëî¸â
      ;; ------------------------------------------------------

      (setq *EXTRACTION-ALL-LAYERS* (extraction-layer-names))
      (setq *EXTRACTION-SELECTED-LAYERS* nil)

      ;; ------------------------------------------------------
      ;; Ïîñëåäíèå ôèëüòðû
      ;; ------------------------------------------------------

      (setq *EXTRACTION-FILTER-FACADES* *EXTRACTION-LAST-FILTER-FACADES*)
      (setq *EXTRACTION-FILTER-VITRAZH* *EXTRACTION-LAST-FILTER-VITRAZH*)
      (setq *EXTRACTION-FILTER-FONAR*   *EXTRACTION-LAST-FILTER-FONAR*)

      ;; ------------------------------------------------------
      ;; Ïîñëåäíèå ïàðàìåòðû
      ;; ------------------------------------------------------

      (setq *EXTRACTION-TASK-ID*      *EXTRACTION-LAST-TASK*)
      (setq *EXTRACTION-REPORT-MODE*  *EXTRACTION-LAST-REPORT-MODE*)
      (setq *EXTRACTION-EXPORT-EXCEL* *EXTRACTION-LAST-EXPORT-EXCEL*)
      (setq *EXTRACTION-EXPORT-TXT*   *EXTRACTION-LAST-EXPORT-TXT*)
      (setq *EXTRACTION-CREATE-TABLE* *EXTRACTION-LAST-CREATE-TABLE*)
      (setq *EXTRACTION-ACTION* 'CANCEL)

      ;; ------------------------------------------------------
      ;; Çàãðóçêà DCL
      ;; ------------------------------------------------------

      (setq *EXTRACTION-DCL-ID* (load_dialog dcl-file))

      (if (< *EXTRACTION-DCL-ID* 0)
        (alert "Íå óäàëîñü çàãðóçèòü extraction.dcl.")
        (progn

          ;; --------------------------------------------------
          ;; Ñîçäàíèå äèàëîãà
          ;; --------------------------------------------------

          (if (new_dialog "extraction_dialog" *EXTRACTION-DCL-ID*)
            (progn

              ;; ==============================================
              ;; Óñòàíîâêà ýëåìåíòîâ äèàëîãà
              ;; ==============================================

              (set_tile "rb_detail"
                (if (= *EXTRACTION-LAST-REPORT-MODE* "DETAIL") "1" "0"))
              (set_tile "rb_summary"
                (if (= *EXTRACTION-LAST-REPORT-MODE* "SUMMARY") "1" "0"))

              (set_tile "chk_xls"
                (if *EXTRACTION-LAST-EXPORT-EXCEL* "1" "0"))
              (set_tile "chk_txt"
                (if *EXTRACTION-LAST-EXPORT-TXT* "1" "0"))
              (set_tile "chk_acad"
                (if *EXTRACTION-LAST-CREATE-TABLE* "1" "0"))

              ;; ------------------------------------------------
              ;; Ôèëüòðû ñëî¸â
              ;; ------------------------------------------------

              (set_tile "chk_filter_facades"
                (if *EXTRACTION-LAST-FILTER-FACADES* "1" "0"))
              (set_tile "chk_filter_vitrazh"
                (if *EXTRACTION-LAST-FILTER-VITRAZH* "1" "0"))
              (set_tile "chk_filter_fonar"
                (if *EXTRACTION-LAST-FILTER-FONAR* "1" "0"))

              ;; ------------------------------------------------
              ;; Çàäà÷à
              ;; ------------------------------------------------

              (set_tile "rb_task_fasonka"
                (if (eq *EXTRACTION-LAST-TASK* 'FASONKA) "1" "0"))
              (set_tile "rb_task_subsystem"
                (if (eq *EXTRACTION-LAST-TASK* 'SUBSYSTEM) "1" "0"))
              (set_tile "rb_task_cladding"
                (if (eq *EXTRACTION-LAST-TASK* 'CLADDING) "1" "0"))
              (set_tile "rb_task_vitrazh"
                (if (eq *EXTRACTION-LAST-TASK* 'VITRAZH) "1" "0"))
              (set_tile "rb_task_zapolnenie"
                (if (eq *EXTRACTION-LAST-TASK* 'ZAPOLNENIE) "1" "0"))

              ;; ------------------------------------------------
              ;; ×åêáîêñû Ïîäñèñòåìû
              ;; ------------------------------------------------

              (set_tile "chk_subsystem_1"
                (if (car *EXTRACTION-LAST-SUBSYSTEM-CHECKS*) "1" "0"))
              (set_tile "chk_subsystem_2"
                (if (cadr *EXTRACTION-LAST-SUBSYSTEM-CHECKS*) "1" "0"))
              (set_tile "chk_subsystem_3"
                (if (caddr *EXTRACTION-LAST-SUBSYSTEM-CHECKS*) "1" "0"))

              ;; =================================================
              ;; ÈÍÈÖÈÀËÈÇÀÖÈß ÏÅÐÅÈÌÅÍÎÂÀÒÎÐÀ
              ;; =================================================

              (if (= (type blockrename-init) 'SUBR)
                (blockrename-init)
              )

              ;; =================================================
              ;; ÍÀÑÒÐÎÉÊÀ ÑÎÑÒÎßÍÈß ÏÎÄÑÈÑÒÅÌÛ
              ;; =================================================

              (if (eq *EXTRACTION-LAST-TASK* 'SUBSYSTEM)
                (progn
                  (mode_tile "box_subsystem_layers" 0)
                  (mode_tile "chk_subsystem_1" 0)
                  (mode_tile "chk_subsystem_2" 0)
                  (mode_tile "chk_subsystem_3" 0)

                  (extraction-rebuild-layer-list)

                  (if *EXTRACTION-LAST-SUBSYSTEM-LAYERS*
                    (extraction-select-layers-in-list
                      *EXTRACTION-LAST-SUBSYSTEM-LAYERS*))

                  (extraction-sync-checks-from-layers)
                )
                (progn
                  (mode_tile "box_subsystem_layers" 1)
                  (mode_tile "chk_subsystem_1" 1)
                  (mode_tile "chk_subsystem_2" 1)
                  (mode_tile "chk_subsystem_3" 1)

                  (extraction-rebuild-layer-list)

                  (if *EXTRACTION-LAST-FASONKA-LAYERS*
                    (extraction-select-layers-in-list
                      *EXTRACTION-LAST-FASONKA-LAYERS*))
                )
              )

              ;; =================================================
              ;; ÍÀÇÍÀ×ÅÍÈÅ ÄÅÉÑÒÂÈÉ
              ;; =================================================

              ;; -------------------------------------------------
              ;; Îñíîâíûå êíîïêè
              ;; -------------------------------------------------

              (action_tile "btn_help"   "(extraction-help)")
              (action_tile "btn_save"   "(extraction-save)")
              (action_tile "btn_saveas" "(extraction-saveas)")
              (action_tile "btn_close"  "(extraction-close)")

              ;; -------------------------------------------------
              ;; Ðàñêðîé
              ;; -------------------------------------------------

              (action_tile "btn_cutline"  "(extraction-cutline)")
              (action_tile "btn_cutsheet" "(extraction-cutsheet)")

              ;; -------------------------------------------------
              ;; Ñëîè
              ;; -------------------------------------------------

              (action_tile "btn_select_all"      "(extraction-select-all-layers)")
              (action_tile "btn_clear_all"       "(extraction-clear-all-layers)")
              (action_tile "chk_filter_facades"  "(extraction-filter-facades)")
              (action_tile "chk_filter_vitrazh"  "(extraction-filter-vitrazh)")
              (action_tile "chk_filter_fonar"    "(extraction-filter-fonar)")
              (action_tile "lst_layers"          "(extraction-layer-selection)")

              ;; -------------------------------------------------
              ;; Ðåæèì îò÷¸òà
              ;; -------------------------------------------------

              (action_tile "rb_detail"
                "(setq *EXTRACTION-REPORT-MODE* \"DETAIL\")(setq *EXTRACTION-LAST-REPORT-MODE* \"DETAIL\")")
              (action_tile "rb_summary"
                "(setq *EXTRACTION-REPORT-MODE* \"SUMMARY\")(setq *EXTRACTION-LAST-REPORT-MODE* \"SUMMARY\")")

              ;; -------------------------------------------------
              ;; Çàäà÷è
              ;; -------------------------------------------------

              (action_tile "rb_task_fasonka"
                "(setq *EXTRACTION-TASK-ID* 'FASONKA)(setq *EXTRACTION-LAST-TASK* 'FASONKA)(extraction-toggle-subsystem-layers)")
              (action_tile "rb_task_subsystem"
                "(setq *EXTRACTION-TASK-ID* 'SUBSYSTEM)(setq *EXTRACTION-LAST-TASK* 'SUBSYSTEM)(extraction-toggle-subsystem-layers)")
              (action_tile "rb_task_cladding"
                "(setq *EXTRACTION-TASK-ID* 'CLADDING)(setq *EXTRACTION-LAST-TASK* 'CLADDING)(extraction-toggle-subsystem-layers)")
              (action_tile "rb_task_vitrazh"
                "(setq *EXTRACTION-TASK-ID* 'VITRAZH)(setq *EXTRACTION-LAST-TASK* 'VITRAZH)(extraction-toggle-subsystem-layers)")
              (action_tile "rb_task_zapolnenie"
                "(setq *EXTRACTION-TASK-ID* 'ZAPOLNENIE)(setq *EXTRACTION-LAST-TASK* 'ZAPOLNENIE)(extraction-toggle-subsystem-layers)")

              ;; -------------------------------------------------
              ;; ×åêáîêñû Ïîäñèñòåìû
              ;; -------------------------------------------------

              (action_tile "chk_subsystem_1" "(extraction-subsystem-check-changed 1)")
              (action_tile "chk_subsystem_2" "(extraction-subsystem-check-changed 2)")
              (action_tile "chk_subsystem_3" "(extraction-subsystem-check-changed 3)")

              ;; =================================================
              ;; ÍÎÂÛÉ ÁËÎÊ — ÏÅÐÅÈÌÅÍÎÂÀÒÎÐ
              ;; =================================================

              (action_tile "chk_filter_anonymous" "(blockrename-filter-anonymous)")
              (action_tile "edt_block_search"     "(blockrename-search-changed)")
              (action_tile "lst_blocks"           "(blockrename-selected)")
              (action_tile "btn_block_rename"     "(blockrename-rename)")

              ;; =================================================
              ;; ÇÀÏÓÑÊ ÄÈÀËÎÃÀ
              ;; =================================================

              (start_dialog)

              ;; =================================================
              ;; ÎÁÐÀÁÎÒÊÀ ÄÅÉÑÒÂÈÉ ÏÎÑËÅ ÇÀÊÐÛÒÈß
              ;; =================================================

              (cond

                ;; ------------------------------------------------
                ;; SAVE
                ;; ------------------------------------------------

                ((eq *EXTRACTION-ACTION* 'SAVE)
                 (run-task
                   *EXTRACTION-TASK-ID*
                   *EXTRACTION-SELECTED-LAYERS*
                   *EXTRACTION-REPORT-MODE*
                   *EXTRACTION-EXPORT-EXCEL*
                   *EXTRACTION-EXPORT-TXT*
                   *EXTRACTION-CREATE-TABLE*
                   nil)
                )

                ;; ------------------------------------------------
                ;; SAVE AS
                ;; ------------------------------------------------

                ((eq *EXTRACTION-ACTION* 'SAVEAS)
                 (setq save-base
                   (vl-catch-all-apply 'tu-get-save-base
                     (list *EXTRACTION-TASK-ID*)))

                 (if (vl-catch-all-error-p save-base)
                   (princ "\nÎøèáêà âûáîðà ôàéëà.")
                   (if save-base
                     (run-task
                       *EXTRACTION-TASK-ID*
                       *EXTRACTION-SELECTED-LAYERS*
                       *EXTRACTION-REPORT-MODE*
                       *EXTRACTION-EXPORT-EXCEL*
                       *EXTRACTION-EXPORT-TXT*
                       *EXTRACTION-CREATE-TABLE*
                       save-base)
                   )
                 )
                )

                ;; ------------------------------------------------
                ;; CUTLINE
                ;; ------------------------------------------------

                ((eq *EXTRACTION-ACTION* 'CUTLINE)
                 (setq r
                   (vl-catch-all-apply 'cutline-main '()))

                 (if (vl-catch-all-error-p r)
                   (princ "\nÌîäóëü CUTLINE íå çàãðóæåí èëè îøèáêà âûïîëíåíèÿ."))
                )

                ;; ------------------------------------------------
                ;; CUTSHEET
                ;; ------------------------------------------------

                ((eq *EXTRACTION-ACTION* 'CUTSHEET)
                 (setq r
                   (vl-catch-all-apply 'cutsheet-main '()))

                 (if (vl-catch-all-error-p r)
                   (princ "\nÌîäóëü CUTSHEET íå çàãðóæåí èëè îøèáêà âûïîëíåíèÿ."))
                )
              )

              ;; --------------------------------------------------
              ;; Âûãðóçêà DCL
              ;; --------------------------------------------------

              (unload_dialog *EXTRACTION-DCL-ID*)
            )
            (alert "Íå óäàëîñü îòêðûòü äèàëîã EXTRACTION.")
          )
        )
      )
    )
  )

  (princ)
)


;; ============================================================
;; ÐÓÑÑÊÀß ÊÎÌÀÍÄÀ
;; ============================================================

(defun c:ÝÊÑÒÐÀÊÖÈß ()
  (c:extraction)
)


(princ "\nEXTRACTION.LSP çàãðóæåí.")
(princ)