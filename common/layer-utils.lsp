;;; ============================================================
;;; common/layer-utils.lsp
;;; Работа со слоями и групповыми фильтрами
;;; ============================================================
(vl-load-com)

(defun tu-layer-filter-dictionary ( / layer0 layertable ext dict )
  (if (setq layer0 (tblobjname "layer" "0"))
    (progn
      (setq layertable (cdr (assoc 330 (entget layer0))))
      (if layertable
        (progn
          (setq ext (cdr (assoc 360 (entget layertable))))
          (if ext
            (progn
              (setq dict (dictsearch ext "ACLYDICTIONARY"))
              (if dict
                (cdr (assoc -1 dict))
                nil
              )
            )
            nil
          )
        )
        nil
      )
    )
    nil
  )
)

(defun tu-group-filter-layer-names-from-data (data visited / out pair code ref objdef objtype nested)
  (setq out '())
  (foreach pair data
    (setq code (car pair))
    (cond
      ((= code 330)
       (setq ref (cdr pair))
       (if (and ref (not (member ref visited)))
         (progn
           (setq objdef (entget ref))
           (if (and objdef (= (cdr (assoc 0 objdef)) "LAYER"))
             (if (cdr (assoc 2 objdef))
               (setq out (cons (cdr (assoc 2 objdef)) out))
             )
           )
         )
       )
      )
      ((= code 350)
       (setq ref (cdr pair))
       (if (and ref (not (member ref visited)))
         (progn
           (setq objdef (entget ref))
           (if objdef
             (progn
               (setq nested (tu-group-filter-layer-names-from-data objdef (cons ref visited)))
               (setq out (append nested out))
             )
           )
         )
       )
      )
    )
  )
  (tu-sort-strings-ci (tu-list-unique-ci out))
)

(defun tu-group-filter-names-and-layers ( / dict itm ftype fname layers out )
  (setq out '())
  (setq dict (tu-layer-filter-dictionary))
  (if dict
    (progn
      (setq itm (dictnext dict T))
      (while itm
        (setq ftype (cdr (assoc 1 itm)))
        (setq fname (cdr (assoc 300 itm)))
        (if (or (= ftype "AcLyLayerGroup") (= ftype "ACAD_LAYER_GROUP"))
          (if (= (type fname) 'STR)
            (progn
              (setq layers (tu-group-filter-layer-names-from-data itm '()))
              (setq out (cons (list fname layers) out))
            )
          )
        )
        (setq itm (dictnext dict))
      )
    )
  )
  (reverse out)
)

(princ "\nLAYER-UTILS.LSP загружен.")
(princ)