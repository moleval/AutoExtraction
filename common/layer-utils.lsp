;;; ============================================================
;;; common/layer-utils.lsp
;;; Работа со слоями и групповыми фильтрами AutoCAD
;;; ============================================================

(vl-load-com)

(defun tu-layer-names ( / acad doc layers out item name)
  (setq out '())
  (setq acad (vlax-get-acad-object))
  (setq doc  (tu-safe-call 'vla-get-ActiveDocument (list acad)))
  (if doc
    (progn
      (setq layers (tu-safe-call 'vla-get-Layers (list doc)))
      (if layers
        (vlax-for item layers
          (setq name (tu-safe-call 'vla-get-Name (list item)))
          (if name
            (setq out (cons name out))
          )
        )
      )
    )
  )
  (tu-sort-strings-ci (tu-list-unique-ci out))
)

;; ------------------------------------------------------------
;; Получение словаря групповых фильтров.
;;
;; Структура AutoCAD: ExtensionDictionary таблицы LAYER ->
;; ACAD_LAYERFILTERS -> AcLyDictionary -> записи фильтров.
;; AcLyLayerGroup определяется по DXF-коду 1.
;; ------------------------------------------------------------

(defun tu-layer-filter-dictionary ( / acad doc layers extdict entry dict)
  (setq acad (vlax-get-acad-object))
  (setq doc  (tu-safe-call 'vla-get-ActiveDocument (list acad)))

  (if doc
    (progn
      (setq layers (tu-safe-call 'vla-get-Layers (list doc)))
      (if layers
        (progn
          (setq extdict
            (tu-safe-call 'vla-get-ExtensionDictionary (list layers))
          )

          (if extdict
            (progn
              ;; В современных версиях AutoCAD групповые фильтры
              ;; находятся в ACLYDICTIONARY внутри extension dictionary.
              ;; Для совместимости дополнительно пробуем ACAD_LAYERFILTERS.
              (setq entry
                (dictsearch
                  (vlax-vla-object->ename extdict)
                  "ACLYDICTIONARY"
                )
              )

              (if entry
                (setq dict (cdr (assoc -1 entry)))
                (progn
                  (setq entry
                    (dictsearch
                      (vlax-vla-object->ename extdict)
                      "ACAD_LAYERFILTERS"
                    )
                  )

                  (if entry
                    (setq dict
                      (cdr
                        (assoc -1
                          (dictsearch
                            (cdr (assoc 360 entry))
                            "ACLYDICTIONARY"
                          )
                        )
                      )
                    )
                  )
                )
              )

              dict
            )
          )
        )
      )
    )
  )
)

;; ------------------------------------------------------------
;; Рекурсивно извлекает имена слоёв из содержимого Group Filter.
;; В Group Filter связи с объектами хранятся через DXF 330.
;; Для объекта LAYER имя находится в DXF 2.
;; Для вложенного AcLyLayerGroup повторяем обход.
;; ------------------------------------------------------------

(defun tu-group-filter-layer-names-from-data
       (data dict visited / out ref objdef objtype nested)
  (setq out '())

  (foreach pair data
    (if (= (car pair) 330)
      (progn
        (setq ref (cdr pair))
        (if (and ref (not (member ref visited)))
          (progn
            (setq objdef (entget ref))
            (if objdef
              (progn
                (setq objtype (cdr (assoc 0 objdef)))
                (cond
                  ((= objtype "LAYER")
                   (if (cdr (assoc 2 objdef))
                     (setq out
                       (cons (cdr (assoc 2 objdef)) out)
                     )
                   )
                  )

                  ((= (cdr (assoc 1 objdef)) "AcLyLayerGroup")
                   (setq nested
                     (tu-group-filter-layer-names-from-data
                       objdef
                       dict
                       (cons ref visited)
                     )
                   )
                   (setq out (append nested out))
                  )
                )
              )
            )
          )
        )
      )
    )
  )

  (tu-sort-strings-ci (tu-list-unique-ci out))
)

;; ------------------------------------------------------------
;; Возвращает:
;; ( ("Имя фильтра" ("Слой1" "Слой2" ...)) ... )
;; только для AcLyLayerGroup.
;; ------------------------------------------------------------

(defun tu-group-filter-names-and-layers ( / dict itm ftype fname layers out)
  (setq out '())
  (setq dict (tu-layer-filter-dictionary))

  (if dict
    (progn
      (setq itm (dictnext dict T))
      (while itm
        (setq ftype (cdr (assoc 1 itm)))
        (setq fname (cdr (assoc 300 itm)))

        (if (and
              (= ftype "AcLyLayerGroup")
              fname
            )
          (progn
            (setq layers
              (tu-group-filter-layer-names-from-data
                itm
                dict
                '()
              )
            )
            (setq out
              (cons
                (list fname layers)
                out
              )
            )
          )
        )

        (setq itm (dictnext dict))
      )
    )
  )

  (reverse out)
)

;; ------------------------------------------------------------
;; Возвращает слои из групповых фильтров, имя которых содержит
;; хотя бы одно из keywords (без учёта регистра).
;; ------------------------------------------------------------

(defun tu-filtered-layer-names ( / filters layers keywords f)
  (setq keywords '("фасад" "витраж" "фонар"))
  (setq filters (tu-group-filter-names-and-layers))
  (setq layers '())

  (foreach f filters
    (if
      (vl-some
        '(lambda (key)
           (and
             (nth 0 f)
             (vl-string-search
               (strcase key)
               (strcase (nth 0 f))
             )
           )
         )
        keywords
      )
      (setq layers (append layers (nth 1 f)))
    )
  )

  (tu-sort-strings-ci (tu-list-unique-ci layers))
)

(princ)
