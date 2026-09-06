;;; ============================================================
;;; common/layer-utils.lsp
;;; Работа со слоями и групповыми фильтрами слоёв AutoCAD 2016+
;;; ============================================================

(vl-load-com)

;; ------------------------------------------------------------
;; Если функции сортировки и уникализации ещё не загружены,
;; пытаемся загрузить task-utils.lsp из той же папки
;; ------------------------------------------------------------
(if (or (not tu-sort-strings-ci)
        (not tu-list-unique-ci))
    (progn
      (setq tu-common-path
             (strcat (vl-filename-directory (findfile "layer-utils.lsp"))
                     "/task-utils.lsp"))
      (if (findfile tu-common-path)
          (load tu-common-path)
      )
    )
)

;; ============================================================
;; Получение списка всех слоёв чертежа
;; ============================================================
(defun tu-layer-names ( / acad doc layers out item name )
  (setq out '())
  (setq acad (vl-catch-all-apply 'vlax-get-acad-object '()))
  (if (and (not (vl-catch-all-error-p acad))
           acad)
    (progn
      (setq doc (vl-catch-all-apply 'vla-get-ActiveDocument (list acad)))
      (if (and (not (vl-catch-all-error-p doc))
               doc)
        (progn
          (setq layers (vl-catch-all-apply 'vla-get-Layers (list doc)))
          (if (and (not (vl-catch-all-error-p layers))
                   layers)
            (vlax-for item layers
              (setq name (vl-catch-all-apply 'vla-get-Name (list item)))
              (if (and (not (vl-catch-all-error-p name))
                       (= (type name) 'STR))
                (setq out (cons name out))
              )
            )
          )
        )
      )
    )
  )
  (tu-sort-strings-ci (tu-list-unique-ci out))
)

;; ============================================================
;; Получение ACLYDICTIONARY
;;
;; Структура:
;;   Layer Table (таблица слоёв)
;;     ??? Расширенный словарь (код 360)
;;           ??? "ACLYDICTIONARY"
;;                 ??? AcLyLayerGroup
;;                 ??? AcLyLayerFilter
;;
;; В AutoCAD 2016 таблица слоёв не доступна через Named Object
;; Dictionary, поэтому получаем её через владельца слоя "0".
;; ============================================================
(defun tu-layer-filter-dictionary ( / layer0 layertable ext dict )
  ;; Получаем любой слой (например, "0")
  (if (setq layer0 (tblobjname "layer" "0"))
    (progn
      ;; Владелец слоя — таблица слоёв (код 330)
      (setq layertable (cdr (assoc 330 (entget layer0))))
      (if layertable
        (progn
          ;; Расширенный словарь таблицы слоёв (код 360)
          (setq ext (cdr (assoc 360 (entget layertable))))
          (if ext
            (progn
              ;; Ищем в нём ACLYDICTIONARY
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

;; ============================================================
;; Извлечение слоёв из данных AcLyLayerGroup
;;
;; DXF-коды:
;;   330 — прямая ссылка на объект LAYER
;;   350 — вложенный AcLyLayerGroup
;; ============================================================
(defun tu-group-filter-layer-names-from-data ( data visited / out pair code ref objdef objtype nested )
  (setq out '())
  (foreach pair data
    (setq code (car pair))
    (cond
      ;; Прямая ссылка на слой
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
      ;; Вложенный групповой фильтр
      ((= code 350)
       (setq ref (cdr pair))
       (if (and ref (not (member ref visited)))
         (progn
           (setq objdef (entget ref))
           (if objdef
             (progn
               (setq nested (tu-group-filter-layer-names-from-data
                               objdef
                               (cons ref visited)))
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

;; ============================================================
;; Получение списка групповых фильтров и их слоёв:
;;   ( ("Имя фильтра" ("Слой1" "Слой2" ...)) ... )
;; Только AcLyLayerGroup
;; ============================================================
(defun tu-group-filter-names-and-layers ( / dict itm ftype fname layers out )
  (setq out '())
  (setq dict (tu-layer-filter-dictionary))
  (if dict
    (progn
      (setq itm (dictnext dict T))
      (while itm
        (setq ftype (cdr (assoc 1 itm)))
        (setq fname (cdr (assoc 300 itm)))
        ;; В старом словаре тип может быть в коде 1 как "ACAD_LAYER_GROUP"
        (if (or (= ftype "AcLyLayerGroup")
                (= ftype "ACAD_LAYER_GROUP"))
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

;; ============================================================
;; Возвращает слои из групповых фильтров, имена которых
;; содержат хотя бы одно ключевое слово:
;;   фасад, витраж, фонар (регистр не учитывается)
;; ============================================================
(defun tu-filtered-layer-names ( / filters layers keywords f fname )
  (setq keywords '("фасад" "витраж" "фонар"))
  (setq filters (tu-group-filter-names-and-layers))
  (setq layers '())
  (foreach f filters
    (setq fname (car f))
    (if (and (= (type fname) 'STR)
             (vl-some '(lambda (key)
                         (and (= (type key) 'STR)
                              (vl-string-search (strcase key) (strcase fname))))
                      keywords))
      (setq layers (append layers (cadr f)))
    )
  )
  (tu-sort-strings-ci (tu-list-unique-ci layers))
)

(princ "\nlayer-utils.lsp загружен.")
(princ)