;;; ============================================================
;;; RELOAD.LSP
;;; Автоматическая перезагрузка AutoExtraction
;;; AutoCAD 2016+
;;;
;;; Возможности:
;;;   - определяет корень проекта по extraction.lsp;
;;;   - загружает common-модули в правильном порядке;
;;;   - загружает Extraction-модули в правильном порядке;
;;;   - проверяет наличие каждого файла;
;;;   - перехватывает ошибки загрузки;
;;;   - точно сообщает имя файла, вызвавшего ошибку;
;;;   - продолжает загрузку следующих модулей;
;;;
;;; ДОБАВЛЕНО:
;;;   - автоподгрузка tests\chkparens.lsp (если файл существует);
;;;   - пост-диагностика скобок ТОЛЬКО при ошибке загрузки:
;;;     печатает номер строки с небалансом упавшего файла.
;;;     Guard: (= (type chk-parens-scan) 'SUBR) - если проверка
;;;     не загружена, RELOAD работает как раньше.
;;; ============================================================

(vl-load-com)

;; ------------------------------------------------------------
;; Списки модулей - на верхнем уровне файла.
;; c:RELOAD при каждом запуске перечитывает СЕБЯ с диска (см. ниже),
;; поэтому эти списки всегда актуальны - даже если команда RELOAD
;; в памяти AutoCAD определена старой редакцией.
;; ------------------------------------------------------------
(setq *ae-reload-common-files*
  '(
    "task-utils.lsp"
    "settings-utils.lsp"
    "perf-utils.lsp"
    "layer-utils.lsp"
    "select-utils.lsp"
    "excel-utils.lsp"
    "table-utils.lsp"
    "txt-utils.lsp"
    "validation-utils.lsp"
  )
)

(setq *ae-reload-extraction-files*
  '(
    "fasonka.lsp"
    "subsystem.lsp"
    "cladding.lsp"
    "zapolnenie.lsp"
    "settings.lsp"
    "help.lsp"
    "extraction.lsp"
    "cutline.lsp"
    "cutsheet.lsp"
    "blockrename.lsp"
  )
)

;; ------------------------------------------------------------
;; ПЛАГИНЫ: сторонние модули. Это отдельные продукты со своими
;; репозиториями: в списки COMMON/EXTRACTION не входят и грузятся
;; последними. Для каждого имени файл ищется сначала в каталогах
;; разработки (см. *ae-reload-plugin-dev-dirs* ниже), затем в папке
;; Plugins\ проекта - туда кладутся принятые плагины, как есть
;; (cp1251, без перекодирования).
;; Если файл не найден нигде и папки Plugins\ нет - секция RELOAD
;; молча пропускается (отсутствие плагина - не ошибка проекта).
;; ------------------------------------------------------------
(setq *ae-reload-plugin-files*
  '(
    "PlotFrameToPDF.lsp"
  )
)

;; Каталоги РАЗРАБОТКИ плагинов. Если файл плагина найден в одном из
;; этих каталогов, RELOAD грузит его ОТТУДА, а не из Plugins\ - правки
;; в рабочем репозитории плагина подхватываются без копирования.
;; В таком случае копия в Plugins\ не нужна и в git не кладётся.
;; Пути меняются здесь, без правок кода; список может быть пустым.
;; Пути пишутся БЕЗ завершающего обратного слэша: имя файла добавляется
;; через "\\".
(setq *ae-reload-plugin-dev-dirs*
  '(
    "D:\\PlotFrameToPDF"
  )
)

;; ------------------------------------------------------------
;; Автоподгрузка проверки скобок (дешево: только defun'ы,
;; без сканирования). Если файла нет - молча пропускаем.
;; Загрузка обёрнута в перехват: сбой в tests\chkparens.lsp не должен
;; обрывать загрузку САМОГО reload.lsp (иначе молча теряются его
;; функции, в том числе ae-reload-load-file и c:RELOAD).
;; ------------------------------------------------------------
(setq *ae-chk-ok* nil)
(if (findfile "extraction.lsp")
  (progn
    (setq ae-chk-path
      (strcat
        (vl-filename-directory
          (vl-filename-directory (findfile "extraction.lsp")))
        "\\tests\\chkparens.lsp"))
    (if (findfile ae-chk-path)
      (progn
        (setq ae-chk-res (vl-catch-all-apply 'load (list ae-chk-path)))
        (if (vl-catch-all-error-p ae-chk-res)
          (princ (strcat "\n[RELOAD] ВНИМАНИЕ: не загружен " ae-chk-path ": "
                         (vl-catch-all-error-message ae-chk-res)
                         " - поиск формы со сбоем будет встроенным"))
          (setq *ae-chk-ok* T)
        )
      )
    )
  )
)


(defun ae-reload-rev-count (paths / f cnt found line ff)
  ;; U3: сколько модулей объявили маркер редакции "(ред. N" в своём файле.
  ;; Чтение фаилов сырое (байтовое) - ищем подстроку как есть.
  (setq cnt 0)
  (foreach f paths
    (setq found nil ff (open f "r"))
    (if ff
      (progn
        (while (and (not found) (setq line (read-line ff)))
          ;; признак: "(ред. N" (стандарт) или ", ред. N" (стиль CLADDING)
          (if (or (vl-string-search "(ред. " line)
                  (vl-string-search ", ред. " line))
            (setq found T)))
        (close ff)))
    (if found (setq cnt (1+ cnt))))
  cnt)

;; ------------------------------------------------------------
;; Путь к плагину в каталоге разработки, если файл там есть.
;; Первый найденный каталог выигрывает; nil - файла нет ни в одном.
;; ------------------------------------------------------------
(defun ae-reload-plugin-dev-path (name / d p hit)
  (setq hit nil)
  (foreach d *ae-reload-plugin-dev-dirs*
    (if (null hit)
      (progn
        (setq p (strcat d "\\" name))
        (if (findfile p)
          (setq hit p))
      )
    )
  )
  hit
)


;; ============================================================
;; ВСТРОЕННЫЙ ПОИСК ФОРМЫ СО СБОЕМ ЗАГРУЗКИ (резерв CHKLOAD)
;; Нужен, когда tests\chkparens.lsp недоступен, не загрузился или он
;; старой версии: файл режется на префиксы по границам верхнеуровневых
;; форм (строка начинается с "(" при нулевом балансе), каждый префикс
;; пишется во временный файл и загружается; первый падающий префикс
;; указывает форму со сбоем. Метки те же, что у команды CHKLOAD.
;; ============================================================

;; Тип символа, даже если символ не определён (nil вместо ошибки)
(defun ae-reload-sym-type (sym / r)
  (setq r (vl-catch-all-apply 'type (list (vl-catch-all-apply 'eval (list sym)))))
  (if (vl-catch-all-error-p r) nil r)
)

;; Баланс скобок после каждой строки (строки в кавычках и комментарии
;; не считаются)
(defun ae-reload-line-balances (lines / out bal line i ch instr incomm skip)
  (setq out '() bal 0)
  (foreach line lines
    (setq i 1 instr nil incomm nil skip nil)
    (while (<= i (strlen line))
      (setq ch (substr line i 1))
      (cond
        (skip (setq skip nil))
        (incomm nil)
        ((and instr (= ch "\\")) (setq skip T))
        (instr (if (= ch "\"") (setq instr nil)))
        ((= ch ";") (setq incomm T))
        ((= ch "\"") (setq instr T))
        ((= ch "(") (setq bal (1+ bal)))
        ((= ch ")") (setq bal (1- bal)))
      )
      (setq i (1+ i))
    )
    (setq out (cons bal out))
  )
  (reverse out)
)

;; Записать первые k-1 строк (перевод LF) во временный файл и загрузить.
;; nil = префикс загрузился; иначе - текст ошибки загрузки.
(defun ae-reload-probe-try (lines k tmp / i f res line)
  (setq i 0)
  (setq f (open tmp "w"))
  (if (null f)
    "не открыт временный файл"
    (progn
      (while (< i (1- k))
        (setq line (nth i lines))
        (if (and (> (strlen line) 0) (= (substr line (strlen line)) "\r"))
          (setq line (substr line 1 (1- (strlen line))))
        )
        (write-line line f)
        (setq i (1+ i))
      )
      (close f)
      (setq res (vl-catch-all-apply 'load (list tmp)))
      (vl-file-delete tmp)
      (if (vl-catch-all-error-p res)
        (vl-catch-all-error-message res)
        nil)
    )
  )
)

;; Напечатать, в какой форме сбой загрузки файла
(defun ae-reload-probe-file (path / f line lines tot bals bounds i lo hi mid tmp
                             found n prev cur msg bad)
  (setq f (open path "r"))
  (if (null f)
    (princ (strcat "\n[CHKLOAD] не открыт: " path))
    (progn
      (setq lines '())
      (while (setq line (read-line f)) (setq lines (cons line lines)))
      (close f)
      (setq lines (reverse lines) n (length lines) tot 0)
      (foreach line lines (setq tot (+ tot (strlen line))))
      (setq bals (ae-reload-line-balances lines))
      (setq bounds '() prev 0 i 0)
      (foreach line lines
        (if (and (= (substr line 1 1) "(") (= prev 0))
          (setq bounds (cons (1+ i) bounds)))
        (setq prev (nth i bals))
        (setq i (1+ i))
      )
      (setq bounds (reverse bounds))
      (if (null bounds)
        (princ (strcat "\n[CHKLOAD] " path ": границы форм не найдены"))
        (progn
          (setq tmp (vl-catch-all-apply 'vl-filename-mktemp
                                        (list "ae-probe" (vl-filename-directory path) ".lsp")))
          (if (or (vl-catch-all-error-p tmp) (null tmp))
            (setq tmp (strcat (vl-filename-directory path) "\\ae-probe-tmp.lsp")))
          (princ (strcat "\n[CHKLOAD] " path ", строк " (itoa n)
                         ", символов " (itoa tot)
                         ", форм верхнего уровня " (itoa (length bounds))))
          (setq msg (ae-reload-probe-try lines (1+ n) tmp))
          (if (null msg)
            (princ "\n[CHKLOAD] копия файла (LF) грузится, а сам файл — нет (дело не в содержимом форм)")
            (progn
              (princ (strcat "\n[CHKLOAD] загрузка всего файла: " msg))
              (setq lo 0 hi (1- (length bounds)) found nil)
              (while (<= lo hi)
                (setq mid (fix (/ (+ lo hi) 2.0)))
                (if (ae-reload-probe-try lines (nth mid bounds) tmp)
                  (progn (setq found mid hi (1- mid)))
                  (setq lo (1+ mid))
                )
              )
              ;; Префикс перед формой k падает, префикс перед формой k-1
              ;; грузится -> дефект в форме k-1. Падает только целый файл -
              ;; дефект в последней форме.
              (setq bad (if found (if (> found 0) (1- found) 0) (1- (length bounds))))
              (setq cur (nth bad bounds))
              (setq msg (ae-reload-probe-try lines
                          (if found (nth found bounds) (1+ n)) tmp))
              (princ (strcat "\n[CHKLOAD] СБОЙ В ФОРМЕ №" (itoa (1+ bad)) ", строки "
                             (itoa cur) ".."
                             (itoa (if (< (1+ bad) (length bounds))
                                     (1- (nth (1+ bad) bounds))
                                     n))
                             " | ошибка: " (if msg msg "нет")))
              (setq i 0)
              (foreach line lines
                (setq i (1+ i))
                (if (and (>= i cur) (<= i (+ cur 11)))
                  (princ (strcat "\n[CHKLOAD]   " (itoa i) ": " (substr line 1 160)))
                )
              )
            )
          )
        )
      )
    )
  )
  (princ)
)

;; Запуск поиска: chk-load-find из tests\chkparens.lsp, а если его нет -
;; встроенный поиск. Ошибки самого поиска наружу не выходят.
(defun ae-reload-chkload-run (path / res)
  (if (member (ae-reload-sym-type 'chk-load-find) '(SUBR USUBR))
    (progn
      (setq res (vl-catch-all-apply 'chk-load-find (list path)))
      (if (vl-catch-all-error-p res)
        (princ (strcat "\n[CHKLOAD] поиск прерван: " (vl-catch-all-error-message res)))
      )
    )
    (ae-reload-probe-file path)
  )
  (princ)
)

(defun ae-reload-load-file
       (fullpath note / result)

  ;; ------------------------------------------------------------
  ;; Загрузка одного LSP-файла с перехватом ошибки
  ;; ------------------------------------------------------------

  (if (not (findfile fullpath))
    (progn
      (princ
        (strcat
          "\nНЕ НАЙДЕН: "
          fullpath
        )
      )
      nil
    )
    (progn

      (setq result
        (vl-catch-all-apply
          'load
          (list fullpath)
        )
      )

      (if (vl-catch-all-error-p result)
        (progn
          (princ
            (strcat
              "\nОШИБКА ЗАГРУЗКИ: "
              fullpath
              "\n  Причина: "
              (vl-catch-all-error-message result)
              (if (vl-file-size fullpath)
                (strcat "\n  Размер файла: " (itoa (vl-file-size fullpath)) " байт")
                "")
            )
          )
          ;; ----------------------------------------------------
          ;; Пост-диагностика: показываем строку с небалансом
          ;; скобок упавшего файла (только при ошибке загрузки)
          ;; ----------------------------------------------------
          (if (= (type chk-parens-scan) 'SUBR)
            (progn
              (princ "\n--- диагностика файла: скобки, строки, скрытые байты ---")
              (chk-parens-scan fullpath)
            )
          )
          ;; Локализация сбоя: бисекция по верхнеуровневым формам (CHKLOAD).
          ;; Основной инструмент - chk-load-find из tests\chkparens.lsp;
          ;; если он недоступен (нет файла, файл не загрузился или он
          ;; старой версии), работает встроенный поиск в самом reload.lsp.
          (princ "\n--- поиск формы со сбоем (CHKLOAD) ---")
          (ae-reload-chkload-run fullpath)
          nil
        )
        (progn
          (princ
            (strcat
              "\nЗагружен: "
              fullpath
              (if note
                (strcat "  [" note "]")
                ""
              )
            )
          )
          T
        )
      )
    )
  )
)


(defun c:RELOAD
       ( / root
           common
           extraction-dir
           f
           fullpath
           common-files
           extraction-files
           plugins-dir
           plugin-items
           plugin-cnt
           dev-path
           it
           ok
           errors
           missing
           revcnt
           revpaths)

  ;; ------------------------------------------------------------
  ;; Заголовок
  ;; ------------------------------------------------------------

  (princ
    "\n============================================="
  )
  (princ
    "\n AutoExtraction RELOAD"
  )
  (princ
    "\n============================================="
  )


  ;; ------------------------------------------------------------
  ;; Определяем корень проекта
  ;; ------------------------------------------------------------

  (setq root
    (if (findfile "extraction.lsp")
      (vl-filename-directory
        (vl-filename-directory
          (findfile "extraction.lsp")
        )
      )
      nil
    )
  )


  ;; ------------------------------------------------------------
  ;; Если extraction.lsp не найден
  ;; ------------------------------------------------------------

  (if (not root)
    (progn
      (princ
        "\nОШИБКА: не найден extraction.lsp."
      )
      (princ
        "\nRELOAD прерван."
      )
      (princ)
    )

    (progn

      ;; --------------------------------------------------------
      ;; Пути
      ;; --------------------------------------------------------

      (setq common
        (strcat root "\\common\\")
      )

      (setq extraction-dir
        (strcat root "\\Extraction\\")
      )

      (setq plugins-dir
        (strcat root "\\Plugins\\")
      )


      (princ
        (strcat
          "\nКорень проекта: "
          root
        )
      )

      (princ
        (strcat
          "\nCommon: "
          common
        )
      )

      (princ
        (strcat
          "\nExtraction: "
          extraction-dir
        )
      )


      ;; --------------------------------------------------------
      ;; Счетчики
      ;; --------------------------------------------------------

      (setq ok 0)
      (setq errors 0)
      (setq missing 0)


      ;; --------------------------------------------------------
      ;; COMMON
      ;; --------------------------------------------------------

      ;; Самообновление: перечитать себя с диска - иначе добавленные
      ;; модули не загружаются до перезапуска AutoCAD
      ;; (регрессия 2026-09-22: no function definition TU-PARSE-INT-LIST).
      (setq *ae-reload-self* (vl-catch-all-apply 'load (list (strcat root "\\reload.lsp"))))
      ;; Сверку версий печатает верхний уровень перечитанного reload.lsp.
      ;; Здесь — только если перечитать не удалось: иначе о составе
      ;; комплекта не сообщит никто (в сессии остаются старые определения).
      (if (vl-catch-all-error-p *ae-reload-self*)
        (progn
          (princ (strcat "\n[RELOAD] ВНИМАНИЕ: reload.lsp не перечитан с диска: "
                         (vl-catch-all-error-message *ae-reload-self*)
                         " - в сессии работают определения из памяти AutoCAD"))
          (princ (strcat "\n" (ae-reload-version-line)))
        )
      )

      (setq common-files *ae-reload-common-files*)


      (princ
        "\n"
      )
      (princ
        "\n--- COMMON ---"
      )


      (foreach f common-files

        (setq fullpath
          (strcat common f)
        )

        (if (ae-reload-load-file fullpath)
          (setq ok (1+ ok))
          (if (findfile fullpath)
            (setq errors (1+ errors))
            (setq missing (1+ missing))
          )
        )
      )


      ;; --------------------------------------------------------
      ;; EXTRACTION
      ;; --------------------------------------------------------

      (setq extraction-files *ae-reload-extraction-files*)


      (princ
        "\n"
      )
      (princ
        "\n--- EXTRACTION ---"
      )


      (foreach f extraction-files

        (setq fullpath
          (strcat extraction-dir f)
        )

        (if (ae-reload-load-file fullpath)
          (setq ok (1+ ok))
          (if (findfile fullpath)
            (setq errors (1+ errors))
            (setq missing (1+ missing))
          )
        )
      )


      ;; --------------------------------------------------------
      ;; PLUGINS (сторонние модули)
      ;; --------------------------------------------------------
      ;; Плагин ищется сначала в каталогах разработки
      ;; (*ae-reload-plugin-dev-dirs*), затем в Plugins\ проекта.
      ;; Молча пропускаем то, чего нет нигде и для чего нет папки
      ;; Plugins\: ни строки "НЕ НАЙДЕН", ни влияния на счётчики.
      ;; Но если папка Plugins\ есть, а заявленного файла в ней нет -
      ;; это честно попадает в missing (иначе потеря была бы невидимой).
      (setq plugin-items '())
      (foreach f *ae-reload-plugin-files*

        (setq dev-path (ae-reload-plugin-dev-path f))
        (setq fullpath
          (if dev-path
            dev-path
            (strcat plugins-dir f)
          )
        )

        (if (or dev-path (vl-file-directory-p plugins-dir))
          (setq plugin-items
            (cons
              (cons fullpath (if dev-path "разработка" nil))
              plugin-items
            )
          )
        )
      )
      (setq plugin-items (reverse plugin-items))
      (setq plugin-cnt (length plugin-items))

      (if (> plugin-cnt 0)
        (progn

          (princ
            "\n"
          )
          (princ
            "\n--- PLUGINS ---"
          )

          (foreach it plugin-items

            (setq fullpath (car it))

            (if (ae-reload-load-file fullpath (cdr it))
              (setq ok (1+ ok))
              (if (findfile fullpath)
                (setq errors (1+ errors))
                (setq missing (1+ missing))
              )
            )
          )
        )
      )


      ;; --------------------------------------------------------
      ;; Итог
      ;; --------------------------------------------------------

      ;; U3: номер прогона RELOAD за сессию (выживает перечитывание reload.lsp,
      ;; т.к. сброса на ноль нет) и счётчик маркеров редакции.
      (if (null (boundp '*ae-reload-run*))
        (setq *ae-reload-run* 0))
      (setq *ae-reload-run* (1+ *ae-reload-run*))
      (setq revpaths '())
      (foreach f common-files
        (setq revpaths (cons (strcat common f) revpaths)))
      (foreach f extraction-files
        (setq revpaths (cons (strcat extraction-dir f) revpaths)))
      (foreach it plugin-items
        (setq revpaths (cons (car it) revpaths)))
      (setq revcnt (ae-reload-rev-count revpaths))

      (princ
        "\n"
      )
      (princ
        "\n============================================="
      )

      (princ
        (strcat
          "\n RELOAD завершен (прогон #"
          (itoa *ae-reload-run*)
          ")."
        )
      )
      (princ
        (strcat
          "\n Маркер редакции: "
          (itoa revcnt)
          " из "
          (itoa (+ (length common-files) (length extraction-files) plugin-cnt))
          " модулей."
        )
      )

      (princ
        (strcat
          "\n Успешно загружено: "
          (itoa ok)
        )
      )

      (princ
        (strcat
          "\n Ошибок загрузки: "
          (itoa errors)
        )
      )

      (princ
        (strcat
          "\n Не найдено файлов: "
          (itoa missing)
        )
      )

      (princ
        "\n============================================="
      )


      ;; --------------------------------------------------------
      ;; Дополнительное предупреждение
      ;; --------------------------------------------------------

      (if (> errors 0)
        (princ
          "\nВНИМАНИЕ: имеются ошибки загрузки. См. строки выше."
        )
      )

      (if (> missing 0)
        (princ
          "\nВНИМАНИЕ: некоторые файлы проекта не найдены."
        )
      )
    )
  )

  (princ)
)



;; ------------------------------------------------------------
;; Сверка версий комплекта. Печатается на ВЕРХНЕМ уровне, то есть при
;; каждом чтении reload.lsp с диска (ручная загрузка и самообновление
;; внутри RELOAD). Прежде строку печатало только тело c:RELOAD, а оно
;; выполняется в редакции, загруженной ДО самообновления: если в сессии
;; осталось старое c:RELOAD, строка молча пропадала (прогон #21).
;; ------------------------------------------------------------
(defun ae-reload-root ( / p)
  (setq p (findfile "extraction.lsp"))
  (if p (vl-filename-directory (vl-filename-directory p)))
)

(defun ae-reload-size-str (path / n)
  (setq n (if path (vl-file-size path)))
  (itoa (if n n 0))
)

(defun ae-reload-version-line ( / root)
  (setq root (ae-reload-root))
  (strcat "[RELOAD] сверка версий: reload.lsp "
          (ae-reload-size-str (if root (strcat root "\\reload.lsp")))
          " байт | tests\\chkparens.lsp "
          (ae-reload-size-str (if root (strcat root "\\tests\\chkparens.lsp")))
          " байт | загружен: " (if *ae-chk-ok* "да" "нет")
          " | chk-load-find: "
          (if (member (ae-reload-sym-type 'chk-load-find) '(SUBR USUBR))
            "есть" "нет"))
)

(princ
  "\nRELOAD.LSP загружен. Команда: RELOAD"
)

(princ (strcat "\n" (ae-reload-version-line)))

(princ)