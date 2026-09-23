;;; ============================================================
;;; common/perf-utils.lsp
;;; Этап 5 (П1): поэтапная профилировка метками.
;;; Этап 5.1 требует искать O(n^2) ПРОФИЛИРОВКОЙ, не на глаз -
;;; поэтому сначала инструментация, потом оптимизация по фактам.
;;;
;;; Включение: (setq *ae-perf-on* T)   (по умолчанию отключено).
;;; При выключенном флаге метка = одна проверка условия (бесплатно).
;;; Вывод: строка вида  [PERF] МОДУЛЬ:фаза: N ms  по закрытию метки.
;;; Часы: MILLISECS (системный таймер AutoCAD, миллисекунды).
;;; Переодноимённые (вложенные одинаковые) метки не поддерживаются.
;;; ============================================================

(if (null (boundp '*ae-perf-on*)) (setq *ae-perf-on* nil))
(if (null (boundp '*pu-marks*))   (setq *pu-marks* '()))

(defun pu-begin (key)
  ;; открыть метку; возвращает key (можно использовать как значение)
  (if *ae-perf-on*
    (setq *pu-marks* (cons (cons key (getvar "MILLISECS")) *pu-marks*)))
  key)

(defun pu-end (key / m dt)
  ;; закрыть метку, напечатать продолжительность; возвращает key
  (if *ae-perf-on*
    (progn
      (setq m (assoc key *pu-marks*))
      (if m
        (progn
          (setq dt (- (getvar "MILLISECS") (cdr m)))
          (setq *pu-marks* (vl-remove m *pu-marks*))
          (princ (strcat "\n[PERF] " key ": " (itoa dt) " ms"))))))
  key)

(princ "\nPERF-UTILS.LSP загружен.")
(princ)
