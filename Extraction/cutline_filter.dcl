// ============================================================
// cutline_filter.dcl — Параметры раскроя хлыстов CUTLINE
//
// ОБНОВЛЕНО (Этап 3.1):
//   Добавлен выпадающий список "Тип блока:" для выбора
//   конкретного типа динамического блока
// ============================================================

cutline_filter_dialog : dialog {
  label = "Раскрой хлыстов";

  // ----------------------------------------------------------
  // ТИПЫ ДЛЯ РАСКРОЯ (с количеством справа, прижатым к краю)
  // ----------------------------------------------------------
  : boxed_column {
    label = "Какие детали раскроить";

    : row {
      : radio_button {
        key = "rb_line";
        label = "Только линии";
      }
      : spacer { }
      : text {
        key = "txt_line_count";
        label = "";
        alignment = right;
      }
    }

    : row {
      : radio_button {
        key = "rb_mline";
        label = "Только мультилинии";
      }
      : spacer { }
      : text {
        key = "txt_mline_count";
        label = "";
        alignment = right;
      }
    }

    : row {
      : radio_button {
        key = "rb_dynblock";
        label = "Динамические блоки";
      }
      : spacer { }
      : text {
        key = "txt_dynblock_count";
        label = "";
        alignment = right;
      }
    }

    : row {
      : radio_button {
        key = "rb_both";
        label = "Все типы";
      }
      : spacer { }
      : text {
        key = "txt_both_count";
        label = "";
        alignment = right;
      }
    }

    // ----------------------------------------------------------
    // Выпадающий список типов динамических блоков
    // ДОБАВЛЕНО (Этап 3.1)
    // ----------------------------------------------------------
    : row {
      : text {
        key = "txt_dynblock_filter_label";
        label = "Тип блока:";
        width = 18;
      }
      : popup_list {
        key = "popup_dynblock_type";
        width = 30;
      }
    }
  }

  // ----------------------------------------------------------
  // СЛОИ
  // ----------------------------------------------------------
  : boxed_column {
    label = "Выбранные слои";
    : list_box {
      key = "lst_layers";
      width = 50;
      height = 5;
    }
  }

  // ----------------------------------------------------------
  // ПАРАМЕТРЫ РАСКРОЯ
  // ----------------------------------------------------------
  : boxed_column {
    label = "Параметры раскроя";

    : edit_box {
      key = "edt_stock";
      label = "Длина хлыста, мм:";
      edit_width = 12;
    }
    : edit_box {
      key = "edt_kerf";
      label = "Ширина реза, мм:";
      edit_width = 12;
    }
  }

  // ----------------------------------------------------------
  // ЭКСПОРТ
  // ----------------------------------------------------------
  : boxed_row {
    label = "Экспорт";
    : toggle { key = "chk_xls";  label = ".xls"; }
    : toggle { key = "chk_acad"; label = "Таблица AutoCAD"; }
  }

  // ----------------------------------------------------------
  // КНОПКИ
  // ----------------------------------------------------------
  : row {
    alignment = centered;
    : button {
      key = "btn_ok";
      label = "Раскроить";
      is_default = true;
    }
    : button {
      key = "btn_cancel";
      label = "Отмена";
      is_cancel = true;
    }
  }
}