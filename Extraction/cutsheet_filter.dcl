// ============================================================
// cutsheet_filter.dcl — параметры двумерного раскроя листа
// ============================================================

cutsheet_filter_dialog : dialog {
  label = "Раскрой листа";

  : boxed_column {
    label = "Какие детали раскроить";

    : radio_column {
      : radio_button {
        key = "rb_all";
        label = "Все типы";
        width = 25;
      }
      : radio_button {
        key = "rb_poly";
        label = "Только полилинии";
        width = 25;
      }
      : radio_button {
        key = "rb_dyn";
        label = "Динамические блоки";
        width = 25;
      }
    }

    : row {
      : text { key = "txt_all_count";  label = ""; alignment = left; width = 30; }
    }
    : row {
      : text { key = "txt_poly_count"; label = ""; alignment = left; width = 30; }
    }
    : row {
      : text { key = "txt_dyn_count";  label = ""; alignment = left; width = 30; }
    }
  }

  : boxed_column {
    label = "Тип динамического блока";
    : popup_list {
      key = "popup_dyn_type";
      width = 65;
    }
  }

  : boxed_column {
    label = "Параметры листа";

    : edit_box {
      key = "edt_sheet_w";
      label = "Ширина листа, мм:";
      edit_width = 12;
    }

    : edit_box {
      key = "edt_sheet_h";
      label = "Высота листа, мм:";
      edit_width = 12;
    }

    : edit_box {
      key = "edt_kerf";
      label = "Ширина реза, мм:";
      edit_width = 12;
    }

    : toggle {
      key = "chk_rotate";
      label = "Разрешить поворот деталей на 90°";
    }
  }

  : boxed_row {
    label = "Экспорт";
    : toggle { key = "chk_xls";  label = ".xls"; }
    : toggle { key = "chk_acad"; label = "Карта AutoCAD"; }
  }

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