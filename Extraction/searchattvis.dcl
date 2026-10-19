// ============================================================
// searchattvis.dcl — окно поиска по атрибутам и видимостям блоков
// Команды: ПОИСКАТР (режим «Атрибуты»), ПОИСКВИД (режим «Видимость блока»)
// Логика: searchattvis.lsp. Ширины подбираются по скриншоту.
// ============================================================

searchattvis_dialog : dialog {

  label = "Поиск по атрибутам и видимостям";

  : column {

    : radio_row {
      key = "rb_mode";

      : radio_button {
        key = "rb_mode_attr";
        label = "Атрибуты";
      }

      : radio_button {
        key = "rb_mode_vis";
        label = "Видимость блока";
      }
    }

    : edit_box {
      key = "edt_query";
      edit_width = 50;
    }

    : edit_box {
      key = "edt_value";
      edit_width = 50;
    }

    : list_box {
      key = "lst_result";
      multiple_select = true;
      width = 70;
      height = 14;
      fixed_height = true;
    }

    : text {
      key = "txt_status";
      label = "";
    }

    : row {
      alignment = centered;

      : button {
        key = "btn_select";
        label = "Выбрать";
        fixed_width = true;
        width = 16;
      }

      : button {
        key = "btn_cancel";
        label = "Отменить";
        is_cancel = true;
        fixed_width = true;
        width = 16;
      }
    }
  }
}
