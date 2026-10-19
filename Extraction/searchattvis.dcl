// ============================================================
// searchattvis.dcl — окно поиска по атрибутам и видимостям блоков
// Команды: ПОИСКАТР (режим «Атрибуты»), ПОИСКВИД (режим «Видимость блока»)
// Логика: searchattvis.lsp. Ширины подбираются по скриншоту.
// ============================================================

searchattvis_dialog : dialog {

  label = "Поиск по атрибутам и видимостям";

  : column {
    children_alignment = left;

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

    : row {
      alignment = left;
      children_alignment = left;
      children_fixed_width = true;

      : edit_box {
        key = "edt_query";
        edit_width = 50;
      }
    }

    : row {
      alignment = left;
      children_alignment = left;
      children_fixed_width = true;

      : edit_box {
        key = "edt_value";
        edit_width = 50;
      }
    }

    : row {
      alignment = left;
      children_alignment = left;

      : column {
        : text {
          key = "txt_blocks_label";
          label = "Блоки";
        }
        : list_box {
          key = "lst_blocks";
          multiple_select = true;
          width = 35;
          height = 14;
          fixed_height = true;
        }
      }

      : column {
        : text {
          key = "txt_items_label";
          label = "Атрибуты";
        }
        : list_box {
          key = "lst_items";
          multiple_select = true;
          width = 35;
          height = 14;
          fixed_height = true;
        }
      }
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
        key = "btn_reset";
        label = "Сбросить выбор";
        fixed_width = true;
        width = 18;
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
