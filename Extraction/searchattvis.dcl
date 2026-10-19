// ============================================================
// searchattvis.dcl — окно поиска по атрибутам и видимостям блоков
// Команды: ПОИСКАТР (режим «Атрибуты»), ПОИСКВИД (режим «Видимость блока»)
// Логика: searchattvis.lsp.
// Окно широкое: поля ввода и списки стоят в ряд. Размер задаётся здесь,
// во время работы окно не растягивается (ограничение DCL).
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
        edit_width = 58;
      }

      : edit_box {
        key = "edt_value";
        edit_width = 58;
      }
    }

    : row {
      alignment = left;
      children_alignment = left;

      : column {
        children_alignment = left;

        : text {
          key = "txt_blocks_label";
          label = "Блоки";
        }
        : list_box {
          key = "lst_blocks";
          multiple_select = true;
          width = 62;
          height = 14;
          fixed_width = true;
          fixed_height = true;
        }
      }

      : column {
        children_alignment = left;

        : text {
          key = "txt_items_label";
          label = "Атрибуты";
        }
        : list_box {
          key = "lst_items";
          multiple_select = true;
          width = 62;
          height = 14;
          fixed_width = true;
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

