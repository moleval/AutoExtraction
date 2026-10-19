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

    : row {
      alignment = left;
      children_alignment = left;
      children_fixed_width = true;

      : boxed_column {
        label = "";
        width = 62;
        fixed_width = true;
        children_alignment = left;

        : radio_row {
          key = "rb_row_attr";

          : radio_button {
            key = "rb_mode_attr";
            label = "Атрибуты";
          }
        }

        : edit_box {
          key = "edt_query";
          edit_width = 54;
        }
      }

      : boxed_column {
        label = "";
        width = 62;
        fixed_width = true;
        children_alignment = left;

        : radio_row {
          key = "rb_row_vis";

          : radio_button {
            key = "rb_mode_vis";
            label = "Видимость блока";
          }
        }

        : edit_box {
          key = "edt_value";
          edit_width = 54;
        }
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

