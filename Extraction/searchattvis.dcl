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
        width = 60.4;
        fixed_width = true;
        children_alignment = left;

        : radio_row {
          key = "rb_row_attr";

          : radio_button {
            key = "rb_mode_attr";
            label = "Атрибуты";
          }
        }

        : row {
          alignment = centered;

          : edit_box {
            key = "edt_query";
            edit_width = 56;
          }
        }

        : spacer {
          height = 1.2;
        }
      }

      : boxed_column {
        label = "";
        width = 60.4;
        fixed_width = true;
        children_alignment = left;

        : radio_row {
          key = "rb_row_vis";

          : radio_button {
            key = "rb_mode_vis";
            label = "Видимость блока";
          }
        }

        : row {
          alignment = centered;

          : edit_box {
            key = "edt_value";
            edit_width = 56;
          }
        }

        : spacer {
          height = 1.2;
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
        key = "btn_count";
        label = "Подсчёт";
        fixed_width = true;
        width = 14;
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

searchattvis_count_n_s : dialog {

  label = "Подсчёт";

  : column {
    children_alignment = left;

    : list_box {
      key = "lst_count";
      width = 176;
      height = 14;
      fixed_width = true;
      fixed_height = true;
      tabs = "28 68 96 124";
      tab_truncate = true;
    }

    : text {
      key = "txt_count_status";
      label = "";
      width = 176;
      fixed_width = true;
    }

    : row {
      alignment = centered;

      : button {
        key = "btn_copy";
        label = "Копировать";
        fixed_width = true;
        width = 16;
      }
      : button {
        key = "btn_acad";
        label = "Таблица AutoCAD";
        fixed_width = true;
        width = 22;
      }
      : button {
        key = "btn_count_close";
        label = "Закрыть";
        is_cancel = true;
        fixed_width = true;
        width = 16;
      }
    }
  }
}

searchattvis_count_n_l : dialog {

  label = "Подсчёт";

  : column {
    children_alignment = left;

    : list_box {
      key = "lst_count";
      width = 208;
      height = 14;
      fixed_width = true;
      fixed_height = true;
      tabs = "28 76 112 148";
      tab_truncate = true;
    }

    : text {
      key = "txt_count_status";
      label = "";
      width = 208;
      fixed_width = true;
    }

    : row {
      alignment = centered;

      : button {
        key = "btn_copy";
        label = "Копировать";
        fixed_width = true;
        width = 16;
      }
      : button {
        key = "btn_acad";
        label = "Таблица AutoCAD";
        fixed_width = true;
        width = 22;
      }
      : button {
        key = "btn_count_close";
        label = "Закрыть";
        is_cancel = true;
        fixed_width = true;
        width = 16;
      }
    }
  }
}

searchattvis_count_m_s : dialog {

  label = "Подсчёт";

  : column {
    children_alignment = left;

    : list_box {
      key = "lst_count";
      width = 200;
      height = 14;
      fixed_width = true;
      fixed_height = true;
      tabs = "28 52 92 120 148";
      tab_truncate = true;
    }

    : text {
      key = "txt_count_status";
      label = "";
      width = 200;
      fixed_width = true;
    }

    : row {
      alignment = centered;

      : button {
        key = "btn_copy";
        label = "Копировать";
        fixed_width = true;
        width = 16;
      }
      : button {
        key = "btn_acad";
        label = "Таблица AutoCAD";
        fixed_width = true;
        width = 22;
      }
      : button {
        key = "btn_count_close";
        label = "Закрыть";
        is_cancel = true;
        fixed_width = true;
        width = 16;
      }
    }
  }
}

searchattvis_count_m_l : dialog {

  label = "Подсчёт";

  : column {
    children_alignment = left;

    : list_box {
      key = "lst_count";
      width = 252;
      height = 14;
      fixed_width = true;
      fixed_height = true;
      tabs = "28 72 120 156 192";
      tab_truncate = true;
    }

    : text {
      key = "txt_count_status";
      label = "";
      width = 252;
      fixed_width = true;
    }

    : row {
      alignment = centered;

      : button {
        key = "btn_copy";
        label = "Копировать";
        fixed_width = true;
        width = 16;
      }
      : button {
        key = "btn_acad";
        label = "Таблица AutoCAD";
        fixed_width = true;
        width = 22;
      }
      : button {
        key = "btn_count_close";
        label = "Закрыть";
        is_cancel = true;
        fixed_width = true;
        width = 16;
      }
    }
  }
}
