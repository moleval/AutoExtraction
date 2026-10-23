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

    : boxed_column {
      label = "Режим";
      width = 124;
      fixed_width = true;
      children_alignment = left;

      : radio_row {
        key = "rb_row_mode";

        : radio_button {
          key = "rb_mode_all";
          label = "Все блоки";
        }
        : radio_button {
          key = "rb_mode_attr";
          label = "Блоки с атрибутами";
        }
        : radio_button {
          key = "rb_mode_vis";
          label = "Видимости блоков";
        }
      }
    }

    : boxed_column {
      label = "Поиск";
      width = 124;
      fixed_width = true;
      children_alignment = left;

      : row {
        alignment = left;

        : edit_box {
          key = "edt_name";
          edit_width = 36;
        }
        : edit_box {
          key = "edt_query";
          edit_width = 36;
        }
        : edit_box {
          key = "edt_value";
          edit_width = 36;
        }
      }
      : text {
        label = " ";
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
        label = "Закрыть";
        is_cancel = true;
        fixed_width = true;
        width = 16;
      }
    }
  }
}

searchattvis_count_n : dialog {

  label = "Подсчёт";

  : column {
    children_alignment = left;

    : list_box {
      key = "lst_count";
      width = 59;
      height = 40;
      fixed_width = true;
      fixed_height = true;
      tabs = "7 18 29 38 52";
    }

    : text {
      key = "txt_count_status";
      label = "";
      width = 59;
      fixed_width = true;
      alignment = right;
    }

    : row {
      alignment = centered;

      : button {
        key = "btn_copy";
        label = "Копировать";
        fixed_width = true;
        width = 14;
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
        width = 12;
      }
    }
  }
}

searchattvis_count_m : dialog {

  label = "Подсчёт";

  : column {
    children_alignment = left;

    : list_box {
      key = "lst_count";
      width = 85;
      height = 40;
      fixed_width = true;
      fixed_height = true;
      tabs = "7 34 44 55 65 78";
    }

    : text {
      key = "txt_count_status";
      label = "";
      width = 85;
      fixed_width = true;
      alignment = right;
    }

    : row {
      alignment = centered;

      : button {
        key = "btn_copy";
        label = "Копировать";
        fixed_width = true;
        width = 14;
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
        width = 12;
      }
    }
  }
}

searchattvis_count_ln : dialog {

  label = "Подсчёт";

  : column {
    children_alignment = left;

    : list_box {
      key = "lst_count";
      width = 50;
      height = 40;
      fixed_width = true;
      fixed_height = true;
      tabs = "7 18 28 41";
    }

    : text {
      key = "txt_count_status";
      label = "";
      width = 50;
      fixed_width = true;
      alignment = right;
    }

    : row {
      alignment = centered;

      : button {
        key = "btn_copy";
        label = "Копировать";
        fixed_width = true;
        width = 14;
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
        width = 12;
      }
    }
  }
}

searchattvis_count_lm : dialog {

  label = "Подсчёт";

  : column {
    children_alignment = left;

    : list_box {
      key = "lst_count";
      width = 74;
      height = 40;
      fixed_width = true;
      fixed_height = true;
      tabs = "7 34 44 54 67";
    }

    : text {
      key = "txt_count_status";
      label = "";
      width = 74;
      fixed_width = true;
      alignment = right;
    }

    : row {
      alignment = centered;

      : button {
        key = "btn_copy";
        label = "Копировать";
        fixed_width = true;
        width = 14;
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
        width = 12;
      }
    }
  }
}

searchattvis_count_xn : dialog {

  label = "Подсчёт";

  : column {
    children_alignment = left;

    : list_box {
      key = "lst_count";
      width = 59;
      height = 24;
      fixed_width = true;
      fixed_height = true;
      tabs = "7 18 29 38 52";
    }

    : list_box {
      key = "lst_count2";
      width = 50;
      height = 16;
      fixed_width = true;
      fixed_height = true;
      tabs = "7 18 28 41";
    }

    : text {
      key = "txt_count_status";
      label = "";
      width = 59;
      fixed_width = true;
      alignment = right;
    }

    : row {
      alignment = centered;

      : button {
        key = "btn_copy";
        label = "Копировать";
        fixed_width = true;
        width = 14;
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
        width = 12;
      }
    }
  }
}

searchattvis_count_xm : dialog {

  label = "Подсчёт";

  : column {
    children_alignment = left;

    : list_box {
      key = "lst_count";
      width = 85;
      height = 24;
      fixed_width = true;
      fixed_height = true;
      tabs = "7 34 44 55 65 78";
    }

    : list_box {
      key = "lst_count2";
      width = 74;
      height = 16;
      fixed_width = true;
      fixed_height = true;
      tabs = "7 34 44 54 67";
    }

    : text {
      key = "txt_count_status";
      label = "";
      width = 85;
      fixed_width = true;
      alignment = right;
    }

    : row {
      alignment = centered;

      : button {
        key = "btn_copy";
        label = "Копировать";
        fixed_width = true;
        width = 14;
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
        width = 12;
      }
    }
  }
}
