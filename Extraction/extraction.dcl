// ============================================================
// extraction.dcl — Диалог AutoExtraction
// ============================================================

extraction_dialog : dialog {

  label = "AutoExtraction";

  initial_focus = "lst_layers";


  // ==========================================================
  // ЗАГОЛОВОК
  // ==========================================================

  : row {
    : text {
      label = "Настройка задачи";
      alignment = left;
    }

    : spacer {
      width = 1;
    }

    : button {
      key = "btn_help";
      label = "?";
      fixed_width = true;
      width = 4;
    }
  }


  : spacer {
    height = 0.3;
  }


  // ==========================================================
  // ФИЛЬТРЫ
  // ==========================================================

  : row {
    alignment = left;

    : toggle {
      key = "chk_filter_facades";
      label = "Фасады";
    }

    : toggle {
      key = "chk_filter_vitrazh";
      label = "Витражи";
    }

    : toggle {
      key = "chk_filter_fonar";
      label = "Фонарь 3D";
    }

    : toggle {
      key = "chk_filter_anonymous";
      label = "Анонимные блоки";
    }
  }


  : spacer {
    height = 0.2;
  }


  // ==========================================================
  // ВЕРХНИЙ РЯД: СЛОИ | БЛОКИ
  //
  // Слои:
  //   1 строка кнопок
  //   14 строк list_box
  //   1 строка text
  //   = 16
  //
  // Блоки:
  //   1 строка edit_box
  //   13 строк list_box
  //   1 строка edit_box
  //   1 строка button
  //   = 16
  // ==========================================================

  : row {

    // --------------------------------------------------------
    // СЛОИ
    // --------------------------------------------------------

    : boxed_column {

      label = "Слои";

      width = 40;
      fixed_width = true;

      : row {

        : button {
          key = "btn_select_all";
          label = "Выбрать все";
        }

        : button {
          key = "btn_clear_all";
          label = "Снять выделение";
        }
      }

      : list_box {

        key = "lst_layers";

        width = 36;
        height = 14;

        multiple_select = true;
      }

      : text {
        label = "Если слои не выбраны — поиск по всем слоям";
      }
    }


    // --------------------------------------------------------
    // ПРОМЕЖУТОК
    // --------------------------------------------------------

    : spacer {
      width = 2;
    }


    // --------------------------------------------------------
    // БЛОКИ
    // --------------------------------------------------------

    : boxed_column {

      label = "Блоки";

      width = 40;
      fixed_width = true;

      : edit_box {

        key = "edt_block_search";

        label = "Поиск:";

        edit_width = 24;
      }

      : list_box {

        key = "lst_blocks";

        width = 36;
        height = 13;

        multiple_select = false;
      }

      : edit_box {

        key = "edt_block_rename";

        label = "Новое имя:";

        edit_width = 24;
      }

      : button {

        key = "btn_block_rename";

        label = "Переименовать";
      }
    }
  }


  : spacer {
    height = 0.5;
  }


  // ==========================================================
  // СРЕДНИЙ РЯД: РЕЖИМ ОТЧЁТА | ВЫВОД
  // ==========================================================

  : row {

    : boxed_radio_column {

      label = "Режим отчёта";

      width = 40;
      fixed_width = true;

      : radio_button {
        key = "rb_detail";
        label = "Подробный";
      }

      : radio_button {
        key = "rb_summary";
        label = "Краткий";
      }
    }


    : spacer {
      width = 2;
    }


    : boxed_column {

      label = "Вывод";

      width = 40;
      fixed_width = true;

      : toggle {
        key = "chk_xls";
        label = ".xls";
      }

      : toggle {
        key = "chk_txt";
        label = ".txt";
      }

      : toggle {
        key = "chk_acad";
        label = "AutoCAD";
      }
    }
  }


  : spacer {
    height = 0.5;
  }


  // ==========================================================
  // НИЖНИЙ РЯД: ЗАДАЧИ | СЛОИ ПОДСИСТЕМЫ
  // ==========================================================

  : row {

    : boxed_radio_column {

      label = "Задачи";

      width = 40;
      fixed_width = true;

      : radio_button {
        key = "rb_task_fasonka";
        label = "Фасонка";
      }

      : radio_button {
        key = "rb_task_subsystem";
        label = "Подсистема";
      }

      : radio_button {
        key = "rb_task_cladding";
        label = "Облицовка";
      }

      : radio_button {
        key = "rb_task_vitrazh";
        label = "Витраж";
      }

      : radio_button {
        key = "rb_task_zapolnenie";
        label = "Заполнение";
      }
    }


    : spacer {
      width = 2;
    }


    : boxed_column {

      label = "Слои подсистемы";

      key = "box_subsystem_layers";

      width = 40;
      fixed_width = true;

      : toggle {
        key = "chk_subsystem_1";
        label = "Подсистема";
      }

      : toggle {
        key = "chk_subsystem_2";
        label = "Подсистема алюминиевая";
      }

      : toggle {
        key = "chk_subsystem_3";
        label = "Подсистема оцинкованная";
      }
    }
  }


  : spacer {
    height = 0.5;
  }


  // ==========================================================
  // РАСКРОЙ
  // ==========================================================

  : boxed_row {

    label = "Раскрой";

    : button {
      key = "btn_cutline";
      label = "Раскрой хлыста";
    }

    : button {
      key = "btn_cutsheet";
      label = "Раскрой листа";
    }
  }


  : spacer {
    height = 0.5;
  }


  // ==========================================================
  // КНОПКИ
  // ==========================================================

  : row {

    alignment = centered;

    : button {
      key = "btn_save";
      label = "Сохранить";
      is_default = true;
    }

    : button {
      key = "btn_saveas";
      label = "Сохранить как...";
    }

    : button {
      key = "btn_close";
      label = "Закрыть";
      is_cancel = true;
    }
  }
}