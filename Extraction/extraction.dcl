// ============================================================
// extraction.dcl — Диалог AutoExtraction
// ============================================================

extraction_dialog : dialog {

  label = "AutoExtraction";

  initial_focus = "lst_layers";


  // ==========================================================
  // ЗАГОЛОВОК: РАСКРОЙ + НАСТРОЙКИ
  // ==========================================================

  : row {

    // Блок раскроя уже ширины окна: справа должно остаться место
    // на «Настройки» и «?»
    : boxed_column {

      label = "Раскрой";

      width = 74;
      fixed_width = true;

      : row {
        : button {
          key = "btn_cutline";
          label = "Раскрой хлыста";
        }

        : button {
          key = "btn_cutsheet";
          label = "Раскрой листа";
        }
      }

      // Нижний отступ как у панели «Блоки» под кнопкой «Копия»
      : spacer {
        height = 0.1;
      }
    }

    : spacer {
      // Гибкий отступ держит «Настройки» и «?» у правого края
      width = 1;
      fixed_width = false;
    }

    : column {
      fixed_width = true;
      children_fixed_width = true;

      // Опустить кнопки на высоту заголовка рамки «Раскрой»,
      // чтобы они встали вровень с кнопками раскроя
      : spacer {
        height = 1;
      }

      : row {
        fixed_width = true;
        children_fixed_width = true;

        : button {
          key = "btn_settings";
          label = "Настройки";
          fixed_width = true;
          width = 12;
        }

        : button {
          key = "btn_help";
          label = "?";
          fixed_width = true;
          width = 4;
        }
      }
    }
  }


  : spacer {
    height = 0.3;
  }


  // ==========================================================
  // ФИЛЬТРЫ
  // ==========================================================

  : row {

    // Колонка по ширине панели «Слои»
    // Рамка без заголовка: тот же отступ, что у панели ниже, поэтому
    // чекбоксы встают ровно над полем поиска слоёв
    : boxed_column {
      label = "";
      width = 45;
      fixed_width = true;
      children_alignment = left;
      children_fixed_width = true;

      // Фильтры слоёв: вплотную друг к другу
      : row {
        fixed_width = true;
        children_fixed_width = true;
        alignment = left;

        : toggle {
          key = "chk_filter_my";
          label = "Мои";
        }

        : toggle {
          key = "chk_filter_facades";
          label = "Фасады";
        }

        : toggle {
          key = "chk_filter_vitrazh";
          label = "Витражи";
        }

        : toggle {
          key = "chk_filter_windows";
          label = "Окна";
        }
      }

      // Нижний отступ как у панели «Блоки» под кнопкой «Копия»
      : spacer {
        height = 0.1;
      }
    }


    // Тот же отступ между колонками, что у панелей ниже
    : spacer {
      width = 2;
    }


    // Колонка по ширине панели «Блоки» (рамка без заголовка — см. выше)
    : boxed_column {
      label = "";
      width = 45;
      fixed_width = true;
      children_alignment = left;
      children_fixed_width = true;

      : toggle {
        key = "chk_filter_anonymous";
        label = "Анонимные блоки";
        alignment = left;
        fixed_width = true;
      }

      // Нижний отступ как у панели «Блоки» под кнопкой «Копия»
      : spacer {
        height = 0.1;
      }
    }
  }


  : spacer {
    height = 0.2;
  }


  // ==========================================================
  // ВЕРХНИЙ РЯД: СЛОИ | БЛОКИ
  // ==========================================================

  : row {

    // --------------------------------------------------------
    // СЛОИ
    // --------------------------------------------------------

    : boxed_column {

      label = "Слои";

      width = 45;
      fixed_width = true;

      // Ширины НЕ задаются: каждый элемент заполняет ширину панели,
      // поэтому их левые и правые края совпадают по построению
      // (подсказка в поле видна, пока оно пусто)
      : edit_box {
        key = "edt_layer_search";
      }

      : list_box {

        key = "lst_layers";

        height = 13;

        multiple_select = true;
      }

      : text {
        key = "txt_layers_hint";
        label = "Если слои не выбраны — поиск по всем слоям";
      }

      // Компенсация высоты: в панели «Блоки» на этом месте поле ввода
      // («Введите новое имя»), оно выше текста статуса на свою рамку.
      // Без этого отступа кнопки слоёв стоят чуть выше кнопок блоков.
      : spacer {
        height = 0.1;
      }

      // ------------------------------------------------------
      // КНОПКИ ВЫБРАТЬ ВСЕ + СНЯТЬ ВЫДЕЛЕНИЕ — ниже строки
      // состояния, на уровне кнопок блоков (Копия/Вставить/Имя).
      // Ряд заполняет ширину панели, кнопки делят её поровну:
      // правый край ряда совпадает с правым краем поля поиска.
      // ------------------------------------------------------

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

      width = 45;
      fixed_width = true;

      // Ширины НЕ задаются: каждый элемент заполняет ширину панели,
      // поэтому их левые и правые края совпадают по построению
      // (подсказка в поле видна, пока оно пусто)
      : edit_box {
        key = "edt_block_search";
      }

      : list_box {
        key = "lst_blocks";
        height = 13;
        multiple_select = false;
      }

      : edit_box {
        key = "edt_block_rename";
      }

      // ------------------------------------------------------
      // КНОПКИ КОПИЯ + ВСТАВИТЬ + ИМЯ
      // Ряд заполняет ширину панели, кнопки делят её поровну:
      // правый край ряда совпадает с правым краем поля поиска.
      // ------------------------------------------------------

      : row {
        : button {
          key = "btn_block_copy";
          label = "Копия";
        }

        : button {
          key = "btn_block_insert";
          label = "Вставить";
        }

        : button {
          key = "btn_block_rename";
          label = "Имя";
        }
      }
    }
  }


  : spacer {
    height = 0.5;
  }


  // ==========================================================
  // СРЕДНИЙ РЯД: ЗАДАЧИ | СЛОИ ПОДСИСТЕМЫ
  // ==========================================================

  : row {

    : boxed_radio_column {

      label = "Задачи";

      width = 45;
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

      // Нижний отступ как у панели «Блоки» под кнопкой «Копия»
      : spacer {
        height = 0.1;
      }
    }


    : spacer {
      width = 2;
    }


    : boxed_column {

      label = "Слои подсистемы";

      key = "box_subsystem_layers";

      width = 45;
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

      // Нижний отступ как у панели «Блоки» под кнопкой «Копия»
      : spacer {
        height = 0.1;
      }
    }
  }


  : spacer {
    height = 0.5;
  }


  // ==========================================================
  // НИЖНИЙ РЯД: РЕЖИМ ОТЧЕТА | ВЫВОД
  // ==========================================================

  : row {

    : boxed_radio_column {

      label = "Режим отчета";

      width = 45;
      fixed_width = true;

      : radio_button {
        key = "rb_detail";
        label = "Подробный";
      }

      : radio_button {
        key = "rb_summary";
        label = "Краткий";
      }

      // Нижний отступ как у панели «Блоки» под кнопкой «Копия»
      : spacer {
        height = 0.1;
      }
    }


    : spacer {
      width = 2;
    }


    : boxed_column {

      label = "Вывод";

      width = 45;
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

      // Нижний отступ как у панели «Блоки» под кнопкой «Копия»
      : spacer {
        height = 0.1;
      }
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
