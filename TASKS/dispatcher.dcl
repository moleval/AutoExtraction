task_dispatcher : dialog {
  label = "Выбор слоёв и задачи";
  initial_focus = "lst_layers";

  : row {
    : text { label = "Настройка задачи"; alignment = left; }
    : spacer { width = 1; }
    : button { key = "btn_help"; label = "?"; fixed_width = true; width = 4; }
  }

  : spacer { height = 0.3; }

: row {
  alignment = left;
  : toggle { key = "chk_filter_facades"; label = "Фасады"; }
  : toggle { key = "chk_filter_vitrazh";  label = "Витражи"; }
  : toggle { key = "chk_filter_fonar";    label = "Фонарь 3D"; }
}

  : spacer { height = 0.2; }

  : row {
    : boxed_column {
      label = "Слои";
      : list_box { key = "lst_layers"; width = 32; height = 15; multiple_select = true; }
      : text { label = "Если слои не выбраны - поиск по всем слоям"; }
    }
    : spacer { width = 2; }
    : boxed_column {
      label = "Блоки";
      : list_box { key = "lst_blocks"; width = 36; height = 15; multiple_select = true; }
      : text { label = "Пока не используется"; }
    }
  }

  : spacer { height = 0.5; }

  : row {
    : boxed_radio_column {
      label = "Режим отчёта";
      : radio_button { key = "rb_detail";  label = "Подробный"; }
      : radio_button { key = "rb_summary"; label = "Краткий"; }
    }
    : spacer { width = 2; }
    : boxed_column {
      label = "Вывод";
      : toggle { key = "chk_xls";  label = ".xls"; }
      : toggle { key = "chk_txt";  label = ".txt"; }
      : toggle { key = "chk_acad"; label = "AutoCAD"; }
    }
  }

  : spacer { height = 0.5; }

  : boxed_radio_column {
    label = "Задачи";
    : radio_button { key = "rb_task_fasonka";     label = "Фасонка"; }
    : radio_button { key = "rb_task_subsystem";   label = "Подсистема"; }
    : radio_button { key = "rb_task_cladding";    label = "Облицовка"; }
    : radio_button { key = "rb_task_vitrazh";     label = "Витраж"; }
    : radio_button { key = "rb_task_zapolnenie";  label = "Заполнение"; }
  }

  : spacer { height = 0.5; }

  : boxed_row {
    label = "Раскрой";
    : button { key = "btn_nest1d"; label = "Раскрой хлыста"; }
    : button { key = "btn_nest2d"; label = "Раскрой листа"; }
  }

  : spacer { height = 0.5; }

  : row {
    alignment = centered;
    : button { key = "btn_save";   label = "Сохранить";        is_default = true; }
    : button { key = "btn_saveas"; label = "Сохранить как..."; }
    : button { key = "btn_close";  label = "Закрыть";          is_cancel = true; }
  }
}