// ============================================================
// settings.dcl — настройки AutoExtraction
// ============================================================

settings_dialog : dialog {
  label = "Настройки AutoExtraction";

  : boxed_column {
    label = "Задача";
    : popup_list {
      key = "popup_task";
      label = "Тип задачи:";
      width = 34;
    }
  }

  : boxed_column {
    label = "Входные данные";

    : edit_box {
      key = "edt_layers";
      label = "Слои (маски через ;):";
      edit_width = 62;
    }

    : edit_box {
      key = "edt_blocks";
      label = "Блоки (маски через ;):";
      edit_width = 62;
    }

    : edit_box {
      key = "edt_poly_layers";
      label = "Облицовка: слои полилиний:";
      edit_width = 62;
    }

    : edit_box {
      key = "edt_block_layers";
      label = "Облицовка: слои блоков:";
      edit_width = 62;
    }

    : edit_box {
      key = "edt_block_names";
      label = "Облицовка: имена блоков:";
      edit_width = 62;
    }

    : edit_box {
      key = "edt_frame_allowance";
      label = "Заполнение: припуск на раму, мм:";
      edit_width = 62;
    }

    : text {
      label = "* — любое количество символов; ? — один символ; маски объединяются по ИЛИ.";
    }
  }

  : boxed_column {
    label = "Выход";

    : edit_box {
      key = "edt_table_layer";
      label = "Слой таблицы (CURRENT = текущий):";
      edit_width = 40;
    }

    : edit_box {
      key = "edt_block_template";
      label = "Имя выходного блока ({DWG} = имя чертежа):";
      edit_width = 40;
    }

    : edit_box {
      key = "edt_insert_layer";
      label = "Слой вставки блока (CURRENT = текущий):";
      edit_width = 40;
    }

    : edit_box {
      key = "edt_frame_layer";
      label = "Слой рамки карты:";
      edit_width = 40;
    }
  }

  : row {
    alignment = centered;

    : button {
      key = "btn_defaults";
      label = "По умолчанию";
    }

    : button {
      key = "btn_save";
      label = "Сохранить";
      is_default = true;
    }

    : button {
      key = "btn_cancel";
      label = "Отмена";
      is_cancel = true;
    }
  }
}

// Реестр плагинов (секция [plugins] settings.ini). Открывается из
// settings_dialog; список редактируется в буфере до «Сохранить».
plugins_dialog : dialog {
  label = "Плагины AutoExtraction";

  : boxed_column {
    label = "Плагины (порядок загрузки - сверху вниз)";

    : list_box {
      key = "lst_plugins";
      width = 80;
      height = 12;
      multiple_select = false;
    }

    : text {
      key = "txt_plugin_status";
      label = "";
      width = 80;
    }
  }

  : row {
    alignment = centered;

    : button {
      key = "btn_plugin_add";
      label = "Добавить…";
    }

    : button {
      key = "btn_plugin_change";
      label = "Изменить путь…";
    }

    : button {
      key = "btn_plugin_remove";
      label = "Удалить";
    }
  }

  : row {
    alignment = centered;

    : button {
      key = "btn_plugins_ok";
      label = "ОК";
      is_default = true;
    }

    : button {
      key = "btn_plugins_cancel";
      label = "Отмена";
      is_cancel = true;
    }
  }
}
