task_dispatcher : dialog {
    label = "Выбор слоёв и задачи";
    initial_focus = "lst_layers";

    : row {
        : text {
            label = "Настройка задачи";
            alignment = left;
            width = 45;
        }
        : spacer { width = 1; }
        : button {
            key = "btn_help";
            label = "?";
            fixed_width = true;
            width = 4;
            is_cancel = false;
        }
    }

    : spacer { height = 0.3; }

    : toggle {
        key = "chk_group_filter";
        label = "Групповой фильтр Фасад/Витражи/Фонари";
    }

    : spacer { height = 0.2; }

    : row {
        fixed_width = true;

        : boxed_column {
            label = "Слои";
            width = 34;
            : list_box {
                key = "lst_layers";
                width = 32;
                height = 15;
                multiple_select = true;
                fixed_width = true;
            }
            : text {
                label = "Если слои не выбраны — поиск по всем слоям";
                width = 32;
            }
        }

        : spacer { width = 2; }

        : boxed_column {
            label = "Блоки";
            width = 38;
            : list_box {
                key = "lst_blocks";
                width = 36;
                height = 15;
                multiple_select = true;
                is_enabled = false;
                fixed_width = true;
            }
            : text {
                label = "Пока не используется";
                width = 36;
            }
        }
    }

    : spacer { height = 0.5; }

    : row {
        : boxed_radio_column {
            label = "Режим отчёта";
            width = 34;

            : radio_button {
                key = "rb_detail";
                label = "Подробный";
            }

            : radio_button {
                key = "rb_summary";
                label = "Краткий";
            }
        }

        : spacer { width = 2; }

        : boxed_column {
            label = "Вывод";
            width = 34;

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

    : spacer { height = 0.5; }

    : boxed_radio_column {
        label = "Задачи";
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
            key = "rb_task_steklopakety";
            label = "Стеклопакеты";
        }
    }

    : spacer { height = 0.5; }

    : row {
        alignment = centered;

        : button {
            key = "btn_save";
            label = "Сохранить";
            is_default = true;
            fixed_width = true;
            width = 14;
        }

        : button {
            key = "btn_saveas";
            label = "Сохранить как...";
            fixed_width = true;
            width = 18;
        }

        : button {
            key = "btn_close";
            label = "Закрыть";
            is_cancel = true;
            fixed_width = true;
            width = 14;
        }
    }
}
