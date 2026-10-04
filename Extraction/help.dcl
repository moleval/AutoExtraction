// ============================================================
// help.dcl — справка AutoExtraction
// ============================================================

extraction_help_dialog : dialog {
  label = "Справка AutoExtraction";

  : list_box {
    key = "help_lines";
    width = 100;
    height = 28;
    fixed_width = true;
    fixed_height = true;
    fixed_width_font = true;
  }

  : row {
    alignment = right;
    : button {
      key = "btn_help_close";
      label = "Закрыть";
      is_cancel = true;
      is_default = true;
      width = 12;
      fixed_width = true;
    }
  }
}
