# Структура проекта AutoExtraction

AutoExtraction/
??? Extraction/
? ??? extraction.lsp # Диспетчер задач (команда EXTRACTION / ЭКСТРАКЦИЯ)
? ??? extraction.dcl # Диалоговое окно диспетчера
? ??? fasonka.lsp # Задача «Фасонка» (подсчёт погонажа)
? ??? subsystem.lsp # Задача «Подсистема» (подсчёт элементов)
? ??? cutline.lsp # Модуль линейного раскроя (CUTLINE)
? ??? cutsheet.lsp # Заглушка модуля раскроя листа (CUTSHEET)
??? common/
? ??? task-utils.lsp # Универсальные утилиты (работа со строками, файлами)
? ??? layer-utils.lsp # Работа со слоями и групповыми фильтрами
? ??? select-utils.lsp # Выбор блоков и извлечение динамических свойств
? ??? excel-utils.lsp # Экспорт в Excel (XML Spreadsheet) и CSV
? ??? table-utils.lsp # Создание таблиц AutoCAD
? ??? txt-utils.lsp # Экспорт в текстовый формат (GAL)
??? docs/
? ??? promt.md # Техническое задание (промт)
? ??? requirements.md # Функциональные требования
? ??? requirements-dev.md # Требования к разработке и окружению
? ??? structure.md # Этот файл
? ??? START.md # Инструкция по быстрому старту
??? reload.lsp # Команда RELOAD для перезагрузки всех модулей
??? AutoExtraction.prj # Файл проекта Visual LISP
??? .gitignore # Игнорирование временных файлов
??? README.md # Краткое описание проекта