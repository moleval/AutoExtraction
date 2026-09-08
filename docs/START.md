# Быстрый старт AutoExtraction

## 1. VS Code

Откройте папку `D:\AutoExtraction`.

Установите расширение **AutoCAD AutoLISP Extension** от Autodesk.

Проверьте, что VS Code использует настройки из `.vscode/settings.json`.

## 2. AutoCAD

Для AutoCAD 2023 можно использовать Unicode-режим AutoLISP:

```text
LISPSYS = 1
```

После изменения `LISPSYS` AutoCAD необходимо перезапустить.

## 3. Пути AutoCAD

Для загрузки проекта добавьте как минимум:

```text
D:\AutoExtraction\Extraction
D:\AutoExtraction\common
```

в **Support File Search Path**.

В **Trusted Locations** безопаснее добавить корень:

```text
D:\AutoExtraction\...
```

Окончание `\...` означает доверие также подпапкам.

## 4. Первая загрузка

Выполните:

```text
APPLOAD
```

Загрузите:

```text
D:\AutoExtraction\Extraction\extraction.lsp
```

Диспетчер автоматически подгрузит общие библиотеки и `fasonka.lsp`.

## 5. Проверка

В командной строке AutoCAD:

```text
Extraction
```

Откроется окно выбора задачи.

Также можно выполнить прямой вызов:

```lisp
(fasonka-main nil "DETAIL" T nil T nil)
```

## 6. Что должно работать сейчас

- список слоёв;
- выбор нескольких слоёв;
- режим DETAIL/SUMMARY;
- фильтр по именам групповых фильтров, содержащим `Фасад`, `Витраж` или `Фонар`;
- экспорт Excel `.xls` при наличии COM Excel;
- TXT;
- статическая таблица AutoCAD без формул;
- команда `Extraction`;
- команда `Экстракция`.

## 7. Что пока является заглушкой

Задачи `SUBSYSTEM`, `CLADDING`, `VITRAZH`, `STEKLOPAKETY` зарегистрированы в диспетчере, но их бизнес-модули ещё не реализованы.

Блок `Блоки` в DCL пока отключён.
