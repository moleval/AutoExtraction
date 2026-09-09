#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Скрипт автоэкспорта кода проекта AutoExtraction в текстовые файлы.
Поддерживает три режима:
  1. Обычный        — один общий файл (по умолчанию).
  2. --split-dirs   — отдельный файл для каждого подкаталога.
  3. --split-files  — отдельный файл для каждого исходного файла.

Кодировки обрабатываются автоматически:
  .lsp, .dcl, .prj -> windows-1251
  .md -> utf-8
"""

import os
import sys
import argparse
from pathlib import Path

EXTENSION_ENCODING = {
    '.lsp': 'windows-1251',
    '.dcl': 'windows-1251',
    '.prj': 'windows-1251',
    '.md':  'utf-8'
}

def read_file_with_fallback(filepath, encoding):
    """Читает файл, пытаясь сначала указанную кодировку, затем utf-8."""
    try:
        with open(filepath, 'r', encoding=encoding) as f:
            return f.read()
    except UnicodeDecodeError:
        with open(filepath, 'r', encoding='utf-8') as f:
            return f.read()
    except Exception as e:
        return f'[ОШИБКА ЧТЕНИЯ: {e}]'

def collect_files(root, include_docs, exclude_dirs):
    """Возвращает список кортежей (абсолютный путь, относительный путь, расширение)."""
    extensions = {'.lsp', '.dcl', '.prj'}
    if include_docs:
        extensions.add('.md')

    files = []
    for dirpath, dirnames, filenames in os.walk(root):
        dirnames[:] = [d for d in dirnames if d not in exclude_dirs]
        dirnames.sort()
        filenames.sort()

        for filename in filenames:
            filepath = Path(dirpath) / filename
            ext = filepath.suffix.lower()
            if ext in extensions:
                rel_path = filepath.relative_to(root)
                files.append((filepath, rel_path, ext))
    return files

def write_single_file(files, root, output_file):
    """Собирает все файлы в один выходной файл."""
    output = []
    total = 0
    for filepath, rel_path, ext in files:
        encoding = EXTENSION_ENCODING.get(ext, 'utf-8')
        content = read_file_with_fallback(filepath, encoding)
        output.append('=' * 80)
        output.append(f'ФАЙЛ: {rel_path}')
        output.append('=' * 80)
        output.append(content)
        output.append('\n' + '-' * 80 + '\n')
        total += 1

    with open(output_file, 'w', encoding='utf-8') as f:
        f.write('\n'.join(output))
    print(f'Экспортировано файлов: {total}')
    print(f'Выходной файл: {output_file}')

def write_by_dirs(files, root, output_dir):
    """Создаёт отдельный .txt для каждого подкаталога."""
    groups = {}
    for filepath, rel_path, ext in files:
        # Первая часть относительного пути — имя папки (или '.' для корня)
        parts = rel_path.parts
        if len(parts) == 1:
            dir_name = '_root'
        else:
            dir_name = parts[0]
        groups.setdefault(dir_name, []).append((filepath, rel_path, ext))

    os.makedirs(output_dir, exist_ok=True)
    for dir_name, group_files in groups.items():
        safe_name = dir_name.replace('\\', '_').replace('/', '_')
        out_file = Path(output_dir) / f'AutoExtraction_{safe_name}.txt'
        write_single_file(group_files, root, out_file)

    print(f'Создано файлов по каталогам: {len(groups)}')
    print(f'Каталог: {output_dir}')

def write_by_files(files, root, output_dir):
    """Создаёт отдельный .txt для каждого исходного файла."""
    os.makedirs(output_dir, exist_ok=True)
    count = 0
    for filepath, rel_path, ext in files:
        encoding = EXTENSION_ENCODING.get(ext, 'utf-8')
        content = read_file_with_fallback(filepath, encoding)
        out_file = Path(output_dir) / (rel_path.stem + rel_path.suffix + '.txt')
        # Используем безопасное имя без подпапок
        out_file = Path(output_dir) / (rel_path.name + '.txt')
        with open(out_file, 'w', encoding='utf-8') as f:
            f.write(content)
        count += 1
    print(f'Создано файлов по одному: {count}')
    print(f'Каталог: {output_dir}')

def main():
    parser = argparse.ArgumentParser(description='Экспорт кода проекта AutoExtraction')
    parser.add_argument('root', nargs='?', default='.', help='Корневой каталог проекта')
    parser.add_argument('--include-docs', action='store_true', help='Включать .md файлы')
    parser.add_argument('--exclude', nargs='*', default=['.git', '__pycache__'], help='Исключаемые каталоги')
    parser.add_argument('-o', '--output', default='AutoExtraction_export.txt', help='Имя выходного файла (для обычного режима)')
    parser.add_argument('--split-dirs', action='store_true', help='Разделить по каталогам')
    parser.add_argument('--split-files', action='store_true', help='Разделить по отдельным файлам')
    parser.add_argument('--out-dir', default='export_split', help='Каталог для разделённых файлов')

    args = parser.parse_args()
    root = Path(args.root).resolve()
    if not root.is_dir():
        print(f'Ошибка: {root} не существует.')
        return

    files = collect_files(root, args.include_docs, args.exclude)

    if args.split_files:
        write_by_files(files, root, args.out_dir)
    elif args.split_dirs:
        write_by_dirs(files, root, args.out_dir)
    else:
        write_single_file(files, root, args.output)

if __name__ == '__main__':
    main()