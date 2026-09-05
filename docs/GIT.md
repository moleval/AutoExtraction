# Git / GitHub

Из корня проекта:

```bash
git init -b main
git add .
git commit -m "Initial project structure"
```

Проверка:

```bash
git status
git branch
```

Для новой функциональности:

```bash
git switch -c feature/fasonka
```

После завершения этапа:

```bash
git add .
git commit -m "feat: ..."
git push -u origin feature/fasonka
```

Рекомендуется использовать `main`, а не `master`, чтобы не зависеть от старого имени ветки по умолчанию.
