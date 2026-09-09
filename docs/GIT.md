# Git / GitHub

git branch

Для новой функциональности:

git switch -c feature/fasonka

После завершения этапа:

git add .
git commit -m "feat: ..."
git push -u origin feature/fasonka

Рекомендуется использовать main, а не master, чтобы не зависеть от старого имени ветки по умолчанию.