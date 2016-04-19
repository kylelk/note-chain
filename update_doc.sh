CURRENT_BRANCH=$(git symbolic-ref --short HEAD)
gnatdoc -P note_chain.gpr --no-subprojects %X -l --enable-build

git add -A
git commit
git push origin $CURRENT_BRANCH
git checkout gh-pages
git merge $CURRENT_BRANCH
git push origin gh-pages
git checkout $CURRENT_BRANCH

