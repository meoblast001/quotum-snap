#!/usr/bin/env bash

cwd="$( cd "${BASH_SOURCE[0]%/*}" && pwd )"
cd "$cwd/.."
f=`mktemp -d`
git clone git@github.com:meoblast001/quotum.git "$f/quotum.git"
cabal haddock --executables
pushd "$f/quotum.git"
  git checkout gh-pages && git rm -rf *
popd
mv dist/doc/html/quotum/quotum/* "$f/quotum.git/"
pushd "$f/quotum.git"
  git add -A
  git commit -m "Haddock deploy."
  git push origin gh-pages
popd
rm -rf "$f"

if [ $? == 0 ]; then
  echo "*** Done: http://meoblast001.github.io/quotum/"
  exit 0
else
  echo "*** ERROR!!! Fix the above and try again."
  exit 1
fi
