#!/bin/bash
set -e

rm -rf gh-pages || exit 0;

mkdir -p gh-pages/examples

# compile JS using Elm
cd examples
for i in crate cube first-person thwomp triangle; do
  elm make $i.elm --output ../gh-pages/examples/$i.html
done

# copy the textures
cp -R texture ../gh-pages/examples
cp -R screenshots ../gh-pages/examples

# init branch and commit
cd ../gh-pages
git init
git add .
git commit -m "Deploying to GH Pages"
git push --force "git@github.com:elm-explorations/webgl.git" master:gh-pages
