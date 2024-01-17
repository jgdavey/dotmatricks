#!/usr/bin/env bash

set -e

SUB_REPO="${SUB_REPO:-git@github.com:jgdavey/sub.git}"

for b in $(find bin -type l -prune -exec basename {} \;) ; do
  export NAME="$b"
done

echo "Regenerating your '$NAME' sub!"

if [ -d "./_update" ]; then
  echo "Removing staging folder" 2>&1
  rm -rf _update
fi

git clone $SUB_REPO _update

cd _update

./prepare.sh $NAME >/dev/null

cd ..

rm -rf _update/.git

rsync -au --exclude="LICENSE" _update/ .

rm -rf _update
