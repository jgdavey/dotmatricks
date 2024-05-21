#!/usr/bin/env bash
set -e

dir=$PWD
files=()

manualfile="$dir/FILES"

if [ -f $manualfile ]; then
  IFS=$'\n' read -d '' -r -a files < $manualfile
else
  cd dotfiles
  files=(*)
  cd $dir
fi

echo "${files[@]}"
