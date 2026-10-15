#!/usr/bin/env bash
set -e

# Prints each entry to link into $HOME as ~/.<entry>: names in dotfiles/
# (linked whole), and individual files under config/ (dot-paths skipped).
# A FILES list at the repo root overrides this, in the same format.

dir=$PWD
files=()

manualfile="$dir/FILES"

if [[ -f $manualfile ]]; then
  IFS=$'\n' read -d '' -r -a files < $manualfile
else
  cd dotfiles
  files=(*)
  cd $dir
  while read -r file; do
    files+=("$file")
  done < <(find config -mindepth 1 -name '.*' -prune -o -type f -print)
fi

echo "${files[@]}"
