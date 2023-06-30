#!/bin/sh

# TODO? synchronize with lock (in case of multiple parallel instances)
# TODO? rewrite in Haskell with easy tests
# TODO other modes: select area/window

screenshotDir="$HOME/archive/Media/Pictures/Screens"

function tryOutputPath {
  local suffix="${1:+-$1}"
  local candidate=$(date "+$screenshotDir/%Y-%m-%d-%H:%M:%S$suffix.png")
  if test -f $candidate ; then
    return 1
  else
    echo $candidate
  fi
}

function getOutputPath {
  local suffix="$1"
  local i=0
  mkdir -p "$screenshotDir"
  tryOutputPath "$suffix" || until tryOutputPath "$suffix-$i" ; do
                               i=$((i+1))
                               if test $i -gt 100 ; then
                                 exit 1
                               fi
                             done
}

function main {
  case "${1:---whole}" in
    --print-dir)
      echo "$screenshotDir"
      ;;
    --whole)
      grim $(getOutputPath)
      ;;
    *)
      echo unknown argument
      exit 1
      ;;
  esac
}

main "$@"
