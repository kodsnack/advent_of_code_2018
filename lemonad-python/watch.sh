#!/usr/bin/env bash
[[ -z "$1" ]] && { echo "Parameter 1 (YEAR) is empty" ; exit 1; }
[[ -z "$2" ]] && { echo "Parameter 1 (DAY) is empty" ; exit 1; }

YEAR="${1}"
DAY="${2}"
INPUT="december${DAY}.py"
WATCH_MORE="common/puzzlesolver.py"

if [ ! -f $INPUT ]; then
    echo "'${INPUT}' does not exist!"
    exit 0
fi

while true; do
  fswatch ${INPUT} ${WATCH_MORE} -o | xargs -n1 -I{} ./run.sh "${1}" "${2}"
done;
