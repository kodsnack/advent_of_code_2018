#!/usr/bin/env bash
[[ -z "$1" ]] && { echo "Parameter 1 (YEAR) is empty" ; exit 1; }
[[ -z "$2" ]] && { echo "Parameter 1 (DAY) is empty" ; exit 1; }

YEAR="${1}"
DAY="${2}"
DAY_NO_ZEROS="$(echo $DAY | sed 's/^0*//')"
OUTPUT="december${DAY}.py"
TEST_OUTPUT="tests/test_december${DAY}.py"
PUZZLE_FILE="input/december${DAY}.input"

tmux rename-window "Advent of Code"
tmux split-window -h
tmux split-window -h
tmux select-layout even-horizontal
tmux select-pane -t 1
tmux split-window -v
tmux select-pane -t 3
tmux send-keys -t 4 "pyenv activate advent; python" C-m \;
tmux send-keys -t 3 "vim ${OUTPUT} ${TEST_OUTPUT}" C-m \;
tmux send-keys -t 1 "pyenv activate advent; ./watch.sh ${YEAR} ${DAY}" C-m \;
tmux send-keys -t 2 "pyenv activate advent; ./watch_test.sh ${YEAR} ${DAY}" C-m \;
