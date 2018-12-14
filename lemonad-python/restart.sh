#!/usr/bin/env bash
[[ -z "$1" ]] && { echo "Parameter 1 (YEAR) is empty" ; exit 1; }
[[ -z "$2" ]] && { echo "Parameter 1 (DAY) is empty" ; exit 1; }

YEAR="${1}"
DAY="${2}"
DAY_NO_ZEROS="$(echo $DAY | sed 's/^0*//')"
OUTPUT="december${DAY}.py"
TEST_OUTPUT="tests/test_december${DAY}.py"
PUZZLE_FILE="input/december${DAY}.input"

tmux new-session -s advent -n editor -d
tmux rename-window -t advent "Advent of Code"
tmux split-window -h -t advent
tmux split-window -h -t advent
tmux select-layout -t advent even-horizontal
tmux split-window -v -t advent:1.1
tmux send-keys -t advent:1.4 "pyenv activate advent; python" C-m
tmux send-keys -t advent:1.3 "vim ${OUTPUT} ${TEST_OUTPUT}" C-m
tmux send-keys -t advent:1.1 "pyenv activate advent; ./watch.sh ${YEAR} ${DAY}" C-m
tmux send-keys -t advent:1.2 "pyenv activate advent; ./watch_test.sh ${YEAR} ${DAY}" C-m
tmux select-pane -t advent:1.3
tmux attach -t advent
