#!/usr/bin/env bash
[[ -z "$1" ]] && { echo "Parameter 1 (YEAR) is empty" ; exit 1; }
[[ -z "$2" ]] && { echo "Parameter 1 (DAY) is empty" ; exit 1; }

YEAR="${1}"
DAY="${2}"
DAY_NO_ZEROS="$(echo $DAY | sed 's/^0*//')"
INPUT="solution_template.py"
TEST_INPUT="solution_test_template.py"
OUTPUT="december${DAY}.py"
TEST_OUTPUT="tests/test_december${DAY}.py"
PUZZLE_URL="https://adventofcode.com/${YEAR}/day/${DAY_NO_ZEROS}/input"
PUZZLE_FILE="input/december${DAY}.input"

# Avoid overwriting solutions.
if [ -f $OUTPUT ]; then
    echo "Solution already exists!"
    exit 1
fi

# Avoid creating solution if session cookie is not set.
if [ -z "$AOC_SESSION_COOKIE" ]; then
    echo "Variable AOC_SESSION_COOKIE not set!"
    exit 1
fi

# Fetch input file.
curl --fail "${PUZZLE_URL}" -H "cookie: session=${AOC_SESSION_COOKIE}" -o "${PUZZLE_FILE}" 2>/dev/null
if [ $? -ne 0 ]; then
  echo "Variable AOC_SESSION_COOKIE expired!"
  exit 1
fi

mkdir -p "$(dirname ${TEST_OUTPUT})"
cp "${INPUT}" "${OUTPUT}"
sed -i '' "s/YearYYYY/${YEAR}/g" "${OUTPUT}"
sed -i '' "s/DayDD/${DAY}/g" "${OUTPUT}"
sed -i '' "s@InputFILENAME@${PUZZLE_FILE}@g" "${OUTPUT}"
sed -i '' -e "/InputCONTENT/r ${PUZZLE_FILE}" -e "/InputCONTENT/d" "${OUTPUT}"

cp "${TEST_INPUT}" "${TEST_OUTPUT}"
sed -i '' "s/YearYYYY/${YEAR}/g" "${TEST_OUTPUT}"
sed -i '' "s/DayDD/${DAY}/g" "${TEST_OUTPUT}"
sed -i '' "s@InputFILENAME@${PUZZLE_FILE}@g" "${TEST_OUTPUT}"

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
