#!/usr/bin/env bash

set -eu

stack run > .test-out
echo 'Output:'
cat .test-out

echo
echo 'Diff:'
diff .test-out .correct-out -u && echo 'No difference' || true
rm .test-out
