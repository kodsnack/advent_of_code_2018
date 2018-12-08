#!/usr/bin/env bash

stack run > .correct-out

echo 'Output:'
cat .correct-out
