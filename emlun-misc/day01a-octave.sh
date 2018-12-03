#!/bin/bash
(echo "disp(sum([" ; cat $(dirname "$0")/day01.in ; echo "]))") | octave
