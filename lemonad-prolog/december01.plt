:- begin_tests(december01).
:- use_module(library(plunit)).
:- consult(december01).

test(examplePartOne, [nondet]) :-
  read_input(`+1\n-2\n+3\n+1\n`, Ns) ,
  solve_part_one(Ns, One) ,
  assertion(One == 3) .

test(otherExamplesPartOne1, [nondet]) :-
  read_input(`+1\n+1\n+1\n`, Ns) ,
  solve_part_one(Ns, One) ,
  assertion(One == 3) .

test(otherExamplesPartOne2, [nondet]) :-
  read_input(`+1\n+1\n-2\n`, Ns) ,
  solve_part_one(Ns, One) ,
  assertion(One == 0) .

test(otherExamplesPartOne3, [nondet]) :-
  read_input(`-1\n-2\n-3\n`, Ns) ,
  solve_part_one(Ns, One) ,
  assertion(One == -6) .

test(otherExamplesPartOne3, [nondet]) :-
  read_input(`-1\n-2\n-3\n`, Ns) ,
  solve_part_one(Ns, One) ,
  assertion(One == -6) .

test(examplePartTwo, [nondet]) :-
  read_input(`+1\n-2\n+3\n+1\n`, Ns) ,
  solve_part_two(Ns, Two) ,
  assertion(Two == 2) .

test(otherExamplesPartTwo1, [nondet]) :-
  read_input(`+1\n-1\n`, Ns) ,
  solve_part_two(Ns, Two) ,
  assertion(Two == 0) .

test(otherExamplesPartTwo2, [nondet]) :-
  read_input(`+1\n+1\n-2\n`, Ns) ,
  solve_part_two(Ns, Two) ,
  assertion(Two == 0) .

test(otherExamplesPartTwo3, [nondet]) :-
  read_input(`+3\n+3\n+4\n-2\n-4\n`, Ns) ,
  solve_part_two(Ns, Two) ,
  assertion(Two == 10) .

test(otherExamplesPartTwo4, [nondet]) :-
  read_input(`-6\n+3\n+8\n+5\n-6\n`, Ns) ,
  solve_part_two(Ns, Two) ,
  assertion(Two == 5) .

test(otherExamplesPartTwo5, [nondet]) :-
  read_input(`+7\n+7\n-2\n-7\n-4\n`, Ns) ,
  solve_part_two(Ns, Two) ,
  assertion(Two == 14) .

test(partOneAnswerIsCorrect, [nondet]) :-
  read_input_from_file(`input/december01.input`, Ns) ,
  solve_part_one(Ns, One) ,
  assertion(One == 425) .

test(partTwoAnswerIsCorrect, [nondet]) :-
  read_input_from_file(`input/december01.input`, Ns) ,
  solve_part_two(Ns, Two) ,
  assertion(Two == 57538) .

:- end_tests(december01).
