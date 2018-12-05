/**
 * Module      : Advent of code, december 01, 2018
 * Copyright   : Jonas Nockert / @lemonad
 */
:- module(december01, [solve/0, solve_part_one/2, solve_part_two/2,
                       read_input/2, read_input_from_file/2]).
% Note that the below libraries are autoloaded.
% :- use_module(library(pio)).
% :- use_module(library(dcg/basics), except([digit/3])).
% :- use_module(library(assoc)).

solve :-
  read_input_from_file(`input/december01.input`, Ns) , ! ,
  solve_part_one(Ns, One) ,
  write('Resulting frequency: ') , write(One) , nl ,
  solve_part_two(Ns, Two) ,
  write('Frequency reached twice: ') , write(Two) , nl .

solve_part_one(Ns, Res) :-
  sum_list(Ns, S) , ! ,
  Res = S .

solve_part_two(Ns, Res) :-
  empty_assoc(A0) ,
  S = 0 ,
  twice(Ns, Ns, S, A0, Freq) , ! ,
  Res = Freq .

read_input_from_file(Filename, Ns) :-
  phrase_from_file(nats(Ns), Filename) .

read_input(Str, Ns) :-
  phrase(nats(Ns), Str) .

twice([], Ns, S, A0, Freq) :- twice(Ns, Ns, S, A0, Freq) .
twice([L|Ls], Ns, S, A0, Freq) :-
  get_assoc(S, A0, _) , Freq = S ;
  put_assoc(S, A0, true, A) ,
  S1 is S + L ,
  twice(Ls, Ns, S1, A, Freq) .

nats([]) --> [] .
nats([N|Ns]) --> nat(N) , newline , nats(Ns) .

newline --> `\n`.
space --> ` `.
plus --> `+`.
minus --> `-`.

digit(0) --> `0`. digit(1) --> `1`. digit(2) --> `2`.
digit(3) --> `3`. digit(4) --> `4`. digit(5) --> `5`.
digit(6) --> `6`. digit(7) --> `7`. digit(8) --> `8`.
digit(9) --> `9`.

nat(N)     --> minus , digit(D) , nat(-D, N, -1) .
nat(N)     --> plus , digit(D) , nat(D, N, 1) .
nat(N,N,_) --> [] .
nat(A,N,S) --> digit(D) , { A1 is A * 10 + S * D } , nat(A1, N, S) .
