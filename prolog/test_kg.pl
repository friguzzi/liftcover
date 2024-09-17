
:- module(test,
  [test/0,test_all/0]).
:- use_module(library(plunit)).

test:-
  test_all.

test_all:-
  tests(A),
  run_tests(A).

tests([nations]).


:- begin_tests(nations, []).
:-ensure_loaded(library(examples_lift/nations)).
:-use_module(library(cplint_test/cplint_test)).



test(induce_nations_torch):-
  set_lift(parameter_learning,em_torch),
  set_lift(verbosity,0),
  out(R),
  induce_par_kg(R,R1),
  writeln('Result:'),
  writeln(R1).

test(induce_nations):-
  set_lift(parameter_learning,em_python),
  set_lift(verbosity,0),
  out(R),
  induce_par_kg(R,R1),
  writeln('Result:'),
  writeln(R1).


test(induce_nations_pos):-
  set_lift(parameter_learning,em_python),
  set_lift(verbosity,0),
  set_lift(regularization,l2),
  out(R),
  induce_par_pos_kg(R,R1),
  writeln('Result:'),
  writeln(R1).

:- end_tests(nations).

