
:- module(test,
  [test/0,test_all/0,test_par/0,test_stru/0]).
:- use_module(library(plunit)).

test:-
  test_all.

test_all:-
  par(P),
  stru(S),
  append(P,S,A),
  run_tests(A).

test_par:-
  par(P),
  run_tests(P).

test_stru:-
  stru(S),
  run_tests(S).

par([carc_par]).

stru([bupa,mondial]).

:- begin_tests(bupa, []).
:-ensure_loaded(library(examples/bupa)).
:-use_module(library(cplint_test/cplint_test)).

test(induce_bupa):-
  set_lift(verbosity,0),
  induce_lift([f1,f2,f3,f4],P),test(P,[f5],LL,AUCROC,_ROC,AUCPR,_PR),
  writeln('Result:'),
  writeln(P),
  atomic_list_concat(['\nLL=',LL,'\nAUCROC=',AUCROC,'\nAUCPR=',AUCPR,'\n'],St),
  writeln(St),
  atomic_list_concat(['Expected:\n',
  '\nLL =', -635.5134856663562,
  '\nAUCROC =',1.0,
  '\nAUCPR =', 1.0],St1),
  writeln(St1).

:- end_tests(bupa).
/*
:- begin_tests(carc, []).
:-ensure_loaded(library(examples/carc)).
:-use_module(library(cplint_test/cplint_test)).

test(induce_carc):-
  set_lift(verbosity,0),
  induce_lift([train],P),test(P,[test],LL,AUCROC,_ROC,AUCPR,_PR),
  writeln('Result:'),
  writeln(P),
  atomic_list_concat(['\nLL=',LL,'\nAUCROC=',AUCROC,'\nAUCPR=',AUCPR,'\n'],St),
  writeln(St),
  atomic_list_concat(['Expected:\n',
  '\nLL =', -45.378075874609856,
  '\nAUCROC =',0.7157894736842104,
  '\nAUCPR =', 0.6848640204158833],St1),
  writeln(St1).

:- end_tests(carc).
*/
:- begin_tests(carc_par, []).
:-ensure_loaded(library(examples/carc)).
:-use_module(library(cplint_test/cplint_test)).

test(induce_par_carc):-
  set_lift(verbosity,0),
  induce_lift_par([train],P),test(P,[test],LL,AUCROC,_ROC,AUCPR,_PR),
  writeln('Result:'),
  writeln(P),
  atomic_list_concat(['\nLL=',LL,'\nAUCROC=',AUCROC,'\nAUCPR=',AUCPR,'\n'],St),
  writeln(St),
  atomic_list_concat(['Expected:\n',
  '\nLL =', -38.83800917968338,
  '\nAUCROC =',0.6710526315789473,
  '\nAUCPR =', 0.5989337661969046],St1),
  writeln(St1).
  
  test(in_carc):-
    in(P),test(P,[all],LL,AUCROC,_ROC,AUCPR,_PR),
  writeln('Result:'),
  writeln(P),
  atomic_list_concat(['\nLL=',LL,'\nAUCROC=',AUCROC,'\nAUCPR=',AUCPR,'\n'],St),
  writeln(St),
  atomic_list_concat(['Expected:\n', 
  '\nLL =', -46.00213796704799,
  '\nAUCROC =',0.43950726692662173,
  '\nAUCPR =', 0.4962773673721876],St1),
  writeln(St1).

:- end_tests(carc_par).

:- begin_tests(mondial, []).
:-ensure_loaded(library(examples/mondial)).
:-use_module(library(cplint_test/cplint_test)).

test(induce_mondial):-
  set_lift(verbosity,0),
  induce_lift([f1,f2,f3,f4],P),test(P,[f5],LL,AUCROC,_ROC,AUCPR,_PR),
  writeln('Result:'),
  writeln(P),
  atomic_list_concat(['\nLL=',LL,'\nAUCROC=',AUCROC,'\nAUCPR=',AUCPR,'\n'],St),
  writeln(St),
  atomic_list_concat(['Expected:\n', 
  '\nLL =', -19.942390944070794,
  '\nAUCROC =',0.7250000000000001,
  '\nAUCPR =', 0.8568357523414214],St1),
  writeln(St1).

:- end_tests(mondial).

