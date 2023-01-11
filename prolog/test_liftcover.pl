
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

par([carc_par,muta_par]).

stru([bupa,mondial,nba]).

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

:- begin_tests(muta_par, []).
:-ensure_loaded(library(examples/muta)).
:-use_module(library(cplint_test/cplint_test)).

test(in_muta):-
  in(P),test(P,[10],LL,AUCROC,_ROC,AUCPR,_PR),
writeln('Result:'),
writeln(P),
atomic_list_concat(['\nLL=',LL,'\nAUCROC=',AUCROC,'\nAUCPR=',AUCPR,'\n'],St),
writeln(St),
atomic_list_concat(['Expected:\n', 
'\nLL =', -57.935435521442045,
'\nAUCROC =',0.46212121212121215,
'\nAUCPR =', 0.6901758247756261],St1),
writeln(St1).

test(induce_par_muta):-
  set_lift(verbosity,0),
  induce_lift_par([1,2,3,4,5,6,7,8,9],P),test(P,[10],LL,AUCROC,_ROC,AUCPR,_PR),
  writeln('Result:'),
  writeln(P),
  atomic_list_concat(['\nLL=',LL,'\nAUCROC=',AUCROC,'\nAUCPR=',AUCPR,'\n'],St),
  writeln(St),
  atomic_list_concat(['Expected:\n',
  '\nLL =',-62.26891635625195,
  '\nAUCROC =',0.6287878787878788,
  '\nAUCPR =', 0.8184241612528028],St1),
  writeln(St1).
  

:- end_tests(muta_par).


:- begin_tests(nba, []).
:-ensure_loaded(library(examples/nba)).
:-use_module(library(cplint_test/cplint_test)).

test(induce_nba):-
  set_lift(verbosity,0),
  set_lift(regularization,no),
  induce_lift([f1,f2,f3,f4],P),test(P,[f5],LL,AUCROC,_ROC,AUCPR,_PR),
  writeln('Result:'),
  writeln(P),
  atomic_list_concat(['\nLL=',LL,'\nAUCROC=',AUCROC,'\nAUCPR=',AUCPR,'\n'],St),
  writeln(St),
  atomic_list_concat(['Expected:\n', 
  '\nLL =', -69.80130553310904,
  '\nAUCROC =',0.625,
  '\nAUCPR =', 0.8189102564102563],St1),
  writeln(St1).

test(induce_nba_l1):-
  set_lift(verbosity,0),
  set_lift(regularization,l1),
  induce_lift([f1,f2,f3,f4],P),test(P,[f5],LL,AUCROC,_ROC,AUCPR,_PR),
  writeln('Result:'),
  writeln(P),
  atomic_list_concat(['\nLL=',LL,'\nAUCROC=',AUCROC,'\nAUCPR=',AUCPR,'\n'],St),
  writeln(St),
  atomic_list_concat(['Expected:\n', 
  '\nLL =', -69.80130553310904,
  '\nAUCROC =',0.625,
  '\nAUCPR =', 0.8189102564102563],St1),
  writeln(St1).

test(induce_nba_l2):-
  set_lift(verbosity,0),
  set_lift(regularization,l2),
  induce_lift([f1,f2,f3,f4],P),test(P,[f5],LL,AUCROC,_ROC,AUCPR,_PR),
  writeln('Result:'),
  writeln(P),
  atomic_list_concat(['\nLL=',LL,'\nAUCROC=',AUCROC,'\nAUCPR=',AUCPR,'\n'],St),
  writeln(St),
  atomic_list_concat(['Expected:\n', 
  '\nLL =', -69.80130553310904,
  '\nAUCROC =',0.625,
  '\nAUCPR =', 0.8189102564102563],St1),
  writeln(St1).

test(induce_nba_bayesian):-
  set_lift(verbosity,0),
  set_lift(regularization,bayesian),
  induce_lift([f1,f2,f3,f4],P),test(P,[f5],LL,AUCROC,_ROC,AUCPR,_PR),
  writeln('Result:'),
  writeln(P),
  atomic_list_concat(['\nLL=',LL,'\nAUCROC=',AUCROC,'\nAUCPR=',AUCPR,'\n'],St),
  writeln(St),
  atomic_list_concat(['Expected:\n', 
  '\nLL =', -69.80130553310904,
  '\nAUCROC =',0.625,
  '\nAUCPR =', 0.8189102564102563],St1),
  writeln(St1).

:- end_tests(nba).