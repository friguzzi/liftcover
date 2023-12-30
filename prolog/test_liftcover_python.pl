
:- module(test,
  [test/0,test_all/0]).
:- use_module(library(plunit)).

test:-
  test_all.

test_all:-
  tests(A),
  run_tests(A).

tests([carc,muta,nba,bongard]).


:- begin_tests(carc, []).
:-ensure_loaded(library(examples_lift/carc)).
:-use_module(library(cplint_test/cplint_test)).

test(induce_par_carc):-
  set_lift(verbosity,0),
  set_lift(parameter_learning,em_python),
  set_lift(iter,100),
  writeln(qui),
  induce_par_lift([train],P),
  writeln(qua),
  test_lift(P,[test],LL,AUCROC,_ROC,AUCPR,_PR),
  writeln(quo),
  writeln('Result:'),
  writeln(P),
  atomic_list_concat(['\nLL=',LL,'\nAUCROC=',AUCROC,'\nAUCPR=',AUCPR,'\n'],St),
  writeln(St),
  atomic_list_concat(['Expected:\n',
  '\nLL =', -26.41604269110072,
  '\nAUCROC =',0.6710526315789473,
  '\nAUCPR =', 0.5989337661969046],St1),
  writeln(St1).
  
test(induce_par_gd_carc):-
  set_lift(verbosity,0),
  set_lift(parameter_learning,gd_python),
  set_lift(eta,0.01),
  set_lift(iter,100),
  induce_par_lift([train],P),test_lift(P,[test],LL,AUCROC,_ROC,AUCPR,_PR),
  writeln('Result:'),
  writeln(P),
  atomic_list_concat(['\nLL=',LL,'\nAUCROC=',AUCROC,'\nAUCPR=',AUCPR,'\n'],St),
  writeln(St),
  atomic_list_concat(['Expected:\n',
  '\nLL =', -26.433804567011133,
  '\nAUCROC =',0.6710526315789473,
  '\nAUCPR =', 0.5989337661969046],St1),
  writeln(St1).

:- end_tests(carc).


:- begin_tests(muta, []).
:-ensure_loaded(library(examples_lift/muta)).
:-use_module(library(cplint_test/cplint_test)).


test(induce_par_muta):-
  set_lift(verbosity,0),
  set_lift(parameter_learning,em_python),
  induce_par_lift([1,2,3,4,5,6,7,8,9],P),test_lift(P,[10],LL,AUCROC,_ROC,AUCPR,_PR),
  writeln('Result:'),
  writeln(P),
  atomic_list_concat(['\nLL=',LL,'\nAUCROC=',AUCROC,'\nAUCPR=',AUCPR,'\n'],St),
  writeln(St),
  atomic_list_concat(['Expected:\n',
  '\nLL =',-50.3968461506984,
  '\nAUCROC =',0.4924242424242425,
  '\nAUCPR =', 0.7123209210586535],St1),
  writeln(St1).

test(induce_par_gd_muta):-
  set_lift(verbosity,0),
  set_lift(parameter_learning,gd_python),
  set_lift(parameter_update,adam),
  set_lift(eta,0.01),
  induce_par_lift([1,2,3,4,5,6,7,8,9],P),test_lift(P,[10],LL,AUCROC,_ROC,AUCPR,_PR),
  writeln('Result:'),
  writeln(P),
  atomic_list_concat(['\nLL=',LL,'\nAUCROC=',AUCROC,'\nAUCPR=',AUCPR,'\n'],St),
  writeln(St),
  atomic_list_concat(['Expected:\n',
  '\nLL =',-48.955482123491436,
  '\nAUCROC =',0.6287878787878788,
  '\nAUCPR =', 0.8184241612528028],St1),
  writeln(St1).


:- end_tests(muta).


:- begin_tests(nba, []).
:-ensure_loaded(library(examples_lift/nba)).
:-use_module(library(cplint_test/cplint_test)).

test(induce_nba_l1):-
  set_lift(verbosity,0),
  set_lift(regularization,l1),
  induce_lift([f1,f2,f3,f4],P),test_lift(P,[f5],LL,AUCROC,_ROC,AUCPR,_PR),
  writeln('Result:'),
  writeln(P),
  atomic_list_concat(['\nLL=',LL,'\nAUCROC=',AUCROC,'\nAUCPR=',AUCPR,'\n'],St),
  writeln(St),
  atomic_list_concat(['Expected:\n', 
  '\nLL =', -49.7938188339248,
  '\nAUCROC =',0.625,
  '\nAUCPR =', 0.8189102564102563],St1),
  writeln(St1).

test(induce_nba_l2):-
  set_lift(verbosity,0),
  set_lift(regularization,l2),
  induce_lift([f1,f2,f3,f4],P),test_lift(P,[f5],LL,AUCROC,_ROC,AUCPR,_PR),
  writeln('Result:'),
  writeln(P),
  atomic_list_concat(['\nLL=',LL,'\nAUCROC=',AUCROC,'\nAUCPR=',AUCPR,'\n'],St),
  writeln(St),
  atomic_list_concat(['Expected:\n', 
  '\nLL =', -43.57330116702434,
  '\nAUCROC =',0.625,
  '\nAUCPR =', 0.8189102564102563],St1),
  writeln(St1).

test(induce_nba_bayesian):-
  set_lift(verbosity,0),
  set_lift(regularization,bayesian),
  induce_lift([f1,f2,f3,f4],P),test_lift(P,[f5],LL,AUCROC,_ROC,AUCPR,_PR),
  writeln('Result:'),
  writeln(P),
  atomic_list_concat(['\nLL=',LL,'\nAUCROC=',AUCROC,'\nAUCPR=',AUCPR,'\n'],St),
  writeln(St),
  atomic_list_concat(['Expected:\n', 
  '\nLL =', -47.070639401222,
  '\nAUCROC =',0.625,
  '\nAUCPR =', 0.8189102564102563],St1),
  writeln(St1).

:- end_tests(nba).


:- begin_tests(bongard, []).
:-ensure_loaded(library(examples_lift/bongard)).
:-use_module(library(cplint_test/cplint_test)).

test(in_bongard):-
  in(P),test_lift(P,[test],LL,AUCROC,_ROC,AUCPR,_PR),
writeln('Result:'),
writeln(P),
atomic_list_concat(['\nLL=',LL,'\nAUCROC=',AUCROC,'\nAUCPR=',AUCPR,'\n'],St),
writeln(St),
atomic_list_concat(['Expected:\n', 
'\nLL =', -250.33054186204907,
'\nAUCROC =',0.7626436781609195,
'\nAUCPR =', 0.5615465293941269],St1),
writeln(St1).

test(induce_par_bongard):-
  set_lift(verbosity,1),
  induce_par_lift([train],P),test_lift(P,[test],LL,AUCROC,_ROC,AUCPR,_PR),
  writeln('Result:'),
  writeln(P),
  atomic_list_concat(['\nLL=',LL,'\nAUCROC=',AUCROC,'\nAUCPR=',AUCPR,'\n'],St),
  writeln(St),
  atomic_list_concat(['Expected:\n',
  '\nLL =',-219.68079672612654,
  '\nAUCROC =',0.7501277139208173,
  '\nAUCPR =', 0.5997247517658881],St1),
  writeln(St1).

test(induce_par_gd_bongard):-
  set_lift(verbosity,4),
  set_lift(parameter_learning,gd),
  set_lift(eta,0.01),
  induce_par_lift([train],P),test_lift(P,[test],LL,AUCROC,_ROC,AUCPR,_PR),
  writeln('Result:'),
  writeln(P),
  atomic_list_concat(['\nLL=',LL,'\nAUCROC=',AUCROC,'\nAUCPR=',AUCPR,'\n'],St),
  writeln(St),
  atomic_list_concat(['Expected:\n',
  '\nLL =',-218.22925642272494,
  '\nAUCROC =',0.7485951468710089,
  '\nAUCPR =', 0.5979409655734421],St1),
  writeln(St1).

test(induce_gd_bongard):-
  set_lift(verbosity,1),
  set_lift(parameter_learning,gd),
  set_lift(regularization, no),
  set_lift(eta,0.01),
  set_lift(gamma,0.1),
  induce_lift([train],P),test_lift(P,[test],LL,AUCROC,_ROC,AUCPR,_PR),
  writeln('Result:'),
  writeln(P),
  atomic_list_concat(['\nLL=',LL,'\nAUCROC=',AUCROC,'\nAUCPR=',AUCPR,'\n'],St),
  writeln(St),
  atomic_list_concat(['Expected:\n',
  '\nLL =',-88.11894369013643,
  '\nAUCROC =',0.8199872286079184,
  '\nAUCPR =',0.6676162174173899],St1),
  writeln(St1).

test(induce_gd_l1_bongard):-
  set_lift(verbosity,1),
  set_lift(parameter_learning,gd),
  set_lift(regularization, l1),
  set_lift(eta,0.01),
  set_lift(gamma,1),
  induce_lift([train],P),test_lift(P,[test],LL,AUCROC,_ROC,AUCPR,_PR),
  writeln('Result:'),
  writeln(P),
  atomic_list_concat(['\nLL=',LL,'\nAUCROC=',AUCROC,'\nAUCPR=',AUCPR,'\n'],St),
  writeln(St),
  atomic_list_concat(['Expected:\n',
  '\nLL =',-86.45340693767905,
  '\nAUCROC =',0.8247126436781609,
  '\nAUCPR =', 0.6620667885942804],St1),
  writeln(St1).

test(induce_gd_l2_bongard):-
  set_lift(verbosity,1),
  set_lift(parameter_learning,gd),
  set_lift(regularization, l2),
  set_lift(eta,0.01),
  set_lift(gamma,1),
  induce_lift([train],P),test_lift(P,[test],LL,AUCROC,_ROC,AUCPR,_PR),
  writeln('Result:'),
  writeln(P),
  atomic_list_concat(['\nLL=',LL,'\nAUCROC=',AUCROC,'\nAUCPR=',AUCPR,'\n'],St),
  writeln(St),
  atomic_list_concat(['Expected:\n',
  '\nLL =',-87.36727076138638,
  '\nAUCROC =',0.8188378033205619,
  '\nAUCPR =', 0.6550280976471973],St1),
  writeln(St1).


test(induce_par_gd_adam_bongard):-
  set_lift(verbosity,1),
  set_lift(parameter_learning,gd),
  set_lift(parameter_update, adam),
  set_lift(gamma,1),
  set_lift(iter,20),
  set_lift(adam_params,[0.2,0.8,0.9,1e-8]),
  induce_par_lift([train],P),test_lift(P,[test],LL,AUCROC,_ROC,AUCPR,_PR),
  writeln('Result:'),
  writeln(P),
  atomic_list_concat(['\nLL=',LL,'\nAUCROC=',AUCROC,'\nAUCPR=',AUCPR,'\n'],St),
  writeln(St),
  atomic_list_concat(['Expected:\n',
  '\nLL =',-218.57571674633985,
  '\nAUCROC =',0.7485951468710089,
  '\nAUCPR =', 0.5979409655734421],St1),
  writeln(St1).


:- end_tests(bongard).
