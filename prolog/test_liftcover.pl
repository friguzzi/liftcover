
:- module(test,
  [test/0,test_all/0]).
:- use_module(library(plunit)).

test:-
  test_all.

test_all:-
  tests(A),
  run_tests(A).

tests([rank,carc,muta,bupa,mondial,nba,bongard]).

:- begin_tests(rank, []).
:-ensure_loaded(library(liftcover)).

test(rank1):-
  L=[8.0-spider,4.0-cat,4.0-dog,2.0-penguin,0.0-snake],
  rank(spider,L,1.0),
  rank(cat,L,2.5),
  rank(dog,L,2.5),
  rank(penguin,L,4.0),
  rank(snake,L,5.0).

test(rank2):-
  L=[8.0-spider,8.0-crab,4.0-cat,4.0-dog,4.0-horse,2.0-penguin,0.0-snake],
  rank(spider,L,1.5),
  rank(crab,L,1.5),
  rank(cat,L,4.0),
  rank(dog,L,4.0),
  rank(horse,L,4.0),
  rank(penguin,L,6.0),
  rank(snake,L,7.0).

test(rank3):-
  L=[8.0-spider,8.0-crab,4.0-cat,4.0-dog,4.0-horse,2.0-penguin,0.0-snake,0.0-snail],
  rank(snake,L,7.5),
  rank(snail,L,7.5),
  rank(cricket,L,+inf).

:- end_tests(rank).

:- begin_tests(bupa, []).
:-ensure_loaded(library(examples_lift/bupa)).
:-use_module(library(cplint_test/cplint_test)).

test(induce_bupa):-
  set_lift(verbosity,0),
  induce_lift([f1,f2,f3,f4],P),test_lift(P,[f5],LL,AUCROC,_ROC,AUCPR,_PR),
  writeln('Result:'),
  writeln(P),
  atomic_list_concat(['\nLL=',LL,'\nAUCROC=',AUCROC,'\nAUCPR=',AUCPR,'\n'],St),
  writeln(St),
  atomic_list_concat(['Expected:\n',
  '\nLL =', 0.0,
  '\nAUCROC =',1.0,
  '\nAUCPR =', 1.0],St1),
  writeln(St1).

:- end_tests(bupa).

:- begin_tests(carc, []).
:-ensure_loaded(library(examples_lift/carc)).
:-use_module(library(cplint_test/cplint_test)).

test(in_carc):-
  in(P),test_lift(P,[all],LL,AUCROC,_ROC,AUCPR,_PR),
writeln('Result:'),
writeln(P),
atomic_list_concat(['\nLL=',LL,'\nAUCROC=',AUCROC,'\nAUCPR=',AUCPR,'\n'],St),
writeln(St),
atomic_list_concat(['Expected:\n', 
'\nLL =', -2501.635205175788,
'\nAUCROC =',0.43950726692662173,
'\nAUCPR =', 0.4962773673721876],St1),
writeln(St1).

test(par_test_carc):-
  set_lift(threads,2),
  in(P),test_lift(P,[all],LL,AUCROC,_ROC,AUCPR,_PR),
writeln('Result:'),
writeln(P),
atomic_list_concat(['\nLL=',LL,'\nAUCROC=',AUCROC,'\nAUCPR=',AUCPR,'\n'],St),
writeln(St),
atomic_list_concat(['Expected:\n', 
'\nLL =', -2501.635205175788,
'\nAUCROC =',0.43950726692662173,
'\nAUCPR =', 0.4962773673721876],St1),
writeln(St1).

test(sort_carc):-
  in(P),sort_rules(P,P1),
  subset(P,P1),
  subset(P1,P).

test(filter_carc):-
  in(P),filter_rules(P,P1),
  subset(P,P1).

test(filter_rules_carc):-
  filter_rules([rule(_Name,[_H:0.0,_],_B,_L)],[]).

test(remove_zero_rule):-
  remove_zero([rule(_Name,[_H:0.0,_],_B,_L)],[]).

test(remove_zero):-
  remove_zero([(_H:0.0 :- _)],[]).

test(induce_par_carc):-
  set_lift(verbosity,0),
  induce_par_lift([train],P),test_lift(P,[test],LL,AUCROC,_ROC,AUCPR,_PR),
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
  set_lift(parameter_learning,gd),
  set_lift(eta,0.01),
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

:- begin_tests(mondial, []).
:-ensure_loaded(library(examples_lift/mondial)).
:-use_module(library(cplint_test/cplint_test)).




test(ranked_answers_mondial):-
  ranked_answers(christian_religion(f1,C),C,E),
  E=[-(0.409107,'XMAS'),-(0.409107,'WS'),-(0.409107,'WAFU'),-(0.409107,'VU'),-(0.409107,'TUV'),-(0.409107,'TO'),-(0.409107,'SLB'),-(0.409107,'RI'),-(0.409107,'REUN'),-(0.409107,'PNG'),-(0.409107,'PITC'),-(0.409107,'PAL'),-(0.409107,'NZ'),-(0.409107,'NORF'),-(0.409107,'NMIS'),-(0.409107,'NIUE'),-(0.409107,'NCA'),-(0.409107,'NAU'),-(0.409107,'MH'),-(0.409107,'KIR'),-(0.409107,'HELX'),-(0.409107,'GUAM'),-(0.409107,'FSM'),-(0.409107,'FPOL'),-(0.409107,'FJI'),-(0.409107,'COOK'),-(0.409107,'COCO'),-(0.409107,'AUS'),-(0.409107,'AMSA')].

test(induce_mondial):-
  set_lift(verbosity,0),
  induce_lift([f1,f2,f3,f4],P),test_lift(P,[f5],LL,AUCROC,_ROC,AUCPR,_PR),
  writeln('Result:'),
  writeln(P),
  atomic_list_concat(['\nLL=',LL,'\nAUCROC=',AUCROC,'\nAUCPR=',AUCPR,'\n'],St),
  writeln(St),
  atomic_list_concat(['Expected:\n', 
  '\nLL =', -27.553592935066412,
  '\nAUCROC =',0.7250000000000001,
  '\nAUCPR =', 0.8568357523414214],St1),
  writeln(St1).

test(induce_conc_mondial):-
  set_lift(verbosity,0),
  set_lift(threads,2),
  induce_lift([f1,f2,f3,f4],P),test_lift(P,[f5],LL,AUCROC,_ROC,AUCPR,_PR),
  writeln('Result:'),
  writeln(P),
  atomic_list_concat(['\nLL=',LL,'\nAUCROC=',AUCROC,'\nAUCPR=',AUCPR,'\n'],St),
  writeln(St),
  atomic_list_concat(['Expected:\n', 
  '\nLL =', -27.553592935066412,
  '\nAUCROC =',0.7250000000000001,
  '\nAUCPR =', 0.8568357523414214],St1),
  writeln(St1).

test(prob_lift_sv_mondial):-
  set_lift(single_var,true),
  prob_lift(christian_religion(f1,C),P),
  C='AMSA',P=:=0.409107,!.

:- end_tests(mondial).

:- begin_tests(muta, []).
:-ensure_loaded(library(examples_lift/muta)).
:-use_module(library(cplint_test/cplint_test)).

test(in_muta):-
  in(P),test_lift(P,[10],LL,AUCROC,_ROC,AUCPR,_PR),
writeln('Result:'),
writeln(P),
atomic_list_concat(['\nLL=',LL,'\nAUCROC=',AUCROC,'\nAUCPR=',AUCPR,'\n'],St),
writeln(St),
atomic_list_concat(['Expected:\n', 
'\nLL =', -67.3970487803363,
'\nAUCROC =',0.46212121212121215,
'\nAUCPR =', 0.6901758247756261],St1),
writeln(St1).

test(induce_par_muta):-
  set_lift(verbosity,0),
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
  set_lift(parameter_learning,gd),
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

test(induce_nba):-
  set_lift(verbosity,0),
  set_lift(regularization,no),
  induce_lift([f1,f2,f3,f4],P),test_lift(P,[f5],LL,AUCROC,_ROC,AUCPR,_PR),
  writeln('Result:'),
  writeln(P),
  atomic_list_concat(['\nLL=',LL,'\nAUCROC=',AUCROC,'\nAUCPR=',AUCPR,'\n'],St),
  writeln(St),
  atomic_list_concat(['Expected:\n', 
  '\nLL =', -42.17028441718049,
  '\nAUCROC =',0.625,
  '\nAUCPR =', 0.8189102564102563],St1),
  writeln(St1).

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


test(explain_bongard):-
  explain_lift(pos(2),Expl),
  Expl=[0.197575-(circle(2, o3), inside(2, o2, o3)), 
  0.000448807-(triangle(2, o5), circle(2, o3)), 
  0.000448807-(triangle(2, o2), circle(2, o3)), 
  0.000448807-(triangle(2, o1), circle(2, o3))].

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


test(induce_bongard):-
  set_lift(verbosity,1),
  induce_lift([train],P),test_lift(P,[test],LL,AUCROC,_ROC,AUCPR,_PR),
  writeln('Result:'),
  writeln(P),
  atomic_list_concat(['\nLL=',LL,'\nAUCROC=',AUCROC,'\nAUCPR=',AUCPR,'\n'],St),
  writeln(St),
  atomic_list_concat(['Expected:\n',
  '\nLL =',-102.73759622946764,
  '\nAUCROC =',0.7045977011494252,
  '\nAUCPR =', 0.4424569868032866],St1),
  writeln(St1).

test(induce_conc_par_bongard):-
  set_lift(verbosity,1),
  set_lift(threads,2),
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

test(induce_par_lbfgs_bongard):-
  set_lift(verbosity,4),
  set_lift(parameter_learning,lbfgs),
  set_lift(eta,0.01),
  induce_par_lift([train],P),test_lift(P,[test],LL,AUCROC,_ROC,AUCPR,_PR),
  writeln('Result:'),
  writeln(P),
  atomic_list_concat(['\nLL=',LL,'\nAUCROC=',AUCROC,'\nAUCPR=',AUCPR,'\n'],St),
  writeln(St),
  atomic_list_concat(['Expected:\n',
  '\nLL =',-219.6533450232687,
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

test(prob_bongard):-
  prob_lift(pos(2),P),
  P=:=0.1986549170515436.

test(prob_sv_bongard):-
  set_lift(single_var,true),
  prob_lift(pos(2),P),
  writeln(P),
  P=:=0.19793513395697515.

test(induce_par_single_var_bongard):-
  set_lift(verbosity,1),
  set_lift(single_var,true),
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

test(induce_single_var_bongard):-
  set_lift(verbosity,1),
  set_lift(single_var,true),
  induce_lift([train],P),test_lift(P,[test],LL,AUCROC,_ROC,AUCPR,_PR),
  writeln('Result:'),
  writeln(P),
  atomic_list_concat(['\nLL=',LL,'\nAUCROC=',AUCROC,'\nAUCPR=',AUCPR,'\n'],St),
  writeln(St),
  atomic_list_concat(['Expected:\n',
  '\nLL =',-219.68079672612654,
  '\nAUCROC =',0.7501277139208173,
  '\nAUCPR =', 0.5997247517658881],St1),
  writeln(St1).

:- end_tests(bongard).
