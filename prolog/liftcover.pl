/** <module> slipcover

This module performs learning over Logic Programs with Annotated
Disjunctions and CP-Logic programs.
It performs both parameter and structure learning.

See https://github.com/friguzzi/cplint/blob/master/doc/manual.pdf or
http://ds.ing.unife.it/~friguzzi/software/cplint-swi/manual.html for
details.

@author Fabrizio Riguzzi, Elena Bellodi
@license Artistic License 2.0
@copyright Fabrizio Riguzzi, Elena Bellodi
*/

/*

SLIPCOVER

Copyright (c) 2016, Fabrizio Riguzzi and Elena Bellodi

*/
:-module(liftcover,[set_lift/2,setting_lift/2,
  induce_lift/2,induce_par_lift/2,test_lift/7,
  filter_rules/2,filter_rules/3,sort_rules/2,
  op(500,fx,#),op(500,fx,'-#'),
  test_prob_lift/6]).
:-use_module(library(auc)).
:-use_module(library(lists)).
:-use_module(library(random)).
:-use_module(library(system)).
:-use_module(library(terms)).
:-use_module(library(rbtrees)).
:-use_module(library(apply)).
:-use_module(library(clpfd), [transpose/2]).
:-absolute_file_name(library(lbfgs),F,[solutions(all)]),atomic_concat(F,'.pl',Fpl),exists_file(Fpl),use_module(library(lbfgs));true.
%:-use_foreign_library(foreign(bddem),install).
:-set_prolog_flag(unknown,warning).

%:-multifile setting_lift/2.
%:-use_module(library(sandbox)).


:- dynamic lift_input_mod/1.

:- thread_local v/3, lift_input_mod/1, local_setting/2, rule_lift_n/1.

:- meta_predicate induce_lift(:,-).
:- meta_predicate induce_rules(:,-).
:- meta_predicate induce_par_lift(:,-).
:- meta_predicate induce_parameters(:,-).



:- meta_predicate test_lift(:,+,-,-,-,-,-).
:- meta_predicate test_prob_lift(:,+,-,-,-,-).
:- meta_predicate set_lift(:,+).
:- meta_predicate setting_lift(:,-).
:- meta_predicate filter_rules(:,-).








default_setting_lift(eps,0.0001).
default_setting_lift(eps_f,0.00001).


default_setting_lift(random_restarts_number,1). % number of random restarts of parameter learning
default_setting_lift(random_restarts_number_str_learn,1). % number of random restarts during structure learning of parameter learning for single clauses
default_setting_lift(iter,100).
default_setting_lift(d,1).
default_setting_lift(verbosity,1).
default_setting_lift(logzero,log(0.000001)).
default_setting_lift(megaex_bottom,1).
default_setting_lift(initial_clauses_per_megaex,1).
default_setting_lift(max_iter,10).
default_setting_lift(max_var,4).
default_setting_lift(maxdepth_var,2).
default_setting_lift(beamsize,100).
default_setting_lift(max_clauses,1000).
default_setting_lift(max_body_length,100).
default_setting_lift(neg_literals,false).

default_setting_lift(specialization,bottom).
/* allowed values: mode,bottom */

default_setting_lift(seed,rand(10,1231,3032)).
default_setting_lift(neg_ex,cw).


default_setting_lift(epsilon_parsing, 1e-5).




default_setting_lift(zero,0.000001).
default_setting_lift(minus_infinity,-1.0e20).

default_setting_lift(regularization,l1). % regularization: no, l1, l2, bayesian 
default_setting_lift(gamma,10). % set the value of gamma for regularization l1 and l2
default_setting_lift(ab,[0,10]). % set the value of a and b for regularization baysian
default_setting_lift(min_probability,1e-5).  % Threshold of the probability under which the clause is dropped
default_setting_lift(parameter_learning,em). % parameter learning algorithm: em_python, em, lbfgs, gd_python, gd 
default_setting_lift(max_initial_weight,0.5). % initial weights of dphil in [-0.5 0.5]

default_setting_lift(parameter_update,fixed_learning_rate). % values: fixed_learning_rate,adam
default_setting_lift(eta,0.01). % fixed learning rate
default_setting_lift(adam_params,[0.001,0.9,0.999,1e-8]). % default Adam hyper-pameters
default_setting_lift(processor,cpu). % where to run em_python and gd_python: cpu or gpu

/**
 * induce_lift(+TrainFolds:list_of_atoms,-P:probabilistic_program) is det
 *
 * The predicate performs structure learning using the folds indicated in
 * TrainFolds for training.
 * It returns in P the learned probabilistic program.
 */
induce_lift(TrainFolds,P):-
  induce_rules(TrainFolds,P0),
  rules2terms(P0,P).
/**
 * test_lift(+P:probabilistic_program,+TestFolds:list_of_atoms,-LL:float,-AUCROC:float,-ROC:dict,-AUCPR:float,-PR:dict) is det
 *
 * The predicate takes as input in P a probabilistic program,
 * tests P on the folds indicated in TestFolds and returns the
 * log likelihood of the test examples in LL, the area under the Receiver
 * Operating Characteristic curve in AUCROC, a dict containing the points
 * of the ROC curve in ROC, the area under the Precision Recall curve in AUCPR
 * and a dict containing the points of the PR curve in PR
 */
test_lift(P,TestFolds,LL,AUCROC,ROC,AUCPR,PR):-
  test_prob_lift(P,TestFolds,_NPos,_NNeg,LL,LG),
  compute_areas_diagrams(LG,AUCROC,ROC,AUCPR,PR).

/**
 * test_prob_lift(+P:probabilistic_program,+TestFolds:list_of_atoms,-NPos:int,-NNeg:int,-LL:float,-Results:list) is det
 *
 * The predicate takes as input in P a probabilistic program,
 * tests P on the folds indicated in TestFolds and returns
 * the number of positive examples in NPos, the number of negative examples
 * in NNeg, the log likelihood in LL
 * and in Results a list containing the probabilistic result for each query contained in TestFolds.
 */
test_prob_lift(M:P,TestFolds,NPos,NNeg,CLL,Results) :-
  write2(M,'Testing\n'),
  make_dynamic(M),
  process_clauses(P,M,PRules),
  generate_clauses(PRules,M,0,[],Prog),
  (M:bg(RBG0)->
    process_clauses(RBG0,M,RBG),
    generate_clauses_bg(RBG,ClBG),
    assert_all(ClBG,M,ClBGRef)
  ;
    true
  ),
  findall(Exs,(member(F,TestFolds),M:fold(F,Exs)),L),
  append(L,DB),
  test_no_area(DB,M,Prog,NPos,NNeg,CLL,Results),
  (M:bg(RBG0)->
    retract_all(ClBGRef)
  ;
    true
  ).

induce_rules(M:Folds,R):-
  load_python_module(M),
  make_dynamic(M),
  M:local_setting(seed,Seed),
  setrand(Seed),
  findall(Exs,(member(F,Folds),M:fold(F,Exs)),L),
  append(L,DB),
  (M:bg(RBG0)->
    process_clauses(RBG0,M,RBG),
    generate_clauses_bg(RBG,ClBG),
    assert_all(ClBG,M,ClBGRef)
  ;
    true
  ),
  find_ex(DB,M,Pos,Neg,NPos,_Neg),
  M:local_setting(megaex_bottom, NumMB),
  (NPos >= NumMB ->
      true
    ;
      format2(M,"~nWARN: Number of required bottom clauses is greater than the number of training examples!~n. The number of required bottom clauses will be equal to the number of training examples", []),
      M:set_lift(megaex_bottom, NPos)
  ),
  statistics(walltime,[_,_]),
  (M:local_setting(specialization,bottom)->
    M:local_setting(megaex_bottom,MB),
    deduct(MB,M,DB,[],InitialTheory),
    length(InitialTheory,_LI),
    remove_duplicates(InitialTheory,R1)
  ;
    get_head_atoms(O,M),
    generate_top_cl(O,M,R1)
  ),
  learn_struct(Pos,Neg,M,R1,R2,Score),
  sort_rules_int(R2,R),
  statistics(walltime,[_,WT]),
  WTS is WT/1000,
  write2(M,'\n\n'),
  format2(M,'/* Final score ~f~n',[Score]),
  format2(M,'Wall time ~f */~n',[WTS]),
  write_rules2(M,R,user_output),
  (M:bg(RBG0)->
    retract_all(ClBGRef)
  ;
    true
  ),
  retractall(M:ref_clause(_)),
  retractall(M:ref(_)).

/**
 * sort_rules(+RulesIn:list_of_rules,-RulesOut:list_of_rules) is det
 *
 * The predicate sorts RulesIn according to the probability of the rules
 */

sort_rules(R0,R):-
  rules2terms(R0,P0),
  sort_rules_int(P0,P),
  rules2terms(R,P).

sort_rules_int(P0,P):-
  maplist(to_pair,P0,P1),
  sort(1,@>=,P1,P2),
  maplist(to_pair,P,P2).

to_pair(rule(N,[H:P|R],BL,Lit),P-rule(N,[H:P|R],BL,Lit)).


make_dynamic(M):-
  M:(dynamic int/1),
  findall(O,M:output(O),LO),
  findall(I,M:input(I),LI),
  findall(I,M:input_cw(I),LIC),
  findall(D,M:determination(D,_DD),LDH),
  findall(DD,M:determination(_D,DD),LDD),
  findall(DH,(M:modeh(_,_,_,LD),member(DH,LD)),LDDH),
  append([LO,LI,LIC,LDH,LDD,LDDH],L0),
  remove_duplicates(L0,L),
  maplist(to_dyn(M),L).

to_dyn(M,P/A):-
  A1 is A+1,
  M:(dynamic P/A1),
  A2 is A1+1,
  M:(dynamic P/A2).



learn_struct(Pos,Neg,Mod,Beam,R,Score):-  %+Beam:initial theory of the form [rule(NR,[h],[b]],...], -R:final theory of the same form, -CLL
  format2(Mod,"Clause search~n~n",[]),
  Mod:local_setting(max_iter,M),
  cycle_beam(Beam,Mod,Pos,Neg,[],CL,[],_BG,M),
  maplist(get_cl,CL,LC,MIC,MINC),
  maplist(append,MIC,MIC1),
  transpose(MIC1,MI),
  append(MINC,MIN),
  length(LC,NumCL),
  write2(Mod,"Final parameter learning"),nl2(Mod),
  Mod:local_setting(random_restarts_number,RR),
  learn_param_int(MI,MIN,NumCL,Mod,RR,Par,Score),
  update_theory(LC,Par,Program1),
  maplist(remove_zero,Program1,Program2),
  append(Program2,R),
  format2(Mod,"Best target theory~n~n",[]),
  write_rules2(Mod,R,user_output).

get_cl([C,_,MI,MIN],C,MI,MIN).

pick_first(0,_,[]):-!.

pick_first(_,[],[]):-!.

pick_first(N,[(H,_S)|T],[H|T1]):-
  N1 is N-1,
  pick_first(N1,T,T1).

remove_score([],[]).

remove_score([(H,_S)|T],[H|T1]):-
  remove_score(T,T1).

cycle_structure([],_Mod,R,S,_SP,_Pos,_Neg,R,S,_I):-!.  %empty beam

cycle_structure(_CL,_Mod,R,S,_SP,_Pos,_Neg,R,S,0):-!.  %0 iterations

cycle_structure([(RH,_CLL)|RT],Mod,R0,S0,SP0,Pos,Neg,R,S,M):-
  already_scored([RH|R0],R3,Mod,Score),!,
  format2(Mod,"Theory iteration ~d~n~n",[M]),
  write3(Mod,'Already scored, updated refinement\n'),
  write_rules3(Mod,R3,user_output),
  write3(Mod,'Score '),write3(Mod,Score),write3(Mod,'\n\n\n'),
  (Score>S0->
    R4=R3,
    S4=Score,
    SP1=S0
  ;
    R4=R0,
    S4=S0,
    SP1=SP0
  ),
  M1 is M-1,
  cycle_structure(RT,Mod,R4,S4,SP1,Pos,Neg,R,S,M1).

cycle_structure([(RH,_Score)|RT],Mod,R0,S0,SP0,Pos,Neg,R,S,M):-
  format2(Mod,"Theory iteration ~d~n~n",[M]),
  format3(Mod,"Initial theory~n~n",[]),
  write_rules3(Mod,[RH|R0],user_output),
  Mod:local_setting(random_restarts_number_str_learn,RR),
  learn_param([RH|R0],Mod,Pos,Neg,RR,R3,Score,_MI,_MIN),
  format3(Mod,"Score after parameter learning = ~f~n",[Score]),
  write3(Mod,'Updated Theory\n'),
  write_rules3(Mod,R3,user_output),   %definite rules without probabilities in the head are not written
  (Score>S0->
    R4=R3,
    S4=Score,
    SP1=S0,
    write3(Mod,'New best score\n')
  ;
    R4=R0,
    S4=S0,
    SP1=SP0
  ),
  store_refinement([RH|R0],R3,Mod,Score),
  M1 is M-1,
  cycle_structure(RT,Mod,R4,S4,SP1,Pos,Neg,R,S,M1).


init_gd_par(0,_Max,[]):-!.

init_gd_par(I,Max,[W|TW]):-
  I1 is I-1,
  W is -Max+random_float*2*Max,
  init_gd_par(I1,Max,TW).

init_par(_Env,0):-!.

init_par(Env,I):-
  I1 is I-1,
  optimizer_set_x(Env,I1,0.5),
  init_par(Env,I1).

evaluate_L(Env,M,MIP,MI,L):-
  compute_likelihood_pos(MIP,Env,M,0,0,LP),
  compute_likelihood_neg(MI,Env,M,LN),
  compute_likelihood(LN,M,LP,L).
  
gen_initial_counts(0,[]):-!.

gen_initial_counts(N0,[0|MIP0]):-
  N1 is N0-1,
  gen_initial_counts(N1,MIP0).

evaluate(Env,L,_N,_Step,[M,MIP,MI]):-
%  M:mip(MIP),
%  M:mi(MI),
  compute_likelihood_pos(MIP,Env,M,0,0,LP),
  compute_likelihood_neg(MI,Env,M,LN),
  compute_likelihood(LN,M,LP,L),
  compute_grad(MIP,Env,M,0,MI,LN).

compute_grad([],_Env,_M,_N,_MI,_LN):-!.

compute_grad([HMIP|TMIP],Env,M,N0,MI,LN):-
  compute_sum_neg(MI,M,LN,N0,0,S),
  optimizer_get_x(Env,N0,P0),
  M:local_setting(zero,Zero),
  (P0=<0 ->
    PI=Zero
  ;
    (P0>=1.0->
       PI is 1.0-Zero
     ;
       PI=P0
     )
  ),

 (PI=:= 1.0->
    G is 1.0/Zero
  ;
    G is (HMIP-S)/(1.0-PI)
  ),
  optimizer_set_g(Env,N0,G),
  N1 is N0+1,
  compute_grad(TMIP,Env,M,N1,MI,LN).

compute_sum_neg([],_M,_LN,_I,S,S).

compute_sum_neg([HMI|TMI],M,[HLN|TLN],I,S0,S):-
  nth0(I,HMI,MIR),
  Den is 1.0-exp(-HLN),
  M:local_setting(zero,Zero),
  (Den=<0.0->
    Den1 is Zero
  ;
    Den1 = Den
  ),
  S1 is S0+MIR*exp(-HLN)/Den1,
  compute_sum_neg(TMI,M,TLN,I,S1,S).

compute_likelihood([],_M,L,L).

compute_likelihood([HP|TP],M,L0,L):-
  A is 1.0-exp(-HP),
  M:local_setting(zero,Zero),
  (A=<0.0->
    A1 is Zero
  ;
    A1=A
  ),
  L1 is L0-log(A1),
  compute_likelihood(TP,M,L1,L).

compute_likelihood_neg([],_Env,_M,[]).

compute_likelihood_neg([HMI|TMI],Env,M,[HLN|TLN]):-
  compute_likelihood_pos(HMI,Env,M,0,0,HLN),
  compute_likelihood_neg(TMI,Env,M,TLN).

compute_likelihood_pos([],_Env,_M,_,LP,LP).

compute_likelihood_pos([HMIP|TMIP],Env,M,I,LP0,LP):-
  optimizer_get_x(Env,I,P0),
  M:local_setting(zero,Zero),
  (P0=<0.0 ->
    P=Zero
  ;
    (P0>=1.0->
       P is 1-Zero
     ;
       P=P0
     )
  ),
  LP1 is LP0-log(1-P)*HMIP,
  I1 is I+1,
  compute_likelihood_pos(TMIP,Env,M,I1,LP1,LP).

progress(_Env,FX,X_Norm,G_Norm,Step,_N,Iteration,Ls,0,[M|_]) :-
  format4(M,'~d. Iteration :  f(X)=~4f  |X|=~4f
                |g(X)|=~4f  Step=~4f  Ls=~4f~n',
                [Iteration,FX,X_Norm,G_Norm,Step,Ls]).

gen_par(NC,NC,[]):-!.

gen_par(N0,NC,[[N0,[0.5,0.5]]|T]):-
  N1 is N0+1,
  gen_par(N1,NC,T).


logistic(X,Sigma_X):-
  Sigma_X is 1/(1+exp(-X)).


load_python_module(M):-
  M:local_setting(parameter_learning,PL),
  (PL=em_python;PL=gd_python),!,
  absolute_file_name(library('liftcover.pl'), F),
  file_directory_name(F,Dir),
  py_add_lib_dir(Dir),
  M:local_setting(processor,Proc),
  M:local_setting(verbosity,Verb),
  py_call(liftcover:init(PL,Proc,Verb)).

load_python_module(_).

/**
 * induce_par_lift(+TrainFolds:list_of_atoms,-P:probabilistic_program) is det
 *
 * The predicate learns the parameters of the program stored in the in/1 fact
 * of the input file using the folds indicated in TrainFolds for training.
 * It returns in P the input program with the updated parameters.
 */
induce_par_lift(Folds,ROut):-
  induce_parameters(Folds,R),
  rules2terms(R,ROut).

induce_parameters(M:Folds,R):-
  load_python_module(M),
  make_dynamic(M),
  M:local_setting(seed,Seed),
  setrand(Seed),
  findall(Exs,(member(F,Folds),M:fold(F,Exs)),L),
  append(L,DB),
  statistics(walltime,[_,_]),
  (M:bg(RBG0)->
    process_clauses(RBG0,M,RBG),
    generate_clauses_bg(RBG,ClBG),
    assert_all(ClBG,M,ClBGRef)
  ;
    true
  ),
  M:in(R00),
  process_clauses(R00,M,R0),
  statistics(walltime,[_,_]),
  find_ex(DB,M,Pos,Neg,_NPos,_NNeg),
  M:local_setting(random_restarts_number_str_learn,RR),
  learn_param(R0,M,Pos,Neg,RR,R1,Score,_MI,_MIN),
  sort_rules_int(R1,R),
  statistics(walltime,[_,CT]),
  CTS is CT/1000,
  format2(M,'/* Final score ~f~n',[Score]),
  format2(M,'Wall time ~f */~n',[CTS]),
  write_rules2(M,R,user_output),
  (M:bg(RBG0)->
    retract_all(ClBGRef)
  ;
    true
  ).

/**
 * filter_rules(+RulesIn:list_of_rules,-RulesOut:list_of_rules) is det
 *
 * The predicate removes from RulesIn the rules with a probability
 * below the min_prob parmeter
 */
filter_rules(M:R0,R):-
  M:local_setting(min_probability,Min_prob),
  filter_rules(R0,R,Min_prob).


/**
 * filter_rules(+RulesIn:list_of_rules,-RulesOut:list_of_rules,+Min_prob) is det
 *
 * The predicate removes from RulesIn the rules with a probability
 * below Min_prob
 */

filter_rules(R0,R,Min_prob):-
  rules2terms(R0,R0At),
  remove_clauses(R0At,Min_prob,RAt,_Num),
  rules2terms(R,RAt).
 
remove_clauses(Rules,Prob,RulesOut,Num):-
  remove_clauses_loop(Rules,Prob,0,Num,[],RulesOut).

remove_clauses_loop([],_,Num,Num,Rules,Rules):-!.
remove_clauses_loop([Rule|Rest],Prob,NumCur,Num,RulesCur,RulesOut):-
  Rule=rule(_N,[_Head:Par|_],_,_),
  Par < Prob,!,
  NumCur1 is NumCur+1,
  remove_clauses_loop(Rest, Prob, NumCur1,Num,RulesCur, RulesOut).

remove_clauses_loop([Rule|Rest],Prob,NumCur,Num,RulesCur,[Rule|RulesOut]):-
  remove_clauses_loop(Rest, Prob, NumCur,Num,RulesCur, RulesOut).


test_theory_neg_prob(Ex,M,Theory,MIP0,MIP):-
  test_clause_prob(Theory,M,Ex,MIP0,MIP).

test_clause_prob([],_M,_Exs,MIP,MIP).

test_clause_prob([(H,B,V,_P)|Rest],M,Exs,[MIPH0|MIPT0],[MIPH|MIPT]):-
  maplist(test_ex(V,H,B,M),Exs,L),
  sum_list(L,MIP),
  MIPH is MIPH0+MIP,
  test_clause_prob(Rest,M,Exs,MIPT0,MIPT).

test_ex(_V,H,B,M,E,N):-
  findall(1,(H=E,M:B),L),
  length(L,N).


test_theory_pos_prob([],_M,_Theory,_N,[]).

test_theory_pos_prob([Ex|Rest],M,Th,N,[MI|LMI]):-
  gen_initial_counts(N,MI0),
  test_clause_prob(Th,M,[Ex],MI0,MI),
  test_theory_pos_prob(Rest,M,Th,N,LMI).

learn_param([],M,_,_,_,[],MInf,[],[]):-!,
  M:local_setting(minus_infinity,MInf).

learn_param(Program0,M,Pos,Neg,RR,Program,LL,MI,MIN):-
  generate_clauses(Program0,M,0,[],Pr1),
  length(Program0,N),
  gen_initial_counts(N,MIN0),
  format4(M,'Computing clause statistics~n',[]),
  test_theory_neg_prob(Neg,M,Pr1,MIN0,MIN),
  test_theory_pos_prob(Pos,M,Pr1,N,MI),
  format4(M,'Updating parameters~n',[]),
  learn_param_int(MI,MIN,N,M,RR,Par,LL),
  update_theory(Program0,Par,Program1),
  maplist(remove_zero,Program1,Program2),
  append(Program2,Program).

learn_param_int(MI,MIN,N,M,NR,Par,LL):-
  M:local_setting(parameter_learning,em),!,
  random_restarts(NR,M,-1e20,LL,N,initial,Par,MI,MIN),
  format3(M,"Final LL ~f~n",[LL]).

learn_param_int(MI,MIN,_N,M,NR,Par,LL):-
  M:local_setting(parameter_learning,em_python),!,
  M:local_setting(eps,EA),
  M:local_setting(eps_f,ER),
  M:local_setting(iter,Iter),
  M:local_setting(regularization,Reg),
  M:local_setting(gamma,Gamma),
  M:local_setting(zero,Zero),
  M:local_setting(ab,[A,B]),
  M:local_setting(verbosity,Verb),
  M:local_setting(processor,Device),
  py_call(liftcover:random_restarts(MI,MIN,Device,NR,Iter,EA,ER,Reg,Zero,Gamma,A,B,Verb),-(Par,LL)),
  format3(M,"Final LL ~f~n",[LL]).

learn_param_int(MI,MIN,N,M,NR,Par,LL):-
  M:local_setting(parameter_learning,gd),!,
  random_restarts_gd(NR,M,-1e20,PLL,N,initial,ParR,MI,MIN),  %computes new parameters Par
  maplist(logistic,ParR,Par),
  LL is -PLL,
  format3(M,"Final LL ~f~n",[LL]).


learn_param_int(MI,MIN,_N,M,NR,Par,LL):-
  M:local_setting(parameter_learning,gd_python),!,
  M:local_setting(verbosity,Verb),
  M:local_setting(parameter_update,UpdateMethod),
  M:local_setting(iter,Iter),
  M:local_setting(eps,Eps),
  M:local_setting(adam_params,[Eta,Beta1,Beta2,Epsilon]),
  M:local_setting(eta,LearningRate),
  M:local_setting(gamma,Gamma),
  M:local_setting(regularization,Reg),
  M:local_setting(zero,Zero),
  M:local_setting(processor,Device),
  py_call(liftcover:random_restarts_gd(MI,MIN,Device,NR,UpdateMethod,
    Iter,Eps,Reg,Gamma,LearningRate,Eta,-(Beta1,Beta2),Epsilon,Zero,Verb),-(Par,LL)),
  format3(M,"Final LL ~f~n",[LL]).


learn_param_int(MI,MIN,N,M,_,Par,LL):-
  M:local_setting(parameter_learning,lbfgs),
  optimizer_initialize(N,liftcover,evaluate,progress,[M,MIN,MI],Env),
  init_par(Env,N),
  evaluate_L(Env,M,MIN,MI,L),
% parte da modificare fine
  IL is -L,
  format4(M,"~nInitial L ~f~n",[IL]),
  optimizer_run(Env,_LL,Status),
  format4(M,"Status ~p~n",[Status]),
  new_pars_lbfgs(Env,M,0,N,Par),
  evaluate_L(Env,M,MIN,MI,NewL),
  LL is -NewL,
  optimizer_finalize(Env).


new_pars_lbfgs(_Env,_M,N,N,[]):-!.

new_pars_lbfgs(Env,M,N,NMax,[P|Rest1]):-
    optimizer_get_x(Env,N,P0),
    M:local_setting(zero,Zero),
    (P0=<0.0->
      P=Zero
    ;
      (P0>=1.0->
        P is 1.0-Zero
      ;
        P=P0
      )
    ),
    N1 is N+1,
    new_pars_lbfgs(Env,M,N1,NMax,Rest1).


random_restarts(0,_M,Score,Score,_N,Par,Par,_MI,_MIN):-!.

random_restarts(N,M,Score0,Score,NR,Par0,Par,MI,MIN):-
  M:local_setting(random_restarts_number,NMax),
  Num is NMax-N+1,
  format3(M,"Restart number ~d~n~n",[Num]),
  length(Par1,NR),
  maplist(random,Par1),
  M:local_setting(eps,EA),
  M:local_setting(eps_f,ER),
  M:local_setting(iter,Iter),
  em_quick(EA,ER,0,Iter,M,NR,Par1,-1e20,MI,MIN,ParR,ScoreR),
  format3(M,"Random_restart: Score ~f~n",[ScoreR]),
  N1 is N-1,
  (ScoreR>Score0->
    random_restarts(N1,M,ScoreR,Score,NR,ParR,Par,MI,MIN)
  ;
    random_restarts(N1,M,Score0,Score,NR,Par0,Par,MI,MIN)
  ).

random_restarts_gd(0,_M,Score,Score,_N,Par,Par,_MI,_MIN):-!.

random_restarts_gd(N,M,_Score0,Score,NR,_Par0,Par,MI,MIN):-
  M:local_setting(random_restarts_number,NMax),
  Num is NMax-N+1,
  format3(M,"Restart number ~d~n~n",[Num]),
  M:local_setting(max_initial_weight,Max),
  init_gd_par(NR,Max,Par1),
  evaluate_L_gd(M,MIN,MI,Par1,L),
  ScoreIn is -L,
  format3(M,"GD Random_restart: initial score ~f~n",[ScoreIn]),
  M:local_setting(eps,EA),
  M:local_setting(eps_f,ER),
  M:local_setting(iter,Iter),
  (M:local_setting(parameter_update,adam)->
    findall(0,between(1,NR,_),M0),
    findall(0,between(1,NR,_),M1),
    gd_adam(EA,ER,0,Iter,M,NR,Par1,M0,M1,-1e20,MI,MIN,ParR,ScoreR)
  ;
    gd(EA,ER,0,Iter,M,NR,Par1,-1e20,MI,MIN,ParR,ScoreR)
  ),
  format3(M,"GD Random_restart: Score ~f~n",[ScoreR]),
  N1 is N-1,
  random_restarts_gd(N1,M,ScoreR,Score,NR,ParR,Par,MI,MIN).


gd(_EA,_ER,MaxIter,MaxIter,_M,_NR,Par,Score,_MI,_MIP,Par,Score):-!.

gd(EA,ER,Iter0,MaxIter,M,NR,Par0,Score0,MI,MIP,Par,Score):-
  compute_gradient_gd(MIP,MI,M,Par0,G,LL),
  Score1 is -LL,
  format4(M,"GD Iteration ~d LL ~f~n",[Iter0,Score1]),
  Iter is Iter0+1,
  Diff is Score1-Score0,
  Fract is -Score1*ER,
  (( Diff<EA;Diff<Fract)->
    Score=Score1,
    Par=Par0
  ;
    M:local_setting(eta,Eta),
    M:local_setting(gamma,Gamma),
    (M:local_setting(regularization,l2)->
      maplist(l2,Par0,Reg)
    ;
      (M:local_setting(regularization,l1)->
        maplist(l1,Par0,Reg)
      ;
        findall(0,between(1,NR,_),Reg)
      )
    ),
    maplist(update_par(Eta,Gamma),Par0,Reg,G,Par1),
    gd(EA,ER,Iter,MaxIter,M,NR,Par1,Score1,MI,MIP,Par,Score)
  ).

gd_adam(_EA,_ER,Iter,Iter,_M,_NR,Par,_M0,_M1,Score,_MI,_MIP,Par,Score):-!.

gd_adam(EA,ER,Iter0,MaxIter,M,NR,Par0,M00,M10,Score0,MI,MIP,Par,Score):-
  compute_gradient_gd(MIP,MI,M,Par0,G,LL),
  Score1 is -LL,
  format4(M,"Iteration ~d LL ~f~n",[Iter0,Score1]),
  Iter is Iter0+1,
  Diff is Score1-Score0,
  Fract is -Score1*ER,
  (( Diff<EA;Diff<Fract)->
    Score=Score1,
    Par=Par0
  ;
    M:local_setting(gamma,Gamma),
    M:local_setting(adam_params,[Eta,Beta1,Beta2,Epsilon]),
    (M:local_setting(regularization,l2)->
      maplist(l2,Par0,Reg)
    ;
      (M:local_setting(regularization,l1)->
        maplist(l1,Par0,Reg)
      ;
        findall(0,between(1,NR,_),Reg)
      )
    ),
    maplist(update_grad(Gamma),G,Reg,G1),
    maplist(update_M0(Beta1),M00,G1,M0),
    maplist(update_M1(Beta2),M10,G1,M1),
    EtaIter is Eta*sqrt(1-Beta2^Iter)/(1-Beta1^Iter),
    maplist(update_par_adam(EtaIter,Epsilon),Par0,M0,M1,Par1),
    gd_adam(EA,ER,Iter,MaxIter,M,NR,Par1,M0,M1,Score1,MI,MIP,Par,Score)
  ).

update_par_adam(EtaIter,Epsilon,Par0,M0,M1,Par1):-
  Par1 is Par0-EtaIter*M0/(sqrt(M1)+Epsilon).


update_M0(Beta1,M00,G,M0):-
  M0 is Beta1*M00+(1-Beta1)*G.

update_M1(Beta2,M10,G,M1):-
  M1 is Beta2*M10+(1-Beta2)*G^2.

update_grad(Gamma,G,Reg,G1):-
  G1 is G+Gamma*Reg.


l1(W,R):-
  logistic(W,S),
  R is S*(1-S).

l2(W,R):-
  logistic(W,S),
  R is 2*S^2*(1-S).

update_par(Eta,Gamma,Par0,Reg,G,Par1):-
  Par1 is Par0-Eta*(G+Gamma*Reg).



evaluate_L_gd(M,MIP,MI,Par,L):-
  maplist(logistic,Par,Prob),
  compute_likelihood_pos_gd(MIP,Prob,M,0,LP),
%  write(lpos),nl,
  compute_likelihood_neg_gd(MI,Prob,M,LN),
%  write(lneg),nl,
  compute_likelihood_gd(LN,M,LP,L).



compute_gradient_gd(MIP,MI,M,Par,G,L):-
  maplist(logistic,Par,Prob),
  compute_likelihood_pos_gd(MIP,Prob,M,0,LP),
  compute_likelihood_neg_gd(MI,Prob,M,LN),
  compute_likelihood_gd(LN,M,LP,L),
  compute_grad_gd(MIP,Prob,M,0,MI,LN,G).
%  write(grad),nl.

compute_likelihood_neg_gd([],_Prob,_M,[]).

compute_likelihood_neg_gd([HMI|TMI],Prob,M,[HLN|TLN]):-
  compute_likelihood_pos_gd(HMI,Prob,M,0,HLN),
  compute_likelihood_neg_gd(TMI,Prob,M,TLN).

compute_likelihood_pos_gd([],[],_M,LP,LP).

compute_likelihood_pos_gd([HMIP|TMIP],[P|TP],M,LP0,LP):-
  LP1 is LP0-log(1-P)*HMIP,
  compute_likelihood_pos_gd(TMIP,TP,M,LP1,LP).


compute_likelihood_gd([],_M,L,L).

compute_likelihood_gd([HP|TP],M,L0,L):-
  A is 1.0-exp(-HP),
  (A=:=0.0->
    M:local_setting(logzero,LZ),
    L1 is L0-LZ
  ;
    L1 is L0-log(A)
  ),
  compute_likelihood_gd(TP,M,L1,L).


compute_grad_gd([],[],_M,_N,_MI,_LN,[]):-!.

compute_grad_gd([HMIP|TMIP],[P|TP],M,N0,MI,LN,[G|TG]):-
%  write(prima_comp_grad),nl,
  compute_sum_neg(MI,M,LN,N0,0,S),
%  write(opt),nl,
  G is (HMIP-S)*P,
  N1 is N0+1,
  compute_grad_gd(TMIP,TP,M,N1,MI,LN,TG).

compute_sum_neg_gd([],_M,_LN,_I,S,S).

compute_sum_neg_gd([HMI|TMI],M,[HLN|TLN],I,S0,S):-
%  write(HMI),write(hmi),nl,
%  write(I),write('I'),nl,
  nth0(I,HMI,MIR),
%  write(MIR),write(mir),nl,
%  write(HLN),write(hln),nl,
  Den is 1.0-exp(-HLN),
  S1 is S0+MIR*exp(-HLN)/Den,
  compute_sum_neg_gd(TMI,M,TLN,I,S1,S).


em_quick(_EA,_ER,MaxIter,MaxIter,_M,_NR,Par,Score,_MI,_MIN,Par,Score):-!.

em_quick(EA,ER,Iter0,MaxIter,M,NR,Par0,Score0,MI,MIN,Par,Score):-
  length(Eta0,NR),
  expectation_quick(Par0,M,MI,MIN,Eta0,Eta,Score1),
  format4(M,"Iteration ~d LL ~f~n",[Iter0,Score1]),
  maximization_quick(Eta,M,Par1),
  Iter is Iter0+1,
  Diff is Score1-Score0,
  Fract is -Score1*ER,
  (( Diff<EA;Diff<Fract)->
    Score=Score1,
    Par=Par1
  ;
    em_quick(EA,ER,Iter,MaxIter,M,NR,Par1,Score1,MI,MIN,Par,Score)
  ).

expectation_quick(Par,M,MI,MIN,Eta0,Eta,Score):-
 /* LLO is the negative examples contribution in the LL*/
  M:local_setting(logzero,LogZero),
  foldl(llm(LogZero),Par,MIN,0,LL0),
  maplist(eta0,MIN,Eta0),
  /* positive examples contibution in LL*/
  scan_pos(MI,M,Par,LL0,Eta0,Score,Eta).

maximization_quick(Eta,M,Par):-
  (M:local_setting(regularization,l1)->
    maplist(maximize_L1(M),Eta,Par)
  ;
    (M:local_setting(regularization,l2)->
      maplist(maximize_L2(M),Eta,Par)
  ;
      (M:local_setting(regularization,bayesian)->
        maplist(maximize_bayesian(M),Eta,Par)
      ;
        maplist(maximize(M),Eta,Par)
  )
    )
  ).

maximize(_M,[Eta0,Eta1],Par):-
  (Eta0+Eta1=:=0.0->
    Par=0.0
  ;
    Par is Eta1/(Eta0+Eta1)  
  ).


maximize_L1(M,[Eta0,Eta1],Par):-
  M:local_setting(gamma,Gamma),
/*(((Eta0+Eta1)^2+Gamma^2+2*Gamma*(Eta0-Eta1))<0 ->
writeln((Eta0+Eta1)^2+Gamma^2+2*Gamma*(Eta0-Eta1)),
  Par is 4*Eta1/(2*(Gamma+Eta0+Eta1))
;((2*(Gamma+Eta0+Eta1+sqrt((Eta0+Eta1)^2+Gamma^2+2*Gamma*(Eta0-Eta1)))=:=0.0)->
writeln((2*(Gamma+Eta0+Eta1+sqrt((Eta0+Eta1)^2+Gamma^2+2*Gamma*(Eta0-Eta1)))))

;*/
  Par is 4*Eta1/(2*(Gamma+Eta0+Eta1+sqrt((Eta0+Eta1)^2+Gamma^2+2*Gamma*(Eta0-Eta1))))
%)
  %)
  .

maximize_L2(M,[Eta0,Eta1],Par):-
  M:local_setting(gamma,Gamma),
  Sum is 3*Eta0+3*Eta1+Gamma,
  Arccos is acos(sqrt(Gamma/Sum)*(9*Eta0/2-9*Eta1+Gamma)/(3*Eta0+3*Eta1+Gamma)),
  Par is 2*sqrt(Sum/Gamma)*cos(Arccos/3-2*pi/3)/3+1/3.

maximize_bayesian(M,[Eta0,Eta1],Par):-
  M:local_setting(ab,[A,B]),
  Par is (Eta1+A)/(Eta0+Eta1+A+B).

/*....scan_pos predicate..... */

scan_pos([],_M,_Par,LL,Eta,LL,Eta).

scan_pos([MIH|MIT],M,Par,LL0,Eta0,LL,Eta):-
  M:local_setting(logzero,LogZero),
  foldl(rule_contrib,MIH,Par,1,Prod),
  ProbEx is 1-Prod,
  (ProbEx=:=0.0->
    LLCurrent is LL0+LogZero
   ;
    LLCurrent is LL0+log(ProbEx)
  ),
  maplist(update_eta(ProbEx,M),Eta0,Par,MIH,EtaCurrent),
  scan_pos(MIT,M,Par,LLCurrent,EtaCurrent,LL,Eta).


update_eta(ProbEx,M,[Etai00,Etai10],Pi,MIR,[Etai0,Etai1]):-
  M:local_setting(zero,Zero),
  ( ProbEx=:=0.0->
    CondP is Pi/Zero
  ;
    CondP is Pi/ProbEx
  ),
  OCondP0 is 1-CondP,
  ( OCondP0 <0.0->
    OCondP = 0.0
  ;
    OCondP = OCondP0
  ),
  Etai0 is Etai00+MIR*OCondP,
  Etai1 is Etai10+MIR*CondP.

rule_contrib(MIR,Pi,P0,P):-
  P is P0*(1-Pi)^MIR.

llm(LogZero,Pi,MI,LL0,LL):-
  ((1-Pi)=:=0.0 ->

    ( MI==0 ->
      LL is LL0
     ;
       LL is LL0+MI*LogZero
    )
   ;
    ( MI==0 ->
       LL is LL0
     ;
       LL is LL0+MI*log(1-Pi)
    )

  ).

eta0(MIN,[MIN,0]).


remove_zero(rule(_Name,[_H:P,_],_B,_L),[]):-
    local_setting(zero,Zero),
    P=:=Zero,!.

remove_zero(R,[R]).


update_theory([],_N,[]):-!.

update_theory([rule(Name,[H:_,_],B,L)|Rest],[P|T],[rule(Name,[H:P,'':PN],B,L)|Rest1]):-
    PN is 1-P,
    update_theory(Rest,T,Rest1).



cycle_beam([],_Mod,_Pos,_Neg,CL,CL,CLBG,CLBG,_M):-!.

cycle_beam(_Beam,_Mod,_Pos,_Neg,CL,CL,CLBG,CLBG,0):-!.

cycle_beam(Beam,Mod,Pos,Neg,CL0,CL,CLBG0,CLBG,M):-
  format2(Mod,"Clause iteration ~d~n~n",[M]),
  /*write('\n\ncurrent beam\n\n'),
  write(Beam),
  write('\n\n'),*/
  cycle_clauses(Beam,Mod,Pos,Neg,[],NB,CL0,CL1,CLBG0,CLBG1),
  M1 is M-1,%decreases the number of max_iter M
  cycle_beam(NB,Mod,Pos,Neg,CL1,CL,CLBG1,CLBG,M1).

cycle_clauses([],_M,_Pos,_Neg,NB,NB,CL,CL,CLBG,CLBG):-!.

cycle_clauses([[RH,_ScoreH]|T],M,Pos,Neg,NB0,NB,CL0,CL,CLBG0,CLBG):-
%  write3('\n\nRevising clause\n'),
%  write_rules3([RH],user_output),
%  RH=rule(_,H,B,Lits),
%  write3(H),write3(' '),write3(B),
%  write3('\n'),write3(Lits),write3('\n'),
  findall(RS,specialize_rule(RH,M,RS,_L),LR),!,   %-LR:list of lists, each one correponding to a different revised theory; specialize_rule defined in revise.pl
  length(LR,NR),
  write3(M,'Number of revisions '),write3(M,NR),write3(M,'\n'),
  score_clause_refinements(LR,M,1,NR,Pos,Neg,NB0,NB1,CL0,CL1,CLBG0,CLBG1),
  cycle_clauses(T,M,Pos,Neg,NB1,NB,CL1,CL,CLBG1,CLBG).

score_clause_refinements([],_M,_N,_NR,_Pos,_Neg,NB,NB,CL,CL,CLBG,CLBG).

score_clause_refinements([R1|T],M,Nrev,NRef,Pos,Neg,NB0,NB,CL0,CL,CLBG0,CLBG):-  %scans the list of revised theories
  already_scored_clause(R1,R3,M,Score),!,
  format3(M,'Score ref.  ~d of ~d~n',[Nrev,NRef]),
  write3(M,'Already scored, updated refinement\n'),
  write_rules3(M,[R3],user_output),
  write3(M,'Score '),write3(M,Score),write3(M,'\n\n\n'),
  M:local_setting(beamsize,BS),
  insert_in_order(NB0,[R3,Score],BS,NB1),
  Nrev1 is Nrev+1,
  score_clause_refinements(T,M,Nrev1,NRef,Pos,Neg,NB1,NB,CL0,CL,CLBG0,CLBG).

score_clause_refinements([R1|T],M,Nrev,NRef,Pos,Neg,NB0,NB,CL0,CL,CLBG0,CLBG):-
  format3(M,'Score ref.  ~d of ~d~n',[Nrev,NRef]),
  write_rules3(M,[R1],user_output),
  M:local_setting(random_restarts_number_str_learn,NR),
  learn_param([R1],M,Pos,Neg,NR,NewR,Score,MI,MIN),
  write3(M,'Updated refinement\n'),
  write_rules3(M,NewR,user_output),
  write3(M,'Score (CLL) '),write3(M,Score),write3(M,'\n\n\n'),
  (NewR=[R3]->
    M:local_setting(beamsize,BS),
    M:local_setting(max_clauses,MC),
    insert_in_order(NB0,[R3,Score],BS,NB1),
    (range_restricted(R3)->
      insert_in_order(CL0,[R3,Score,MI,MIN],MC,CL1)
    ;
      CL1=CL0
    ),
    length(CL1,LCL1),
    format2(M,"N. of target clauses ~d~n~n",[LCL1]),
    store_clause_refinement(R1,R3,M,Score),
    Nrev1 is Nrev+1
  ;
    NB1=NB0,
    CL1=CL0,
    Nrev1=Nrev+1
  ),
  CLBG1=CLBG0,  
  score_clause_refinements(T,M,Nrev1,NRef,Pos,Neg,NB1,NB,CL1,CL,CLBG1,CLBG).

range_restricted(rule(_N,HL,BL,_Lit)):-
  term_variables(HL,VH),
  term_variables(BL,VB),
  sublisteq(VH,VB).

sublisteq([],_).

sublisteq([H|T],L):-
  member_eq(H,L),
  sublisteq(T,L).

target(R,M):-
  get_output_preds(R,O),
  member(T,O),
  M:output(T),!.

get_output_preds(rule(_N,HL,_BL,_Lit),O):-
  scan_head(HL,[],O).

scan_head(['':_],O,O):-!.
scan_head([],O,O):-!.
scan_head([H:_P|T],O0,O):-
  functor(H,F,N),
  (member(F/N,O0)->
    O1=O0
  ;
    O1=[F/N|O0]
  ),
  scan_head(T,O1,O).


store_clause_refinement(Ref,RefP,M,Score):-
  elab_clause_ref(Ref,Ref1),
  assert(M:ref_clause(r(Ref1,RefP,Score))).

store_refinement(Ref,RefP,M,Score):-
  elab_ref(Ref,Ref1),
  assert(M:ref(r(Ref1,RefP,Score))).

already_scored_clause(R,R1,M,Score):-
  elab_ref([R],[rule(H,B)]),
  M:ref_clause(r(rule(H,B1),R1,Score)),
  permutation(B,B1).

already_scored(R,R1,M,Score):-
  elab_ref(R,RR),
  M:ref(r(RR,R1,Score)).


elab_clause_ref(rule(_NR,H,B,_Lits),rule(H1,B1)):-
  copy_term((H,B),(H1,B1)).

elab_ref([],[]).

elab_ref([rule(_NR,H,B,_Lits)|T],[rule(H1,B1)|T1]):-!,
  copy_term((H,B),(H1,B1)),
  numbervars((H1,B1),0,_N),
  elab_ref(T,T1).

elab_ref([def_rule(H,B,_Lits)|T],[rule(H1,B1)|T1]):-
  copy_term((H,B),(H1,B1)),
  numbervars((H1,B1),0,_N),
  elab_ref(T,T1).

%insertion in the beam
insert_in_order([],C,BeamSize,[C]):-
  BeamSize>0,!.

insert_in_order(Beam,_New,0,Beam):-!.

insert_in_order([[Th1,Heuristic1|Rest1]|RestBeamIn],[Th,Heuristic|Rest],BeamSize,BeamOut):-
  Heuristic>Heuristic1,!,
  % larger heuristic, insert here
  NewBeam=[[Th,Heuristic|Rest],[Th1,Heuristic1|Rest1]|RestBeamIn],
  length(NewBeam,L),
  (L>BeamSize->
    nth1(L,NewBeam,_Last,BeamOut)
  ;
    BeamOut=NewBeam
  ).

insert_in_order([[Th1,Heuristic1|Rest1]|RestBeamIn],[Th,Heuristic|Rest],BeamSize,
[[Th1,Heuristic1|Rest1]|RestBeamOut]):-
  BeamSize1 is BeamSize -1,
  insert_in_order(RestBeamIn,[Th,Heuristic|Rest],BeamSize1,
  RestBeamOut).



remove_int_atom_list([],[]).

remove_int_atom_list([\+ A|T],[\+ A1|T1]):-!,
  A=..[F,_|Arg],
  A1=..[F|Arg],
  remove_int_atom_list(T,T1).

remove_int_atom_list([A|T],[A1|T1]):-
  A=..[F,_|Arg],
  A1=..[F|Arg],
  remove_int_atom_list(T,T1).



remove_int_atom(\+ A,\+ A1):-!,
  A=..[F,_|T],
  A1=..[F|T].

remove_int_atom(A,A1):-
  A=..[F,_|T],
  A1=..[F|T].


get_heads([],[]).

get_heads([_-H|T],[H|TN]):-
  get_heads(T,TN).

randomize([],[]):-!.

randomize([rule(N,V,NH,HL,BL,LogF)|T],[rule(N,V,NH,HL1,BL,LogF)|T1]):-
  length(HL,L),
  Int is 1.0/L,
  randomize_head(Int,HL,0,HL1),
  randomize(T,T1).

randomize_head(_Int,['':_],P,['':PNull1]):-!,
  PNull is 1.0-P,
  (PNull>=0.0->
    PNull1 =PNull
  ;
    PNull1=0.0
  ).

randomize_head(Int,[H:_|T],P,[H:PH1|NT]):-
  PMax is 1.0-P,
  random(0,PMax,PH1),
  P1 is P+PH1,
  randomize_head(Int,T,P1,NT).



update_head([],[],_N,[]):-!.

update_head([H:_P|T],[PU|TP],N,[H:P|T1]):-
  P is PU/N,
  update_head(T,TP,N,T1).

/* utilities */


rules2terms(R,T):-
  maplist(rule2term,R,T).

rule2term(rule(_N,HL,BL,_Lit),(H:-B)):-
  list2or(HL,H),
  list2and(BL,B).

rule2term(def_rule(H,BL,_Lit),((H:1.0):-B)):-
  list2and(BL,B).


write_rules([],_S).

write_rules([rule(_N,HL,BL,Lit)|T],S):-!,
  copy_term((HL,BL,Lit),(HL1,BL1,Lit1)),
  numbervars((HL1,BL1,Lit1),0,_M),
  write_disj_clause(S,(HL1:-BL1)),
  write_rules(T,S).

write_rules([def_rule(H,BL,Lit)|T],S):-
  copy_term((H,BL,Lit),(H1,BL1,Lit1)),
  numbervars((H1,BL1,Lit1),0,_M),
  write_disj_clause(S,([H1:1.0]:-BL1)),
  write_rules(T,S).


new_par([],[],[]).

new_par([HP|TP],[Head:_|TO],[Head:HP|TN]):-
  new_par(TP,TO,TN).



write_disj_clause(S,(H:-[])):-!,
  write_head(S,H),
  format(S,".~n~n",[]).

write_disj_clause(S,(H:-B)):-
  write_head(S,H),
  format(S,' :-',[]),
  nl(S),
  write_body(S,B).


write_head(S,[A:1.0|_Rest]):-!,
  format(S,"~q:1.0",[A]).

write_head(S,[A:P,'':_P]):-!,
  format(S,"~q:~g",[A,P]).

write_head(S,[A:P]):-!,
  format(S,"~q:~g",[A,P]).

write_head(S,[A:P|Rest]):-
  format(S,"~q:~g ; ",[A,P]),
  write_head(S,Rest).

write_body(S,[]):-!,
  format(S,"  true.~n~n",[]).

write_body(S,[A]):-!,
  format(S,"  ~q.~n~n",[A]).

write_body(S,[A|T]):-
  format(S,"  ~q,~n",[A]),
  write_body(S,T).

/**
 * list2or(+List:list,-Or:term) is det.
 * list2or(-List:list,+Or:term) is det.
 *
 * The predicate succeeds when Or is a disjunction (using the ; operator)
 * of the terms in List
 */
list2or([],true):-!.

list2or([X],X):-
    X\=;(_,_),!.

list2or([H|T],(H ; Ta)):-!,
    list2or(T,Ta).


/**
 * list2and(+List:list,-And:term) is det.
 * list2and(-List:list,+And:term) is det.
 *
 * The predicate succeeds when And is a conjunction (using the , operator)
 * of the terms in List
 */
list2and([],true):-!.

list2and([X],X):-
    X\=(_,_),!.

list2and([H|T],(H,Ta)):-!,
    list2and(T,Ta).


deduct(0,_Mod,_DB,Th,Th):-!.

deduct(NM,Mod,DB,InTheory0,InTheory):-
  get_head_atoms(O,Mod),
  sample_lift(1,DB,Sampled,DB1),
  (Sampled=[M]->
    generate_head(O,M,Mod,[],HL),
    %gtrace,
    ( HL \== [] ->
       (generate_body(HL,Mod,InTheory1),
    	append(InTheory0,InTheory1,InTheory2),
    	NM1 is NM-1,
    	deduct(NM1,Mod,DB1,InTheory2,InTheory)
       )
      ;
       deduct(NM,Mod,DB,InTheory0,InTheory)
    )
  ;
    InTheory=InTheory0
  ).


get_head_atoms(O,M):-
  findall(A,M:modeh(_,A),O).

generate_top_cl([],_M,[]):-!.

generate_top_cl([A|T],M,[(rule(R,[A1:0.5,'':0.5],[],true),-1e20)|TR]):-
  A=..[F|ArgM],
  keep_const(ArgM,Arg),
  A1=..[F|Arg],
  get_next_rule_number(M,R),
  generate_top_cl(T,M,TR).


generate_head([],_M,_Mod,HL,HL):-!.

generate_head([A|T],M,Mod,H0,H1):-
  functor(A,F,N),
  functor(F1,F,N),
  F1=..[F|Arg],
  Pred1=..[F,M|Arg],
  A=..[F|ArgM],
  keep_const(ArgM,Arg),
  findall((A,Pred1),call(Mod:Pred1),L),
  Mod:local_setting(initial_clauses_per_megaex,IC),
  sample_lift(IC,L,L1),
  append(H0,L1,H2),
  generate_head(T,M,Mod,H2,H1).

generate_head_goal([],_M,[]).

generate_head_goal([H|T],M,[H1|T1]):-
  H=..[F|Arg],
  H1=..[F,M|Arg],
  generate_head_goal(T,M,T1).

keep_const([],[]).

keep_const([- _|T],[_|T1]):-!,
  keep_const(T,T1).

keep_const([+ _|T],[_|T1]):-!,
  keep_const(T,T1).

keep_const([-# _|T],[_|T1]):-!,
  keep_const(T,T1).

keep_const([H|T],[H1|T1]):-
  H=..[F|Args],
  keep_const(Args,Args1),
  H1=..[F|Args1],
  keep_const(T,T1).


/**
 * sample_lift(+N,List:list,-Sampled:list,-Rest:list) is det
 *
 * Samples N elements from List and returns them in Sampled.
 * The rest of List is returned in Rest
 * If List contains less than N elements, Sampled is List and Rest
 * is [].
*/
sample_lift(0,List,[],List):-!.

sample_lift(N,List,List,[]):-
  length(List,L),
  L=<N,!.

sample_lift(N,List,[El|List1],Li):-
  length(List,L),
  random(0,L,Pos),
  nth0(Pos,List,El,Rest),
  N1 is N-1,
  sample_lift(N1,Rest,List1,Li).

sample_lift(0,_List,[]):-!.

sample_lift(N,List,List):-
  length(List,L),
  L=<N,!.

sample_lift(N,List,[El|List1]):-
  length(List,L),
  random(0,L,Pos),
  nth0(Pos,List,El,Rest),
  N1 is N-1,
  sample_lift(N1,Rest,List1).

get_args([],[],[],A,A,AT,AT,_).

get_args([HM|TM],[H|TH],[(H,HM)|TP],A0,A,AT0,AT,M):-
  HM=..[F|ArgsTypes],
  H=..[F,M|Args],
  append(A0,Args,A1),
  append(AT0,ArgsTypes,AT1),
  get_args(TM,TH,TP,A1,A,AT1,AT,M).

/* Generation of the bottom clauses */

gen_head([],P,['':P]).

gen_head([H|T],P,[H:P|TH]):-
  gen_head(T,P,TH).

get_modeb([],_Mod,B,B).

get_modeb([F/AA|T],Mod,B0,B):-
  findall((R,B),(Mod:modeb(R,B),functor(B,F,AA)),BL),
  (Mod:local_setting(neg_literals,true)->
    findall((R,(\+ B)),(Mod:modeb(R,B),functor(B,F,AA),all_plus(B)),BNL)
  ;
    BNL=[]
  ),
  append([B0,BL,BNL],B1),
  get_modeb(T,Mod,B1,B).

all_plus(B):-
  B=..[_|Args],
  all_plus_args(Args).

all_plus_args([]).

all_plus_args([+ _ |T]):-!,
  all_plus_args(T).

all_plus_args([H|T]):-
  H \= - _,
  H \= # _,
  H \= -# _,
  H=..[_|Args],
  all_plus_args(Args),
  all_plus_args(T).

generate_body([],_Mod,[]):-!.

generate_body([(A,H)|T],Mod,Out):-
  functor(A,F,AA),
%  findall((R,B),(Mod:modeb(R,B),functor(B,FB,AB),Mod:determination(F/AA,FB/AB)),BL),
  findall(FB/AB,Mod:determination(F/AA,FB/AB),Det),
  get_modeb(Det,Mod,[],BL),
  A=..[F|ArgsTypes],
  H=..[F,M|Args],
  Mod:local_setting(d,D),
  format2(Mod,"Bottom clause: example ~q~n",[H]),
  cycle_modeb(ArgsTypes,Args,[],[],Mod,BL,a,[],BLout0,D,M),
  variabilize(([(H,A)]:-BLout0),CLV),  %+(Head):-Bodylist;  -CLV:(Head):-Bodylist with variables _num in place of constants
  CLV=([Head1]:-BodyList1),
  (range_restricted(rule(_,Head1,BodyList1,_))->
    remove_int_atom(Head1,Head),
    remove_int_atom_list(BodyList1,BodyList2),
    remove_duplicates(BodyList2,BodyList),
    get_next_rule_number(Mod,R),
    copy_term((Head,BodyList),(HeadV,BodyListV)),
    numbervars((HeadV,BodyListV),0,_V),
    format2(Mod,"Clause~n~q:0.5 :-~n",[HeadV]),
    write_body2(Mod,user_output,BodyListV),
    Out=[[rule(R,[Head:0.5,'':0.5],[],BodyList),-1e20]|CL0]
  ;
    format2(Mod,"No range restricted bottom clause~n~n",[]),
    Out=CL0
  ),
  generate_body(T,Mod,CL0).


variabilize((H:-B),(H1:-B1)):-
  variabilize_list(H,H1,[],AS,M),
  variabilize_list(B,B1,AS,_AS,M).


variabilize_list([],[],A,A,_M).

variabilize_list([(\+ H,Mode)|T],[\+ H1|T1],A0,A,M):-
  builtin(H),!,
  H=..[F|Args],
  Mode=..[F|ArgTypes],
  variabilize_args(Args,ArgTypes, Args1,A0,A1),
  H1=..[F,M|Args1],
  variabilize_list(T,T1,A1,A,M).

variabilize_list([(\+ H,Mode)|T],[\+ H1|T1],A0,A,M):-!,
  H=..[F,_M|Args],
  Mode=..[F|ArgTypes],
  variabilize_args(Args,ArgTypes, Args1,A0,A1),
  H1=..[F,M|Args1],
  variabilize_list(T,T1,A1,A,M).

variabilize_list([(H,Mode)|T],[H1|T1],A0,A,M):-
  builtin(H),!,
  H=..[F|Args],
  Mode=..[F|ArgTypes],
  variabilize_args(Args,ArgTypes, Args1,A0,A1),
  H1=..[F,M|Args1],
  variabilize_list(T,T1,A1,A,M).

variabilize_list([(H,Mode)|T],[H1|T1],A0,A,M):-
  H=..[F,_M|Args],
  Mode=..[F|ArgTypes],
  variabilize_args(Args,ArgTypes, Args1,A0,A1),
  H1=..[F,M|Args1],
  variabilize_list(T,T1,A1,A,M).


variabilize_args([],[],[],A,A).

variabilize_args([C|T],[C|TT],[C|TV],A0,A):-!,
  variabilize_args(T,TT,TV,A0,A).

variabilize_args([C|T],[# _Ty|TT],[C|TV],A0,A):-!,
  variabilize_args(T,TT,TV,A0,A).

variabilize_args([C|T],[-# _Ty|TT],[C|TV],A0,A):-!,
  variabilize_args(T,TT,TV,A0,A).

variabilize_args([C|T],[Ty|TT],[V|TV],A0,A):-
  (Ty = +Ty1;Ty = -Ty1),
  member(C/Ty1/V,A0),!,
  variabilize_args(T,TT,TV,A0,A).

variabilize_args([C|T],[Ty|TT],[V|TV],A0,A):-
  (Ty = +Ty1;Ty = -Ty1),!,
  variabilize_args(T,TT,TV,[C/Ty1/V|A0],A).

variabilize_args([C|T],[Ty|TT],[V|TV],A0,A):-
  compound(C),
  C=..[F|Args],
  Ty=..[F|ArgsT],
  variabilize_args(Args,ArgsT,ArgsV,A0,A1),
  V=..[F|ArgsV],
  variabilize_args(T,TT,TV,A1,A).


cycle_modeb(ArgsTypes,Args,ArgsTypes,Args,_Mod,_BL,L,L,L,_,_M):-!.

cycle_modeb(_ArgsTypes,_Args,_ArgsTypes1,_Args1,_Mod,_BL,_L,L,L,0,_M):-!.

cycle_modeb(ArgsTypes,Args,_ArgsTypes0,_Args0,Mod,BL,_L0,L1,L,D,M):-
  find_atoms(BL,Mod,ArgsTypes,Args,ArgsTypes1,Args1,L1,L2,M),
  D1 is D-1,
  cycle_modeb(ArgsTypes1,Args1,ArgsTypes,Args,Mod,BL,L1,L2,L,D1,M).


find_atoms([],_Mod,ArgsTypes,Args,ArgsTypes,Args,L,L,_M).

find_atoms([(R,\+ H)|T],Mod,ArgsTypes0,Args0,ArgsTypes,Args,L0,L1,M):-!,
  H=..[F|ArgsT],
  findall((A,H),instantiate_query_neg(ArgsT,ArgsTypes0,Args0,F,M,A),L),
  call_atoms(L,Mod,[],At),
  remove_duplicates(At,At1),
  ((R = '*' ) ->
    R1= +1e20
  ;
    R1=R
  ),
  sample_lift(R1,At1,At2),
  append(L0,At2,L2),
  find_atoms(T,Mod,ArgsTypes0,Args0,ArgsTypes,Args,L2,L1,M).

find_atoms([(R,H)|T],Mod,ArgsTypes0,Args0,ArgsTypes,Args,L0,L1,M):-
  H=..[F|ArgsT],
  findall((A,H),instantiate_query(ArgsT,ArgsTypes0,Args0,F,M,A),L),
  call_atoms(L,Mod,[],At),
  remove_duplicates(At,At1),
  ((R = '*' ) ->
    R1= +1e20
  ;
    R1=R
  ),
  sample_lift(R1,At1,At2),
  extract_output_args(At2,ArgsT,ArgsTypes0,Args0,ArgsTypes1,Args1),
  append(L0,At2,L2),
  find_atoms(T,Mod,ArgsTypes1,Args1,ArgsTypes,Args,L2,L1,M).


call_atoms([],_Mod,A,A).

call_atoms([(H,M)|T],Mod,A0,A):-
  findall((H,M),Mod:H,L),
  append(A0,L,A1),
  call_atoms(T,Mod,A1,A).


extract_output_args([],_ArgsT,ArgsTypes,Args,ArgsTypes,Args).

extract_output_args([(H,_At)|T],ArgsT,ArgsTypes0,Args0,ArgsTypes,Args):-
  builtin(H),!,
  H=..[_F|ArgsH],
  add_const(ArgsH,ArgsT,ArgsTypes0,Args0,ArgsTypes1,Args1),
  extract_output_args(T,ArgsT,ArgsTypes1,Args1,ArgsTypes,Args).

extract_output_args([(H,_At)|T],ArgsT,ArgsTypes0,Args0,ArgsTypes,Args):-
  H=..[_F,_M|ArgsH],
  add_const(ArgsH,ArgsT,ArgsTypes0,Args0,ArgsTypes1,Args1),
  extract_output_args(T,ArgsT,ArgsTypes1,Args1,ArgsTypes,Args).


add_const([],[],ArgsTypes,Args,ArgsTypes,Args).

add_const([_A|T],[+_T|TT],ArgsTypes0,Args0,ArgsTypes,Args):-!,
  add_const(T,TT,ArgsTypes0,Args0,ArgsTypes,Args).

add_const([A|T],[-Type|TT],ArgsTypes0,Args0,ArgsTypes,Args):-!,
  (already_present(ArgsTypes0,Args0,A,Type)->
    ArgsTypes1=ArgsTypes0,
    Args1=Args0
  ;
    ArgsTypes1=[+Type|ArgsTypes0],
    Args1=[A|Args0]
  ),
  add_const(T,TT,ArgsTypes1,Args1,ArgsTypes,Args).

add_const([A|T],[-# Type|TT],ArgsTypes0,Args0,ArgsTypes,Args):-!,
  (already_present(ArgsTypes0,Args0,A,Type)->
    ArgsTypes1=ArgsTypes0,
    Args1=Args0
  ;
    ArgsTypes1=[+Type|ArgsTypes0],
    Args1=[A|Args0]
  ),
  add_const(T,TT,ArgsTypes1,Args1,ArgsTypes,Args).

add_const([_A|T],[# _|TT],ArgsTypes0,Args0,ArgsTypes,Args):-!,
  add_const(T,TT,ArgsTypes0,Args0,ArgsTypes,Args).

add_const([A|T],[A|TT],ArgsTypes0,Args0,ArgsTypes,Args):-
  atomic(A),!,
  add_const(T,TT,ArgsTypes0,Args0,ArgsTypes,Args).

add_const([A|T],[AT|TT],ArgsTypes0,Args0,ArgsTypes,Args):-
  A=..[F|Ar],
  AT=..[F|ArT],
  add_const(Ar,ArT,ArgsTypes0,Args0,ArgsTypes1,Args1),
  add_const(T,TT,ArgsTypes1,Args1,ArgsTypes,Args).


already_present([+T|_TT],[C|_TC],C,T):-!.

already_present([_|TT],[_|TC],C,T):-
  already_present(TT,TC,C,T).


instantiate_query_neg(ArgsT,ArgsTypes,Args,F,M,A):-
  instantiate_input(ArgsT,ArgsTypes,Args,ArgsB),
  A1=..[F|ArgsB],
  (builtin(A1)->
    A= (\+ A1)
  ;
    A0=..[F,M|ArgsB],
    A = (\+ A0)
  ).

instantiate_query(ArgsT,ArgsTypes,Args,F,M,A):-
  instantiate_input(ArgsT,ArgsTypes,Args,ArgsB),
  A1=..[F|ArgsB],
  (builtin(A1)->
    A=A1
  ;
    A=..[F,M|ArgsB]
  ).


instantiate_input([],_AT,_A,[]).

instantiate_input([-_Type|T],AT,A,[_V|TA]):-!,
  instantiate_input(T,AT,A,TA).

instantiate_input([+Type|T],AT,A,[H|TA]):-!,
  find_val(AT,A,+Type,H),
  instantiate_input(T,AT,A,TA).

instantiate_input([# Type|T],AT,A,[H|TA]):-!,
  find_val(AT,A,+Type,H),
  instantiate_input(T,AT,A,TA).

instantiate_input([-# _Type|T],AT,A,[_V|TA]):-!,
  instantiate_input(T,AT,A,TA).

instantiate_input([C|T],AT,A,[C1|TA]):-
  C=..[F|Args],
  instantiate_input(Args,AT,A,Args1),
  C1=..[F|Args1],
  instantiate_input(T,AT,A,TA).


find_val([T|_TT],[A|_TA],T,A).

find_val([HT|_TT],[HA|_TA],T,A):-
  nonvar(HA),
  HT=..[F|ArgsT],
  HA=..[F|Args],
  find_val(ArgsT,Args,T,A).

find_val([_T|TT],[_A|TA],T,A):-
  find_val(TT,TA,T,A).


get_output_atoms(O,M):-
  findall((A/Ar),M:output((A/Ar)),O).


generate_goal([],_M,_H,G,G):-!.

generate_goal([P/A|T],M,H,G0,G1):-
  functor(Pred,P,A),
  Pred=..[P|Rest],
  Pred1=..[P,H|Rest],
  findall(Pred1,call(M:Pred1),L),
  findall(\+ Pred1,call(M:neg(Pred1)),LN),
  append(G0,L,G2),
  append(G2,LN,G3),
  generate_goal(T,M,H,G3,G1).

remove_duplicates(L0,L):-
  remove_duplicates(L0,[],L1),
  reverse(L1,L).

remove_duplicates([],L,L).

remove_duplicates([H|T],L0,L):-
  member_eq(H,L0),!,
  remove_duplicates(T,L0,L).

remove_duplicates([H|T],L0,L):-
  remove_duplicates(T,[H|L0],L).


/*

EMBLEM and SLIPCASE

Copyright (c) 2011, Fabrizio Riguzzi, Nicola di Mauro and Elena Bellodi

*/





specialize_rule(Rule,M,_SpecRule,_Lit):-
  M:local_setting(max_body_length,ML),
  Rule = rule(_ID,_LH,BL,_Lits),
  length(BL,L),
  L=ML,!,
  fail.

specialize_rule(Rule,M,SpecRule,Lit):-
  M:local_setting(max_body_length,ML),
  Rule = rule(_ID,_LH,BL,_Lits),
  length(BL,L),
  (L=ML->
    !,
    fail
  ;
    specialize_rule_int(Rule,M,SpecRule,Lit),
    (L=:=ML-1->
      range_restricted(SpecRule)
    ;
      true
    )
  ).
    

%used by cycle_clauses in slipcover.pl
specialize_rule_int(Rule,M,SpecRule,Lit):-
  M:local_setting(specialization,bottom),
  Rule = rule(ID,LH,BL,Lits),
  delete_one(Lits,RLits,Lit),
  \+ M:lookahead_cons(Lit,_),
  \+ M:lookahead_cons_var(Lit,_),
  \+ member_eq(Lit,BL),
  append(BL,[Lit],BL1),
  remove_prob(LH,LH1),
  delete(LH1,'',LH2),
  append(LH2,BL1,ALL2),
  dv(LH2,BL1,M,DList), 	%-DList: list of couples (variable,depth)
  extract_fancy_vars(ALL2,Vars1),
  length(Vars1,NV),
  M:local_setting(max_var,MV),
  NV=<MV,
  linked_clause(BL1,M,LH2),
  M:local_setting(maxdepth_var,MD),
  exceed_depth(DList,MD),
  \+ banned_clause(LH2,BL1),
  SpecRule=rule(ID,LH,BL1,RLits).

specialize_rule_int(Rule,M,SpecRule,Lit):-
  M:local_setting(specialization,bottom),
  Rule = rule(ID,LH,BL,Lits),
  delete_one(Lits,RLits,Lit),
  \+ member_eq(Lit,BL),
  append(BL,[Lit],BL0),
  \+M:lookahead_cons_var(Lit,_),
  (M:lookahead(Lit,LLit1);M:lookahead_cons(Lit,LLit1)),
  copy_term(LLit1,LLit2),
  specialize_rule_la_bot(LLit2,RLits,RLits1,BL0,BL1),
  remove_prob(LH,LH1),
  delete(LH1,'',LH2),
  append(LH2,BL1,ALL2),
  dv(LH2,BL1,M,DList),
  extract_fancy_vars(ALL2,Vars1),
  length(Vars1,NV),
  M:local_setting(max_var,MV),
  NV=<MV,
  linked_clause(BL1,M,LH2),
  M:local_setting(maxdepth_var,MD),
  exceed_depth(DList,MD),
  \+ banned_clause(LH2,BL1),
  SpecRule=rule(ID,LH,BL1,RLits1).

specialize_rule_int(Rule,M,SpecRule,Lit):-
  M:local_setting(specialization,bottom),
  Rule = rule(ID,LH,BL,Lits),
  delete_one(Lits,RLits,Lit),
  \+ member_eq(Lit,BL),
  append(BL,[Lit],BL0),
  M:lookahead_cons_var(Lit,LLit2),
  specialize_rule_la_bot(LLit2,RLits,_RLits1,BL0,BL1),
  remove_prob(LH,LH1),
  delete(LH1,'',LH2),
  append(LH2,BL1,ALL2),
  dv(LH2,BL1,M,DList),
  extract_fancy_vars(ALL2,Vars1),
  length(Vars1,NV),
  M:local_setting(max_var,MV),
  NV=<MV,
  linked_clause(BL1,M,LH2),
  M:local_setting(maxdepth_var,MD),
  exceed_depth(DList,MD),
  \+ banned_clause(LH2,BL1),
  SpecRule=rule(ID,LH,BL1,[]).

specialize_rule_int(Rule,M,SpecRule,Lit):-
  M:local_setting(specialization,mode),%!,
  findall(BL , M:modeb(_,BL), BLS),
  specialize_rule(BLS,Rule,M,SpecRule,Lit).


update_head1([],_N,[]):-!.

update_head1([H:_P|T],N,[H:P|T1]):-
	       P is 1/N,
	       update_head1(T,N,T1).


banned_clause(H,B):-
  lift_input_mod(M),
  numbervars((H,B),0,_N),
  M:banned(H2,B2),
  mysublist(H2,H),
  mysublist(B2,B).


mysublist([],_).

mysublist([H|T],L):-
  member(H,L),
  mysublist(T,L).


specialize_rule([Lit|_RLit],Rule,M,SpecRul,SLit):-
  Rule = rule(ID,LH,BL,true),
  remove_prob(LH,LH1),
  append(LH1,BL,ALL),
  specialize_rule1(Lit,ALL,SLit),
  append(BL,[SLit],BL1),
  (M:lookahead(SLit,LLit1);M:lookahead_cons(SLit,LLit1)),
  specialize_rule_la(LLit1,LH1,BL1,BL2),
  append(LH1,BL2,ALL2),
  extract_fancy_vars(ALL2,Vars1),
  length(Vars1,NV),
  M:local_setting(max_var,MV),
  NV=<MV,
  SpecRul = rule(ID,LH,BL2,true).

specialize_rule([Lit|_RLit],Rule,M,SpecRul,SLit):-
  Rule = rule(ID,LH,BL,true),
  remove_prob(LH,LH1),
  append(LH1,BL,ALL),
  specialize_rule1(Lit,ALL,SLit),
  \+ M:lookahead_cons(SLit,_),
  append(BL,[SLit],BL1),
  append(LH1,BL1,ALL1),
  extract_fancy_vars(ALL1,Vars1),
  length(Vars1,NV),
  M:local_setting(max_var,MV),
  NV=<MV,
  SpecRul = rule(ID,LH,BL1,true).

specialize_rule([_|RLit],Rule,M,SpecRul,Lit):-
  specialize_rule(RLit,Rule,M,SpecRul,Lit).


specialize_rule_la([],_LH1,BL1,BL1).

specialize_rule_la([Lit1|T],LH1,BL1,BL3):-
  copy_term(Lit1,Lit2),
  lift_input_mod(M),
  M:modeb(_,Lit2),
  append(LH1,BL1,ALL1),
  specialize_rule1(Lit2,ALL1,SLit1),
  append(BL1,[SLit1],BL2),
  specialize_rule_la(T,LH1,BL2,BL3).


specialize_rule_la_bot([],Bot,Bot,BL,BL).

specialize_rule_la_bot([Lit|T],Bot0,Bot,BL1,BL3):-
  delete_one(Bot0,Bot1,Lit),
  \+ member_eq(Lit,BL1),
  append(BL1,[Lit],BL2),
  specialize_rule_la_bot(T,Bot1,Bot,BL2,BL3).


remove_prob(['':_P],[]):-!.

remove_prob([X:_|R],[X|R1]):-
  remove_prob(R,R1).


specialize_rule1(Lit,Lits,SpecLit):-
  Lit =.. [Pred|Args],
  exctract_type_vars(Lits,TypeVars0),
  remove_duplicates(TypeVars0,TypeVars),
  take_var_args(Args,TypeVars,Args1),
  SpecLit =.. [Pred|Args1],
  \+ member_eq(SpecLit,Lits).


convert_to_input_vars([],[]):-!.

convert_to_input_vars([+T|RT],[+T|RT1]):-
  !,
  convert_to_input_vars(RT,RT1).

convert_to_input_vars([-T|RT],[+T|RT1]):-
  convert_to_input_vars(RT,RT1).



remove_eq(X,[Y|R],R):-
  X == Y,
  !.

remove_eq(X,[_|R],R1):-
  remove_eq(X,R,R1).

linked_clause(BodyLits,M,[Head]):-
  input_head_variables(Head,M,InputHeadVars),
  linked_clause_int(BodyLits,M,InputHeadVars).

linked_clause_int([],_M,_).

linked_clause_int([L|R],M,PrevVars):-
  input_variables(L,M,InputVars),
  linked(InputVars,PrevVars),!,
  term_variables(L,LVars),
  append(LVars,PrevVars,PrevVars1),
  linked_clause_int(R,M,PrevVars1).


linked([],_).

linked([X|R],L) :-
  member_eq(X,L),
  !,
  linked(R,L).


input_variables(\+ LitM,M,InputVars):-
  !,
  LitM=..[P|Args],
  length(Args,LA),
  length(Args1,LA),
  Lit1=..[P|Args1],
%  copy_term(LitM,Lit0),
  M:modeb(_,Lit1),
  Lit1 =.. [P|Args1],
  convert_to_input_vars(Args1,Args2),
  Lit2 =.. [P|Args2],
  input_vars(LitM,Lit2,InputVars).

input_variables(LitM,M,InputVars):-
  LitM=..[P|Args],
  length(Args,LA),
  length(Args1,LA),
  Lit1=..[P|Args1],
  M:modeb(_,Lit1),
  input_vars(LitM,Lit1,InputVars).

input_head_variables(LitM,M,InputVars):-
  LitM=..[P|Args],
  length(Args,LA),
  length(Args1,LA),
  Lit1=..[P|Args1],
  M:modeh(_,Lit1),
  input_vars(LitM,Lit1,InputVars).

input_vars(Lit,Lit1,InputVars):-
  Lit =.. [_|Vars],
  Lit1 =.. [_|Types],
  input_vars1(Vars,Types,InputVars).


input_vars1([],_,[]).

input_vars1([V|RV],[+_T|RT],[V|RV1]):-
  !,
  input_vars1(RV,RT,RV1).

input_vars1([_V|RV],[_|RT],RV1):-
  input_vars1(RV,RT,RV1).


exctract_type_vars([],[]).

exctract_type_vars([Lit|RestLit],TypeVars):-
  Lit =.. [Pred|Args],
  length(Args,L),
  length(Args1,L),
  Lit1 =.. [Pred|Args1],
  take_mode(Lit1),
  type_vars(Args,Args1,Types),
  exctract_type_vars(RestLit,TypeVars0),
  !,
  append(Types,TypeVars0,TypeVars).


take_mode(Lit):-
  lift_input_mod(M),
  M:modeh(_,Lit),!.

take_mode(Lit):-
  lift_input_mod(M),
  M:modeb(_,Lit),!.

take_mode(Lit):-
  lift_input_mod(M),
  M:mode(_,Lit),!.


type_vars([],[],[]).

type_vars([V|RV],[+T|RT],[V=T|RTV]):-
  !,
  type_vars(RV,RT,RTV).

type_vars([V|RV],[-T|RT],[V=T|RTV]):-atom(T),!,
  type_vars(RV,RT,RTV).

type_vars([_V|RV],[_T|RT],RTV):-
  type_vars(RV,RT,RTV).


take_var_args([],_,[]).

take_var_args([+T|RT],TypeVars,[V|RV]):-
  !,
  member(V=T,TypeVars),
  take_var_args(RT,TypeVars,RV).

take_var_args([-T|RT],TypeVars,[_V|RV]):-
  atom(T),
  take_var_args(RT,TypeVars,RV).

take_var_args([-T|RT],TypeVars,[V|RV]):-
  member(V=T,TypeVars),
  take_var_args(RT,TypeVars,RV).

take_var_args([T|RT],TypeVars,[T|RV]):-
  T\= + _,(T\= - _; T= - A,number(A)),
  take_var_args(RT,TypeVars,RV).


choose_rule(Theory,Rule):-
  member(Rule,Theory).


add_rule(Theory,add(rule(ID,H,[],true))):-
  new_id(ID),
  findall(HL , modeh(_,HL), HLS),
  length(HLS,NH),
  P is 1/(NH+1),
  add_probs(HLS,H,P),
  \+ member(rule(_,H,[],true),Theory).

add_rule(Theory,TheoryGen):-
  findall(HL , modeh(_,HL), HLS),
  add_rule(HLS,Theory,TheoryGen).

add_rule([X|_R],Theory,TheoryGen) :-
  new_id(ID),
  X =.. [P|A],
  length(A,LA),
  length(A1,LA),
  PH =.. [P|A1],
  TheoryGen = add(rule(ID,[PH:0.5,'':0.5],[],true)),
  \+ member(rule(_,[PH:_,'':_],[],true),Theory).

add_rule([_X|R],Theory,TheoryGen) :-
  add_rule(R,Theory,TheoryGen).


add_probs([],['':P],P):-!.

add_probs([H|T],[H:P|T1],P):-
  add_probs(T,T1,P).


extract_fancy_vars(List,Vars):-
  term_variables(List,Vars0),
  fancy_vars(Vars0,1,Vars).


fancy_vars([],_,[]).

fancy_vars([X|R],N,[NN2=X|R1]):-
  name(N,NN),
  append([86],NN,NN1),
  name(NN2,NN1),
  N1 is N + 1,
  fancy_vars(R,N1,R1).


delete_one([X|R],R,X).

delete_one([X|R],[X|R1],D):-
  delete_one(R,R1,D).


remove_last([_X],[]) :-
  !.

remove_last([X|R],[X|R1]):-
  remove_last(R,R1).


delete_matching([],_El,[]).

delete_matching([El|T],El,T1):-!,
  delete_matching(T,El,T1).

delete_matching([H|T],El,[H|T1]):-
  delete_matching(T,El,T1).


%Computation of the depth of the variables in the clause's head/body
dv(H,B,M,DV1):-			%DV1: returns a list of couples (Variable, Max depth)
	term_variables(H,V),
	head_depth(V,DV0),
	findall((MD-DV),var_depth(B,M,DV0,DV,0,MD),LDs),
        get_max(LDs,-1,-,DV1).


input_variables_b(\+ LitM,M,InputVars):-!,
	  LitM=..[P|Args],
	  length(Args,LA),
	  length(Args1,LA),
	  Lit1=..[P|Args1],
	  M:modeb(_,Lit1),
	  all_plus(Lit1),
	  input_vars(LitM,Lit1,InputVars).

input_variables_b(LitM,M,InputVars):-
	  LitM=..[P|Args],
	  length(Args,LA),
	  length(Args1,LA),
	  Lit1=..[P|Args1],
	  M:modeb(_,Lit1),
	  input_vars(LitM,Lit1,InputVars).



%associates depth 0 to each variable in the clause's head
head_depth([],[]).
head_depth([V|R],[[V,0]|R1]):-
  head_depth(R,R1).

%associates a depth to each variable in the clause's body
var_depth([],_M,PrevDs1,PrevDs1,MD,MD):-!.

var_depth([L|R],M,PrevDs,PrevDs1,_MD,MD):-    		%L = a body literal, MD = maximum depth set by the user
  input_variables_b(L,M,InputVars),
  term_variables(L, BodyAtomVars),
  output_vars(BodyAtomVars,InputVars,OutputVars),
  depth_InputVars(InputVars,PrevDs,0,MaxD),   		%MaxD: maximum depth of the input variables in the body literal
  D is MaxD+1,
  compute_depth(OutputVars,D,PrevDs,PrevDs0), 		%Computes the depth for the output variables in the body literal
  var_depth(R,M,PrevDs0,PrevDs1,D,MD).

get_max([],_,Ds,Ds).

get_max([(MD-DsH)|T],MD0,_Ds0,Ds):-
  MD>MD0,!,
  get_max(T,MD,DsH,Ds).

get_max([_H|T],MD,Ds0,Ds):-
	get_max(T,MD,Ds0,Ds).

delete_eq([],_E,[]).

delete_eq([H|T],E,T1):-
  H==E,!,
  delete_eq(T,E,T1).

delete_eq([H|T],E,[H|T1]):-
  delete_eq(T,E,T1).

output_vars(OutVars,[],OutVars):-!.
output_vars(BodyAtomVars,[I|InputVars],OutVars):-
  delete_eq(BodyAtomVars, I, Residue),
  output_vars(Residue,InputVars, OutVars).

% returns D as the maximum depth of the variables in the list (first argument)
depth_InputVars([],_,D,D).
depth_InputVars([I|Input],PrevDs,D0,D):-
	 member_l(PrevDs,I,MD),
	 (MD>D0->
		D1=MD
	;
		D1=D0
         ),
	 depth_InputVars(Input,PrevDs,D1,D).

member_l([[L,D]|_P],I,D):-
     I==L,!.
member_l([_|P],I,D):-
     member_l(P,I,D).

compute_depth([],_,PD,PD):-!.
compute_depth([O|Output],D,PD,RestO):-
	member_l(PD,O,_),!,
	compute_depth(Output,D,PD,RestO).

compute_depth([O|Output],D,PD,[[O,D]|RestO]):-
	compute_depth(Output,D,PD,RestO).



%checks if a variable's depth exceeds the setting_lift
exceed_depth([],_):-!.
exceed_depth([H|T],MD):-
	nth1(2,H,Dep),
	Dep<MD, %setting_lift(maxdepth_var,MD),
	exceed_depth(T,MD).

/*

EMBLEM and SLIPCASE

Copyright (c) 2011, Fabrizio Riguzzi and Elena Bellodi

*/




%:- yap_flag(single_var_warnings, on).


load(FileIn,C1,R):-
  open(FileIn,read,SI),
  read_clauses_dir(SI,C),
  close(SI),
  process_clauses(C,[],C1,[],R).


add_inter_cl(CL):-
  %findall(A,(input(A);output(A)),L),
  findall(A,(input(A)),L),
  gen_cl(L,CL).


gen_cl([],[]).

gen_cl([H/A|T],[C|T1]):-
  functor(F,H,A),
  add_mod_arg(F,Module,F1),
  add_bdd_arg(F,BDD,Module,F2),
  C=(F2:-(F1,one(BDD))),
  gen_cl(T,T1).


assert_all([],_M,[]).

assert_all([H|T],M,[HRef|TRef]):-
  assertz(M:H,HRef),
  assert_all(T,M,TRef).

assert_all([],[]).

assert_all([H|T],[HRef|TRef]):-
  assertz(slipcover:H,HRef),
  assert_all(T,TRef).


retract_all([],_):-!.

retract_all([H|T],M):-
  erase(M,H),
  retract_all(T,M).

retract_all([]):-!.

retract_all([H|T]):-
  erase(H),
  retract_all(T).


read_clauses_dir(S,[Cl|Out]):-
  read_term(S,Cl,[]),
  (Cl=end_of_file->
    Out=[]
  ;
    read_clauses_dir(S,Out)
  ).


process_clauses([],_M,[]):-!.

process_clauses([end_of_file],_M,[]):-!.

process_clauses([H|T],M,[H1|T1]):-
  term_expansion_int(H,M,(_,[H1])),
  process_clauses(T,M,T1).


get_next_rule_number(M,R):-
  retract(M:rule_lift_n(R)),
  R1 is R+1,
  assert(M:rule_lift_n(R1)).

add_bdd_arg(A,Env,BDD,A1):-
  A=..[P|Args],
  append(Args,[Env,BDD],Args1),
  A1=..[P|Args1].


add_bdd_arg_db(A,Env,BDD,DB,A1):-
  A=..[P|Args],
  append(Args,[DB,Env,BDD],Args1),
  A1=..[P|Args1].


add_bdd_arg(A,_Env,_BDD,Module,A1):-
  A=..[P|Args],
  A1=..[P,Module|Args].


add_bdd_arg_db(A,_Env,_BDD,DB,Module,A1):-
  A=..[P|Args],
  append(Args,[DB],Args1),
  A1=..[P,Module|Args1].

add_mod_arg(A,Module,A1):-
  A=..[P|Args],
  A1=..[P,Module|Args].


generate_rules_fact([],_Env,_VC,_R,_Probs,_N,[],_Module).

generate_rules_fact([Head:_P1,'':_P2],Env,VC,R,Probs,N,[Clause],Module):-!,
  add_bdd_arg(Head,Env,BDD,Module,Head1),
  Clause=(Head1:-(pita:get_var_n(Env,R,VC,Probs,V),pita:equality(Env,V,N,BDD))).

generate_rules_fact([Head:_P|T],Env,VC,R,Probs,N,[Clause|Clauses],Module):-
  add_bdd_arg(Head,Env,BDD,Module,Head1),
  Clause=(Head1:-(pita:get_var_n(Env,R,VC,Probs,V),pita:equality(Env,V,N,BDD))),
  N1 is N+1,
  generate_rules_fact(T,Env,VC,R,Probs,N1,Clauses,Module).


generate_rules_fact_db([],_Env,_VC,_R,_Probs,_N,[],_Module).

generate_rules_fact_db([Head:_P1,'':_P2],Env,VC,R,Probs,N,[Clause],Module):-!,
  add_bdd_arg_db(Head,Env,BDD,_DB,Module,Head1),
  Clause=(Head1:-(pita:get_var_n(Env,R,VC,Probs,V),pita:equality(Env,V,N,BDD))).

generate_rules_fact_db([Head:_P|T],Env,VC,R,Probs,N,[Clause|Clauses],Module):-
  add_bdd_arg_db(Head,Env,BDD,_DB,Module,Head1),
  Clause=(Head1:-(pita:get_var_n(Env,R,VC,Probs,V),pita:equality(Env,V,N,BDD))),
  N1 is N+1,
  generate_rules_fact_db(T,Env,VC,R,Probs,N1,Clauses,Module).


generate_clause(Head,Env,Body,_VC,_R,_Probs,_BDDAnd,_N,Clause,Module):-
  add_bdd_arg(Head,Env,_BDD,Module,Head1),
  Clause=(Head1:-Body).


generate_clause_db(Head,Env,Body,_VC,_R,_Probs,DB,_BDDAnd,_N,Clause,Module):-
  add_bdd_arg_db(Head,Env,_BDD,DBH,Module,Head1),
  Clause=(Head1:-(DBH>=1,DB is DBH-1,Body)).


generate_rules([],_Env,_Body,_VC,_R,_Probs,_BDDAnd,_N,[],_Module).

generate_rules([Head:_P1,'':_P2],Env,Body,VC,R,Probs,BDDAnd,N,[Clause],Module):-!,
  generate_clause(Head,Env,Body,VC,R,Probs,BDDAnd,N,Clause,Module).

generate_rules([Head:_P|T],Env,Body,VC,R,Probs,BDDAnd,N,[Clause|Clauses],Module):-
  generate_clause(Head,Env,Body,VC,R,Probs,BDDAnd,N,Clause,Module),
  N1 is N+1,
  generate_rules(T,Env,Body,VC,R,Probs,BDDAnd,N1,Clauses,Module).


generate_rules_db([],_Env,_Body,_VC,_R,_Probs,_DB,_BDDAnd,_N,[],_Module):-!.

generate_rules_db([Head:_P1,'':_P2],Env,Body,VC,R,Probs,DB,BDDAnd,N,[Clause],Module):-!,
  generate_clause_db(Head,Env,Body,VC,R,Probs,DB,BDDAnd,N,Clause,Module).

generate_rules_db([Head:_P|T],Env,Body,VC,R,Probs,DB,BDDAnd,N,[Clause|Clauses],Module):-
  generate_clause_db(Head,Env,Body,VC,R,Probs,DB,BDDAnd,N,Clause,Module),!,%agg.cut
  N1 is N+1,
  generate_rules_db(T,Env,Body,VC,R,Probs,DB,BDDAnd,N1,Clauses,Module).

process_body_bg([],[],_Module).

process_body_bg([\+ H|T],[\+ H|Rest],Module):-
  builtin(H),!,
  process_body_bg(T,Rest,Module).

process_body_bg([\+ H|T],[\+ H1|Rest],Module):-!,
  add_mod_arg(H,Module,H1),
  process_body_bg(T,Rest,Module).

process_body_bg([H|T],[H|Rest],Module):-
  builtin(H),!,
  process_body_bg(T,Rest,Module).

process_body_bg([H|T],[H1|Rest],Module):-!,
  add_mod_arg(H,Module,H1),
  process_body_bg(T,Rest,Module).



process_body([],BDD,BDD,Vars,Vars,[],_Env,_Module).

process_body([\+ H|T],BDD,BDD1,Vars,Vars1,[\+ H|Rest],Env,Module):-
  builtin(H),!,
  process_body(T,BDD,BDD1,Vars,Vars1,Rest,Env,Module).

process_body([\+ H|T],BDD,BDD1,Vars,Vars1,[
(\+ H1)|Rest],Env,Module):-
  add_mod_arg(H,Module,H1),
  process_body(T,BDD,BDD1,Vars,Vars1,Rest,Env,Module).

process_body([H|T],BDD,BDD1,Vars,Vars1,[H|Rest],Env,Module):-
  builtin(H),!,
  process_body(T,BDD,BDD1,Vars,Vars1,Rest,Env,Module).

process_body([H|T],BDD,BDD1,Vars,Vars1,
[H1|Rest],Env,Module):-
  add_mod_arg(H,Module,H1),
  process_body(T,BDD,BDD1,Vars,Vars1,Rest,Env,Module).

process_body_db([],BDD,BDD,_DB,Vars,Vars,[],_Env,_Module):-!.

process_body_db([\+ H|T],BDD,BDD1,DB,Vars,Vars1,[\+ H|Rest],Env,Module):-
  builtin(H),!,
  process_body_db(T,BDD,BDD1,DB,Vars,Vars1,Rest,Env,Module).

process_body_db([\+ H|T],BDD,BDD1,DB,Vars,Vars1,[
  (\+ H1)|Rest],Env,Module):-!,
  add_mod_arg(H,Module,H1),
  process_body_db(T,BDD,BDD1,DB,Vars,Vars1,Rest,Env,Module).

process_body_db([H|T],BDD,BDD1,DB,Vars,Vars1,[H|Rest],Env,Module):-
  builtin(H),!,
  process_body_db(T,BDD,BDD1,DB,Vars,Vars1,Rest,Env,Module).

process_body_db([H|T],BDD,BDD1,DB,Vars,Vars1,
[H1|Rest],Env,Module):-
  add_mod_arg(H,Module,H1),
  process_body_db(T,BDD,BDD1,DB,Vars,Vars1,Rest,Env,Module).



given(H):-
  lift_input_mod(M),
  functor(H,P,Ar),
  (M:input(P/Ar)).


given_cw(H):-
  lift_input_mod(M),
  functor(H,P,Ar),
  (M:input_cw(P/Ar)).


and_list([],B,B).

and_list([H|T],B0,B1):-
  and(B0,H,B2),
  and_list(T,B2,B1).

/*
or_list([H],_Env,H):-!.

or_list([H|T],Env,B):-
  or_list1(T,Env,H,B).


or_list1([],_Env,B,B).

or_list1([H|T],Env,B0,B1):-
  or(Env,B0,H,B2),
  or_list1(T,Env,B2,B1).
*/

/**
 * set_lift(+Parameter:atom,+Value:term) is det
 *
 * The predicate sets the value of a parameter
 * For a list of parameters see
 * https://github.com/friguzzi/cplint/blob/master/doc/manual.pdf or
 * http://ds.ing.unife.it/~friguzzi/software/cplint-swi/manual.html
 */
set_lift(M:Parameter,Value):-
  retract(M:local_setting(Parameter,_)),
  assert(M:local_setting(Parameter,Value)).

/**
 * setting_lift(+Parameter:atom,-Value:term) is det
 *
 * The predicate returns the value of a parameter
 * For a list of parameters see
 * https://github.com/friguzzi/cplint/blob/master/doc/manual.pdf or
 * http://ds.ing.unife.it/~friguzzi/software/cplint-swi/manual.html
 */
setting_lift(M:P,V):-
  M:local_setting(P,V).


difference([],_,[]).

difference([H|T],L2,L3):-
  member_eq(H,L2),!,
  difference(T,L2,L3).

difference([H|T],L2,[H|L3]):-
  difference(T,L2,L3).


member_eq(E,[H|_T]):-
  E==H,!.

member_eq(E,[_H|T]):-
  member_eq(E,T).





process_head(HeadList,M, GroundHeadList) :-
  ground_prob(HeadList), !,
  process_head_ground(HeadList,M, 0, GroundHeadList).

process_head(HeadList,_M, HeadList).


/* process_head_ground([Head:ProbHead], Prob, [Head:ProbHead|Null])
 * ----------------------------------------------------------------
 */
process_head_ground([Head:ProbHead],M, Prob, [Head:ProbHead1|Null]) :-!,
  ProbHead1 is ProbHead,
  ProbLast is 1 - Prob - ProbHead1,
  M:local_setting(epsilon_parsing, Eps),
  EpsNeg is - Eps,
  ProbLast > EpsNeg,
  (ProbLast > Eps ->
    Null = ['':ProbLast]
  ;
    Null = []
  ).

process_head_ground([Head:ProbHead|Tail], M, Prob, [Head:ProbHead1|Next]) :-
  ProbHead1 is ProbHead,
  ProbNext is Prob + ProbHead1,
  process_head_ground(Tail, M, ProbNext, Next).


ground_prob([]).

ground_prob([_Head:ProbHead|Tail]) :-
  ground(ProbHead), % Succeeds if there are no free variables in the term ProbHead.
  ground_prob(Tail).

get_probs([], []).

get_probs([_H:P|T], [P1|T1]) :-
  P1 is P,
  get_probs(T, T1).




generate_clauses([],_M,_N,C,C):-!.

generate_clauses([H|T],M,N,C0,C):-
  term_variables(H,V),
  gen_clause(H,M,N,N1,rule(_,[_:P|_],_,_),[(Head:-BA)]),!,  %agg.cut
  append(C0,[(Head,BA,V,P)],C1),
  generate_clauses(T,M,N1,C1,C).


gen_clause((H :- Body),_M,N,N,(H :- Body),[(H :- Body)]):-!.



gen_clause(rule(_R,HeadList,BodyList,Lit),_M,N,N1,
  rule(N,HeadList,BodyList,Lit),Clauses):-!,
% disjunctive clause with more than one head atom senza depth_bound
  process_body(BodyList,_BDD,BDDAnd,[],_Vars,BodyList1,Env,Module),
  list2and(BodyList1,Body1),
  get_probs(HeadList,Probs),
  generate_rules(HeadList,Env,Body1,[],N,Probs,BDDAnd,0,Clauses,Module),
  N1 is N+1.

gen_clause(def_rule(H,BodyList,Lit),_M,N,N,def_rule(H,BodyList,Lit),Clauses) :- !,%agg. cut
% disjunctive clause with a single head atom senza depth_bound con prob =1
  process_body(BodyList,_BDD,BDDAnd,[],_Vars,BodyList2,Env,Module),
  list2and(BodyList2,Body1),
  add_bdd_arg(H,Env,BDDAnd,Module,Head1),
  Clauses=[(Head1 :- Body1)].


generate_clauses_bg([],[]):-!.

generate_clauses_bg([H|T],[CL|T1]):-
  gen_clause_bg(H,CL),  %agg.cut
  generate_clauses_bg(T,T1).

gen_clause_bg(def_rule(H,BodyList,_Lit),Clauses) :-
% disjunctive clause with a single head atom e depth_bound
  process_body_bg(BodyList,BodyList2,Module),
  list2and(BodyList2,Body1),
  add_mod_arg(H,Module,Head1),
  Clauses=(Head1 :- Body1).


/**
 * builtin(+Goal:atom) is det
 *
 * Succeeds if Goal is an atom whose predicate is defined in Prolog
 * (either builtin or defined in a standard library).
 */
builtin(G):-
  builtin_int(G),!.

builtin_int(average(_L,_Av)).
builtin_int(G):-
  predicate_property(G,built_in).
builtin_int(G):-
  predicate_property(G,imported_from(lists)).
builtin_int(G):-
  predicate_property(G,imported_from(apply)).
builtin_int(G):-
  predicate_property(G,imported_from(nf_r)).
builtin_int(G):-
  predicate_property(G,imported_from(matrix)).
builtin_int(G):-
  predicate_property(G,imported_from(clpfd)).



term_expansion_int((Head :- Body),M, (_Clauses,[rule(R,HeadList,BodyList,true)])) :-
% disjunctive clause with a single head atom senza DB, con prob. diversa da 1
  ((Head:-Body) \= ((user:term_expansion(_,_) ):- _ )),
  (Head = ((_H:_);_);Head=(_H:_)), !,
  list2or(HeadListOr, Head),
  get_next_rule_number(M,R),
  process_head(HeadListOr,M, HeadList),
  list2and(BodyList, Body).


term_expansion_int((Head :- Body),_M,(Clauses,[def_rule(Head,BodyList,true)])) :-
% definite clause senza DB
  ((Head:-Body) \= ((user:term_expansion(_,_)) :- _ )),!,
  list2and(BodyList, Body),
  process_body(BodyList,BDD,BDDAnd,[],_Vars,BodyList2,Env,Module),
  append([pita:one(Env,BDD)],BodyList2,BodyList3),
  list2and(BodyList3,Body2),
  add_bdd_arg(Head,Env,BDDAnd,Module,Head1),
  Clauses=(Head1 :- Body2).


term_expansion_int(Head,M,(_,[rule(R,HeadList,[],true)])) :-
% disjunctive fact with a single head atom e prob. generiche, senza db
  (Head \= ((user:term_expansion(_,_)) :- _ )),
  Head=(_H:_), !,
  list2or(HeadListOr, Head),
  process_head(HeadListOr,M, HeadList),
  get_next_rule_number(M,R).

term_expansion_int(Head, _M,((Head1:-pita:one(Env,One)),[def_rule(Head,[],true)])) :-
% definite fact without db
  (Head \= ((user:term_expansion(_,_) ):- _ )),
  (Head\= end_of_file),!,
  add_bdd_arg(Head,Env,One,_Module,Head1).


test_no_area(TestSet,M,Prog,NPos,NNeg,LL,Results):-
%  S= user_output,
%  SA= user_output,
%  format(SA,"Fold;\tCLL;\t AUCROC;\t AUCPR~n",[]),
  %gtrace,
  test_folds(TestSet,M,Prog,Results,NPos,NNeg,LL).


/*
  ROC = c3{data:_{x:x, rows:[x-'ROC'|ROC0]},
    axis:_{x:_{min:0.0,max:1.0,padding:0.0,
        tick:_{values:[0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0]}},
           y:_{min:0.0,max:1.0,padding:_{bottom:0.0,top:0.0},
        tick:_{values:[0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0]}}}},
  PR = c3{data:_{x:x, rows:[x-'PR'|PR0]},
    axis:_{x:_{min:0.0,max:1.0,padding:0.0,
        tick:_{values:[0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0]}},
           y:_{min:0.0,max:1.0,padding:_{bottom:0.0,top:0.0},
        tick:_{values:[0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0]}}}}.
*/
test_folds(F,M,Prog,LG,NPos,NNeg,LL):-
  find_ex(F,M,Pos,Neg,NPos,NNeg),
  maplist(compute_prob_ex_pos(Prog,M),Pos,LGP),
  maplist(compute_prob_ex_neg(Prog,M),Neg,LGN),
  append(LGP,LGN,LG0),
  keysort(LG0,LG),
  foldl(ll(M),LG,0,LL).

ll(M,P- (\+ _),LL0,LL):-!,
  (P=:=1.0->
    M:local_setting(logzero,LZ),
    LL is LL0+LZ
  ;
    LL is LL0+ log(1-P)
  ).

ll(M,P- _,LL0,LL):-
  (P=:=0.0->
    M:local_setting(logzero,LZ),
    LL is LL0+LZ
  ;
    LL is LL0+ log(P)
  ).
 




find_ex(DB,M,Pos,Neg,NPos,NNeg):-
  M:local_setting(neg_ex,given),!,
  M:output(P/A),!,
  find_ex_pred([P/A],M,DB,[],Pos,[],Neg),
  length(Pos,NPos),
  length(Neg,NNeg).

find_ex(DB,M,Pos,Neg,NPos,NNeg):-
  M:local_setting(neg_ex,cw),
  M:output(P/A),!,
  find_ex_pred_cw([P/A],M,DB,[],Pos,[],Neg),
  length(Pos,NPos),
  length(Neg,NNeg).


find_ex_pred([],_M,_DB,Pos,Pos,Neg,Neg).

find_ex_pred([P/A|T],M,DB,Pos0,Pos,Neg0,Neg):-
  functor(At,P,A),
  find_ex_db(DB,M,At,Pos0,Pos1,Neg0,Neg1),
  find_ex_pred(T,M,DB,Pos1,Pos,Neg1,Neg).

find_ex_db([],_M,_At,Pos,Pos,Neg,Neg).

find_ex_db([H|T],M,At,Pos0,Pos,Neg0,Neg):-
  At=..[P|L],
  At1=..[P,H|L],
  findall(At1,M:At1,LP),
  findall(At1,M:neg(At1),LN),
  append([Pos0,LP],Pos1),
  append([Neg0,LN],Neg1),
  find_ex_db(T,M,At,Pos1,Pos,Neg1,Neg).


find_ex_pred_cw([],_M,_DB,Pos,Pos,Neg,Neg).

find_ex_pred_cw([P/A|T],M,DB,Pos0,Pos,Neg0,Neg):-
  functor(At,P,A),
  findall(Types,get_types(At,M,Types),LT),
  append(LT,LLT),
  remove_duplicates(LLT,Types1),
  find_ex_db_cw(DB,M,At,Types1,Pos0,Pos1,Neg0,Neg1),
  find_ex_pred_cw(T,M,DB,Pos1,Pos,Neg1,Neg).

get_types(At,_M,[]):-
  At=..[_],!.

get_types(At,M,Types):-
  M:modeh(_,At),
  At=..[_|Args],
  get_args(Args,Types).

get_types(At,M,Types):-
  M:modeh(_,HT,_,_),
  member(At,HT),
  At=..[_|Args],
  get_args(Args,Types).


get_args([],[]).

get_args([+H|T],[H|T1]):-!,
  get_args(T,T1).

get_args([-H|T],[H|T1]):-!,
  get_args(T,T1).

get_args([#H|T],[H|T1]):-!,
  get_args(T,T1).

get_args([-#H|T],[H|T1]):-!,
  get_args(T,T1).

get_args([H|T],[H|T1]):-
  get_args(T,T1).




get_constants([],_Mod,_M,[]).

get_constants([Type|T],Mod,M,[(Type,Co)|C]):-
  find_pred_using_type(Type,Mod,LP),
  find_constants(LP,Mod,M,[],Co),
  get_constants(T,Mod,M,C).

find_pred_using_type(T,M,L):-
  (setof((P,Ar,A),pred_type(T,M,P,Ar,A),L)->
    true
  ;
    L=[]
  ).

pred_type(T,M,P,Ar,A):-
  M:modeh(_,S),
  S=..[P|Args],
  length(Args,Ar),
  scan_args(Args,T,1,A).

pred_type(T,M,P,Ar,A):-
  M:modeb(_,S),
  S=..[P|Args],
  length(Args,Ar),
  scan_args(Args,T,1,A).

scan_args([+T|_],T,A,A):-!.

scan_args([-T|_],T,A,A):-!.

scan_args([#T|_],T,A,A):-!.

scan_args([-#T|_],T,A,A):-!.

scan_args([_|Tail],T,A0,A):-
  A1 is A0+1,
  scan_args(Tail,T,A1,A).

find_constants([],_Mod,_M,C,C).

find_constants([(P,Ar,A)|T],Mod,M,C0,C):-
  gen_goal(1,Ar,A,Args,ArgsNoV,V),
  G=..[P,M|Args],
  (setof(V,ArgsNoV^call_goal(Mod,G),LC)->
    true
  ;
    LC=[]
  ),
  append(C0,LC,C1),
  remove_duplicates(C1,C2),
  find_constants(T,Mod,M,C2,C).

call_goal(M,G):-
  M:G.

gen_goal(Arg,Ar,_A,[],[],_):-
  Arg =:= Ar+1,!.

gen_goal(A,Ar,A,[V|Args],ArgsNoV,V):-!,
  Arg1 is A+1,
  gen_goal(Arg1,Ar,A,Args,ArgsNoV,V).

gen_goal(Arg,Ar,A,[ArgV|Args],[ArgV|ArgsNoV],V):-
  Arg1 is Arg+1,
  gen_goal(Arg1,Ar,A,Args,ArgsNoV,V).



find_ex_db_cw([],_M,_At,_Ty,Pos,Pos,Neg,Neg).

find_ex_db_cw([H|T],M,At,Types,Pos0,Pos,Neg0,Neg):-
  get_constants(Types,M,H,C),
  At=..[P|L],
  get_types(At,M,TypesA),!,
  length(L,N),
  length(LN,N),
  At1=..[P,H|LN],
  findall(At1,M:At1,LP),
  (setof(At1,neg_ex(LN,TypesA,M,At1,C),LNeg)->true;LNeg=[]),
  append([Pos0,LP],Pos1),
  append([Neg0,LNeg],Neg1),
  find_ex_db_cw(T,M,At,Types,Pos1,Pos,Neg1,Neg).

neg_ex([],[],M,At1,_C):-
  \+ M:At1.

neg_ex([H|T],[HT|TT],M,At1,C):-
  member((HT,Co),C),
  member(H,Co),
  neg_ex(T,TT,M,At1,C).



compute_prob_ex_neg(Prog,M,H,PG- (\+ H)):-
  length(Prog,N),
  gen_initial_counts(N,MI0),
  test_theory_neg_prob([H],M,Prog,MI0,MI),
  compute_prob_ex(Prog,MI,1,PG0),
  PG is 1-PG0.

compute_prob_ex_pos(Prog,M,H,PG- H):-
  length(Prog,N),
  gen_initial_counts(N,MI0),
  test_theory_neg_prob([H],M,Prog,MI0,MI),
  compute_prob_ex(Prog,MI,1,PG0),
  PG is 1-PG0.

compute_prob_ex([],[],PG,PG).

compute_prob_ex([(_,_,_,P)|R],[MIH|MIT],PG0,PG):-
  PG1 is PG0*(1-P)^MIH,
  compute_prob_ex(R,MIT,PG1,PG).

writes([H-H1],S):-
  format(S,"~f - (~q)]).~n~n",[H,H1]).

writes([H-H1|T],S):-
  format(S,"~f - (~q),~n",[H,H1]),
  writes(T,S).


write_p(P,S):-
  get_xy(P,PX,PY),
  format(S,"x=[",[]),
  writesf(PX,S),
  format(S,"y=[",[]),
  writesf(PY,S),
  format(S,"
figure('Name','roc','NumberTitle','off')
set(gca,'XLim',[0.0 1.0])
set(gca,'YLim',[0.0 1.0])
x=[x 1.0]
y=[y 0.0]
k=convhull(x,y)
plot(x(k),y(k),'r-',x,y,'--b+')
%A = polyarea(x,y)~n~n
%save area_roc.csv  A -ascii -append
",
  []).

get_xy([],[],[]).

get_xy([X-Y|T],[X|TX],[Y|TY]):-
  get_xy(T,TX,TY).


writesf([H],S):-
  format(S,"~f]~n",[H]).

writesf([H|T],S):-
  format(S,"~f ",[H]),
  writesf(T,S).

write_ppr(P,S):-
  get_xy(P,PX,PY),
  format(S,"rec=[",[A]),
  writesf(PX,S),
  format(S,"prec=[",[A]),
  writesf(PY,S),
  format(S,"
figure('Name','pr','NumberTitle','off')
set(gca,'XLim',[0.0 1.0])
set(gca,'YLim',[0.0 1.0])
rec=[0.0  rec 1.0];
prec=[0.0 prec 0.0];
plot(rec,prec,'--*k')
%A=polyarea(rec,prec)
%save area_pr.csv  A -ascii -append
~n~n",
  []).

/**
 * write2(+Module:atom,+Message:term) is det
 *
 * The predicate calls write(Message) if the verbosity is at least 2.
 * Module is used to get the verbosity setting
 */
write2(M,A):-
  M:local_setting(verbosity,Ver),
  (Ver>1->
    write(A)
  ;
    true
  ).
/**
 * write3(+Module:atom,+Message:term) is det
 *
 * The predicate calls write(Message) if the verbosity is at least 3.
 * Module is used to get the verbosity setting.
 */
write3(M,A):-
  M:local_setting(verbosity,Ver),
  (Ver>2->
    write(A)
  ;
    true
  ).

/**
 * write4(+Module:atom,+Message:term) is det
 *
 * The predicate calls write(Message) if the verbosity is at least 4.
 * Module is used to get the verbosity setting.
 */
write4(M,A):-
  M:local_setting(verbosity,Ver),
  (Ver>3->
    write(A)
  ;
    true
  ).
/**
 * nl2(+Module:atom) is det
 *
 * The predicate prints a newline if the verbosity is at least 2.
 * Module is used to get the verbosity setting.
 */
nl2(M):-
  M:local_setting(verbosity,Ver),
  (Ver>1->
    nl
  ;
    true
  ).
/**
 * nl3(+Module:atom) is det
 *
 * The predicate prints a newline if the verbosity is at least 3.
 * Module is used to get the verbosity setting.
 */
nl3(M):-
  M:local_setting(verbosity,Ver),
  (Ver>2->
    nl
  ;
    true
  ).

/**
 * nl4(+Module:atom) is det
 *
 * The predicate prints a newline if the verbosity is at least 4.
 * Module is used to get the verbosity setting.
 */
nl4(M):-
  M:local_setting(verbosity,Ver),
  (Ver>3->
    nl
  ;
    true
  ).
/**
 * format2(+Module:atom,+Format, :Arguments) is det
 *
 * The predicate calls format(Format,Arguments) if the verbosity is at least 2.
 * Module is used to get the verbosity setting.
 */
format2(M,A,B):-
  M:local_setting(verbosity,Ver),
  (Ver>1->
    format(A,B)
  ;
    true
  ).
/**
 * format3(+Module:atom,+Format, :Arguments) is det
 *
 * The predicate calls format(Format,Arguments) if the verbosity is at least 3.
 * Module is used to get the verbosity setting.
 */
format3(M,A,B):-
  M:local_setting(verbosity,Ver),
  (Ver>2->
    format(A,B)
  ;
    true
  ).
/**
 * format4(+Module:atom,+Format, :Arguments) is det
 *
 * The predicate calls format(Format,Arguments) if the verbosity is at least 4.
 * Module is used to get the verbosity setting.
 */
format4(M,A,B):-
  M:local_setting(verbosity,Ver),
  (Ver>3->
    format(A,B)
  ;
    true
  ).
/**
 * write_rules2(+Module:atom,+Rules:list,+Stream:atom) is det
 *
 * The predicate write the rules in Rules on stream Stream if the verbosity is at least 2.
 * Module is used to get the verbosity setting.
 */
write_rules2(M,A,B):-
  M:local_setting(verbosity,Ver),
  (Ver>1->
    write_rules(A,B)
  ;
    true
  ).
/**
 * write_rules3(+Module:atom,+Rules:list,+Stream:atom) is det
 *
 * The predicate write the rules in Rules on stream Stream if the verbosity is at least 3.
 * Module is used to get the verbosity setting.
 */
write_rules3(M,A,B):-
  M:local_setting(verbosity,Ver),
  (Ver>2->
    write_rules(A,B)
  ;
    true
  ).


write_disj_clause2(M,A,B):-
  M:local_setting(verbosity,Ver),
  (Ver>1->
    write_disj_clause(A,B)
  ;
    true
  ).

write_disj_clause3(M,A,B):-
  M:local_setting(verbosity,Ver),
  (Ver>2->
    write_disj_clause(A,B)
  ;
    true
  ).

write_body2(M,A,B):-
  M:local_setting(verbosity,Ver),
  (Ver>1->
    write_body(A,B)
  ;
    true
  ).

write_body3(M,A,B):-
  M:local_setting(verbosity,Ver),
  (Ver>2->
    write_body(A,B)
  ;
    true
  ).




lift_expansion((:- begin_bg), []) :-
  prolog_load_context(module, M),
  lift_input_mod(M),!,
  assert(M:bg_on).

lift_expansion(C, M:bgc(C)) :-
  prolog_load_context(module, M),
  C\= (:- end_bg),
  lift_input_mod(M),
  M:bg_on,!.

lift_expansion((:- end_bg), []) :-
  prolog_load_context(module, M),
  lift_input_mod(M),!,
  retractall(M:bg_on),
  findall(C,M:bgc(C),L),
  retractall(M:bgc(_)),
  (M:bg(BG0)->
    retract(M:bg(BG0)),
    append(BG0,L,BG),
    assert(M:bg(BG))
  ;
    assert(M:bg(L))
  ).






lift_expansion((:- begin_in), []) :-
  prolog_load_context(module, M),
  lift_input_mod(M),!,
  assert(M:in_on).

lift_expansion(C, M:inc(C)) :-
  prolog_load_context(module, M),
  C\= (:- end_in),
  lift_input_mod(M),
  M:in_on,!.

lift_expansion((:- end_in), []) :-
  prolog_load_context(module, M),
  lift_input_mod(M),!,
  retractall(M:in_on),
  findall(C,M:inc(C),L),
  retractall(M:inc(_)),
  (M:in(IN0)->
    retract(M:in(IN0)),
    append(IN0,L,IN),
    assert(M:in(IN))
  ;
    assert(M:in(L))
  ).

lift_expansion(begin(model(I)), []) :-
  prolog_load_context(module, M),
  lift_input_mod(M),!,
  retractall(M:model(_)),
  assert(M:model(I)),
  assert(M:int(I)).

lift_expansion(end(model(_I)), []) :-
  prolog_load_context(module, M),
  lift_input_mod(M),!,
  retractall(M:model(_)).

lift_expansion(At, A) :-
  prolog_load_context(module, M),
  lift_input_mod(M),
  M:model(Name),
  At \= (_ :- _),
  At \= end_of_file,
  (At=neg(Atom)->
    Atom=..[Pred|Args],
    Atom1=..[Pred,Name|Args],
    A=neg(Atom1)
  ;
    (At=prob(Pr)->
      A='$prob'(Name,Pr)
    ;
      At=..[Pred|Args],
      Atom1=..[Pred,Name|Args],
      A=Atom1
    )
  ).

:- multifile sandbox:safe_meta/2.

sandbox:safe_meta(liftcover:induce_par_lift(_,_) ,[]).
sandbox:safe_meta(liftcover:induce_lift(_,_), []).
sandbox:safe_meta(liftcover:test_prob_lift(_,_,_,_,_,_), []).
sandbox:safe_meta(liftcover:test_lift(_,_,_,_,_,_,_), []).
sandbox:safe_meta(liftcover:set_lift(_,_), []).
sandbox:safe_meta(liftcover:setting_lift(_,_), []).
sandbox:safe_meta(liftcover:filter_rules(_,_), []).

:- multifile sandbox:safe_primitive/1.
sandbox:safe_primitive(liftcover:filter_rules(_,_,_), []).
sandbox:safe_primitive(liftcover:sort_rules(_,_,_), []).



:- thread_local lift_file/1.


user:term_expansion((:- lift), []) :-!,
  prolog_load_context(module, M),
  prolog_load_context(source, Source),
  asserta(lift_file(Source)),  
%  retractall(input_mod(_)),
%  M:dynamic(model/1),
%  M:set_prolog_flag(unkonw,fail),
  retractall(M:local_setting(_,_)),
  findall(local_setting(P,V),default_setting_lift(P,V),L),
  assert_all(L,M,_),
  assert(lift_input_mod(M)),
  retractall(M:rule_lift_n(_)),
  assert(M:rule_lift_n(0)),
  M:dynamic((modeh/2,modeh/4,fixed_rule/3,banned/2,lookahead/2,
    lookahead_cons/2,lookahead_cons_var/2,prob/2,output/1,input/1,input_cw/1,
    ref_clause/1,ref/1,model/1,neg/1,rule/4,determination/2,
    bg_on/0,bg/1,bgc/1,in_on/0,in/1,inc/1,int/1)),
  style_check(-discontiguous).

user:term_expansion(end_of_file, C) :-
  lift_file(Source),
  prolog_load_context(source, Source),
  retractall(lift_file(Source)),
  prolog_load_context(module, M),
  lift_input_mod(M),!,
  retractall(lift_input_mod(M)),
  make_dynamic(M),
%  retractall(M:tabled(_)),
  %retractall(lift_input_mod(M)),
  C=[(:- style_check(+discontiguous)),end_of_file].

user:term_expansion(In, Out) :-
  \+ current_prolog_flag(xref, true),
  lift_file(Source),
  prolog_load_context(source, Source),
  lift_expansion(In, Out).






%

