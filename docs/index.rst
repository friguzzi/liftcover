.. phil documentation master file, created by
   sphinx-quickstart on Thu May 30 15:47:04 2019.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

===========================================
LIFTCOVER Manual 
===========================================

.. toctree::
	:maxdepth: 2


Introduction
============
LIFTCOVER is a system for learning simple probabilistic logic programs with scalability in mind
 :cite:`NguRig19-ML-IJ`. 

Installation
============
LIFTCOVER is distributed as a `pack <http://www.swi-prolog.org/pack/list?p=liftcover>`_ of
 `SWI-Prolog <http://www.swi-prolog.org/>`_. 
To install it, use ::

    $ swipl
    ?- pack_install(liftcover).

Requirements
-------------
It requires the packs

 * `lbfgs <https://github.com/friguzzi/lbfgs>`_
 * `cplint <https://github.com/friguzzi/cplint>`_
 
They are installed automatically when installing pack `liftcover` or can be installed manually as follows ::

	$ swipl
	?- pack_install(lfbgs).
	?- pack_install(cplint).


You can upgrade the pack with ::

    $ swipl
    ?- pack_upgrade(liftcovver).

Note that the packs on which `liftcover` depends are not upgraded automatically. 
In this case, they need to be upgraded manually.

Example of use
---------------
::

    $ cd <pack>/liftcover/prolog/examples
    $ swipl
    ?- [muta].
    ?- induce_par_lift([1,2,3,4,5,6,7,8,9],P).

Testing the installation
------------------------
::

    $ swipl
    ?- [library(test_liftcover)].
    ?- test.



Syntax 
==================

LIFTCOVER learns *liftable probabilistic logic programs* a restricted version of Logic programs with annotated disjunctions LPADs (:cite:`VenVer03-TR,VenVer04-ICLP04-IC`). A HPLP is a set of annotated clauses whose head contain a single atom annotated with a probability. For the rest, the usual syntax of Prolog is used. 

A clause in such a program has a single head with the *target predicate* (the predicate you want to learn)
and a body composed of *input predicates* or predicates defined by deterministic clauses (typically facts).

A general HPLP clause has the following form: ::

    h:p:- b1,b2...,bn.


The following example inspired from the UWCSE dataset used in :cite:`kok2005learning` is represented as (file `uwcse.pl <http://cplint.eu/e/phil/uwcse.pl>`__) ::

   advisedby(A,B):0.3 :-student(A),professor(B),project(A,C),project(B,C).
   advisedby(A,B):0.6 :-student(A),professor(B),ta(C,A),taughtby(C,B).

   student(harry). professor(ben). 
   project(harry,pr1). project(harry,pr2).
   project(ben,pr1). project(ben,pr2). 
   taughtby(c1,ben). taughtby(c2,ben).
   ta(c_1,harry). ta(c_2,harry).

where :code:`publication(A,B,C)` means that A is a publication with author B produced in project C.
:code:`advisedby/2` is the target predicate and :code:`student/1, professor/1, project/2, ta/2, taughtby/2` 
are input predicates.

The first clause states that a student A is advised by a professor B with probability 0.3 if they both
 work on the same project C.  
The second states that a student A is advised by a professor B with probability 0.6 if the student
 is a teacher assistant in a course taught by the professor. 

The facts in the program state that harry is a student and ben a professor.
 They have two joint courses c1 and c2, two joint projects pr1 and pr2.


Semantics
==================
The semantics of liftable PLP directly inherits the semantics of LPADs.



Learning
==================

Input
-----
To execute the learning algorithms, prepare a Prolog file divided in five parts

- preamble
- background knowledge, i.e., knowledge valid for all interpretations
- liftable PLP for which you want to learn the parameters (optional)
- language bias information
- example interpretations

The preamble must come first, the order of the other parts can be changed.

For example, consider the Bongard problems of :cite:`RaeLae95-ALT95`. `bongard.pl <http://cplint.eu/e/lift/bongard.pl>`__ and `bongardkeys.pl <http://cplint.eu/e/lift/bongardkeys.pl>`__ represent a Bongard problem for LIFTCOVER. 


Preamble
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
In the preamble, the LIFTCOVER library is loaded with (`bongard.pl <http://cplint.eu/e/lift/bongard.pl>`__): ::

	:- use_module(library(liftcover)).

Now you can initialize  with ::

	:- lift.

At this point you can start setting parameters for SLEAHP such as for example ::

	:- set_lift(megaex_bottom,10).
	:- set_lift(min_probability,0.00001).
	:- set_lift(verbosity,1).

We will later see the list of available parameters.


Background and Initial hierarchical program
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Now you can specify the background knowledge with a fact of the form ::

	bg(<list of terms representing clauses>).

where the clauses must be deterministic. 
Alternatively, you can specify a set of clauses by including them in a section between :code:`:- begin_bg.` and :code:`:- end_bg.` 
Moreover, you can specify an initial program with a fact of the form ::

	in(<list of terms representing clauses>).

The initial program is used in parameter learning for providing the structure. 
Remember to enclose each clause in parentheses because :code:`:-` has the highest precedence.

For example, `bongard.pl <http://cplint.eu/e/lift/bongard.pl>`__ has the initial program ::
	
    in([(pos:0.197575 :-
       circle(A),
       in(B,A)),
    (pos:0.000303421 :-
       circle(A),
       triangle(B)), 
    (pos:0.000448807 :-
       triangle(A),
       circle(B))]).

Alternatively, you can specify an input program in a section between :code:`:- begin_in.` and :code:`:- end_in.` as for example ::

	:- begin_in.

	pos:0.197575 :-
		circle(A),
		in(B,A).
	pos:0.000303421 :-
		circle(A),
		triangle(B).
	pos:0.000448807 :-
		triangle(A),
		circle(B).
	
	:- end_in.

If you specify both a :code:`in/1` fact and a section, the clauses of the two will be combined.

The annotations of the head atoms of the initial program can be probabilities, as in the example above. In parameter learning, the learning procedure can start with the initial parameters in the program. In this case, it is up to the user to ensure that there are values between 0 and 1. In the case of structure learning, the initial program is not necessary. 


Language Bias
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
The language bias part contains the declarations of the input and output predicates.
The output predicate is declared as ::

	output(<predicate>/<arity>).

and indicates the predicate whose atom you want to predict (target predicate). 
Derivations for the atoms for this predicate in the input data is built by the system. 
Input predicates are those whose atoms you are not interested in predicting. 
You can declare input predicates with ::

	input(<predicate>/<arity>).

For these predicates, the only true atoms are those in the interpretations and those derivable from them using the background knowledge.

Then, for structure learning you have to specify the language bias by means of mode declarations in the style of `Progol <https://www.doc.ic.ac.uk/~shm/progol.html>`_. ::

	modeh(<recall>,<predicate>(<arg1>,...)).

specifies the atoms that can appear in the head of clauses.  ::

	modeb(<recall>,<predicate>(<arg1>,...)).

specifies the atoms that can appear in the body of clauses. :code:`<recall>` can be an integer or :code:`*`. :code:`<recall>` indicates how many atoms for the predicate specification are retained in the bottom clause during a saturation step. :code:`*` stands for all those that are found. 
Otherwise the indicated number of atoms are randomly chosen.


Arguments of the form ::

	+<type>

specifies that the argument should be an input variable of type :code:`<type>`, i.e., a variable replacing a :code:`+<type>` argument in the head or a :code:`-<type>` argument in a preceding literal in the current hypothesized clause.

Another argument form is ::

	-<type>

for specifying that the argument should be a output variable of type :code:`<type>`. 
Any variable can replace this argument, either input or output. 
The only constraint on output variables is that those in the head of a clause must appear as output variables in an atom in the body.

Other forms are ::

	#<type>

for specifying an argument which should be replaced by a constant of type :code:`<type>` in the bottom clause but should not be used for replacing input variables of the following literals when building the bottom clause or ::

	-#<type>

for specifying an argument which should be replaced by a constant of type :code:`<type>` in the bottom clause and that should be used for replacing input variables of the following literals when building the bottom clause. ::

	<constant>

for specifying a constant.


An example of language bias for the Bongard domain is ::

	output(pos/0).

	input(triangle/1).
	input(square/1).
	input(circle/1).
	input(in/2).
	input(config/2).

	modeh(*,pos).
	modeb(*,triangle(-obj)).
	modeb(*,square(-obj)).
	modeb(*,circle(-obj)).
	modeb(*,in(+obj,-obj)).
	modeb(*,in(-obj,+obj)).
	modeb(*,config(+obj,-#dir)).

LIFTCOVER also requires facts for the :code:`determination/2` Aleph-style predicate that indicate which predicates can appear in the body of clauses. 
For example ::

	determination(pos/0,triangle/1).
	determination(pos/0,square/1).
	determination(pos/0,circle/1).
	determination(pos/0,in/2).
	determination(pos/0,config/2).


state that :code:`triangle/1` can appear in the body of clauses for :code:`pos/0`.



Example Interpretations
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
The last part of the file contains the data. 
You can specify data with two modalities: models and keys. 
In the models case, you specify an example model (or interpretation or mega-example) as a list of Prolog facts initiated by :code:`begin(model(<name>)).` and terminated by :code:`end(model(<name>)).` as in ::

	begin(model(2)).
	pos.
	triangle(o5).
	config(o5,up).
	square(o4).
	in(o4,o5).
	circle(o3).
	triangle(o2).
	config(o2,up).
	in(o2,o3).
	triangle(o1).
	config(o1,up).
	end(model(2)).


The facts in the interpretation are loaded in SWI-Prolog database by adding an extra initial argument equal to the name of the model. 
After each interpretation is loaded, a fact of the form :code:`int(<id>)` is asserted, where :code:`id` is the name of the interpretation. 
This can be used in order to retrieve the list of interpretations.

Alternatively, with the keys modality, you can directly write the facts and the first argument will be interpreted as a model identifier. 
The above interpretation in the keys modality is ::

	pos(2).
	triangle(2,o5).
	config(2,o5,up).
	square(2,o4).
	in(2,o4,o5).
	circle(2,o3).
	triangle(2,o2).
	config(2,o2,up).
	in(2,o2,o3).
	triangle(2,o1).
	config(2,o1,up).

which is contained in the `bongardkeys.pl <http://cplint.eu/e/lift/bongardkeys.pl>`_. 
This is also how model :code:`2` above is stored in SWI-Prolog database. 
The two modalities, models and keys, can be mixed in the same file. 
Facts for :code:`int/1` are not asserted for interpretations in the key modality but can be added by the user explicitly.

Note that you can add background knowledge that is not probabilistic directly to the file writing clauses taking into account the model argument. 
For example (:code:`carc.pl`), contains ::

	connected(_M,Ring1,Ring2):-
		Ring1 \= Ring2,
		member(A,Ring1),
		member(A,Ring2), !.

	symbond(Mod,A,B,T):- bond(Mod,A,B,T).
	symbond(Mod,A,B,T):- bond(Mod,B,A,T).

where the first argument of all atoms is the model.

Then you must indicate how examples are divided in folds with facts of the form: :code:`fold(<fold_name>,<list of model identifiers>)`, as for example ::

	fold(train,[2,3,...]).
	fold(test,[490,491,...]).

As the input file is a Prolog program, you can define intentionally the folds as in ::

	fold(all,F):-
  	findall(I,int(I),F).

:code:`fold/2` is dynamic so you can also write ::

	:- fold(all,F),
    	sample_lift(4,F,FTr,FTe),
      	assert(fold(rand_train,FTr)),
      	assert(fold(rand_test,FTe)).

which however must be inserted after the input interpretations otherwise the facts for :code:`int/1` will not be available and the fold :code:`all` would be empty. 
This command uses :code:`sample_lift(N,List,Sampled,Rest)` exported from :code:`liftcover` 
that samples :code:`N` elements from :code:`List` and returns the sampled elements in :code:`Sampled` and the rest in :code:`Rest`. 
If :code:`List` has :code:`N` elements or less, :code:`Sampled` is equal to :code:`List` and :code:`Rest` is empty.

Commands
--------

Parameter Learning
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
To execute PHIL, prepare an input file as indicated above and call ::

	?- induce_hplp_par(+List_of_folds:list,-P:list).

where :code:`<list of folds>` is a list of the folds for training and :code:`P` will contain the input program with updated parameters.

For example `bongard.pl <http://cplint.eu/e/phil/bongard.pl>`__, you can perform parameter learning on the :code:`train` fold with ::

	?- induce_hplp_par([train],P).

Structure Learning
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
To execute SLEAHP, prepare an input file in the editor panel as indicated above and call ::

	?- induce_hplp(+List_of_folds:list,-P:list).

where :code:`List_of_folds` is a list of the folds for training and :code:`P` will contain the learned program.

For example `bongard.pl <http://cplint.eu/e/phil/bongard.pl>`__, you can perform structure learning on the :code:`train` fold with ::

	?- induce_hplp([train],P).

A program can also be tested on a test set with :code:`test_hplp/7`  as described below.

Between two executions of :code:`induce_hplp/2` you should exit SWI-Prolog to have a clean database.



Testing
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
A program can also be tested on a test set in SLEAHP with ::

	test_hplp(+Program:list,+List_of_folds:list,-LL:float,-AUCROC:float,-ROC:list,-AUCPR:float,-PR:list) is det

where :code:`Program` is a list of terms representing clauses and :code:`List_of_folds` is a list of folds.

:code:`test_hplp/7` returns the log likelihood of the test examples in :code:`LL`, the Area Under the ROC curve in :code:`AUCROC`, a dictionary containing the list of points (in the form of Prolog pairs :code:`x-y`) of the ROC curve in :code:`ROC`, the Area Under the PR curve in :code:`AUCPR`, a dictionary containing the list of points of the PR curve in :code:`PR`.


Then you can draw the curves in :code:`phil` using C3.js as follows ::

	compute_areas_diagrams(+ExampleList:list,-AUCROC:float,-ROC:dict,-AUCPR:float,-PR:dict) is det

(from pack `auc.pl <http://www.swi-prolog.org/pack/list?p=auc>`_) that takes as input a list :code:`ExampleList` of pairs probability-literal.

For example, to test on fold :code:`test` the program learned on fold :code:`train` you can run the query ::

	?- induce_hplp_par([train],P),
  	test_hplp(P,[test],LL,AUCROC,ROC,AUCPR,PR).

Or you can test the input program on the fold :code:`test` with ::

	?- in(P),test_hplp(P,[test],LL,AUCROC,ROC,AUCPR,PR).

In :code:`phil` on SWISH, by including ::

	:- use_rendering(c3).
	:- use_rendering(lpad).

in the code before :code:`:- phil.` the curves will be shown as graphs using C3.js and the output program will be pretty printed.

Hyper-parameters for Learning
-----------------------------
Hyper-parameters are set with commands of the form ::

	:- set_hplp(<parameter>,<value>).

They are available both for PHIL and SLEAHP:

* PHIL hyper-parameters

    - :code:`algorithmType` (values: :code:`{dphil,emphil}`, default value: :code:`dphil`): defines the parameter learning algorithm.
    - :code:`maxIter_phil` (values: integer, default value: 1000): maximum number of iteration of PHIL. If set to -1, no maximum number of iterations is imposed.
    - :code:`epsilon_deep` (values: real, default value: 0.0001): if the difference in the log likelihood in two successive PHIL (DPHIL and EMPHIL) iteration is smaller than :code:`epsilon_deep`, then PHIL stops.
    - :code:`epsilon_deep_fraction` (values: real, default value: 0.00001): if the difference in the log likelihood in two successive PHIL iteration is smaller than :code:`epsilon_deep_fraction*(-current log likelihood)`, then PHIL stops.
    - :code:`seed` (values: seed(integer) or seed(random), default value :code:`seed(3032)`): seed for the Prolog random functions, see `SWI-Prolog manual <http://www.swi-prolog.org/pldoc/man?predicate=set_random/1>`__ .
    - :code:`setSeed` (values: :code:`{no,yes}`, default value: :code:`no`): if :code:`yes`, the value of :code:`c_seed` is used as seed for randomly initializing the probabilities in PHIL  otherwise the actual clock value is used.
    - :code:`c_seed` (values: unsigned integer, default value 21344)): seed for the C random functions if :code:`setSeed` is set to :code:`yes`.
    - :code:`useInitParams` (values: :code:`{no,yes}`, default value: :code:`no`): if :code:`yes` the initial parameters during the learning are the ones indicated in the program. Otherwise, the initial parameters are generated randomly using the clock seed.
    - :code:`logzero` (values: negative real, default value :math:`\log(0.000001)`): value assigned to :math:`\log(0)`.
    - :code:`zero` (values: real, default value :math:`0.000001`): value assigned to :math:`0`.
    - :code:`max_initial_weight` (values: real number , default value: 0.5): weights in DPHIL are randomly initialized with values in the interval [-max_initial_weight, max_initial_weight].
    - :code:`adam_params` (values: list of four values [learningRate, beta1, beta2, epsilon], default value: [0.001,0.9,0.999,1e-8]): provides Adam's hyper-parameters.
    - :code:`batch_strategy` (values: :code:`minibatch(size)`, :code:`stoch_minibatch(size)`, :code:`batch`, default value: :code:`minibatch(10)`): defines the gradient descent strategy in DPHIL. If set to :code:`minibatch(size)` the mini batch strategy with batch size :code:`size` is used i.e at each iteration, :code:`size` arithmetic circuits (ACs) are used to compute the gradients. If set to :code:`batch` the whole set of ACs are used to compute the gradients. :code:`stoch_minibatch(size)` is a modified version of mini batch in which at each iteration, :code:`size`  ACs are randomly selected for computing the gradients.
    - :code:`saveStatistics` (values: :code:`{no,yes}`, default value: :code:`no`): if :code:`yes`, the statistics (evolution of the likelihood and other values) during parameter learning are saved in a folder whose name is the value of :code:`statistics_folder` hyper-parameter. 
    - :code:`statistics_folder` (values: string , default value: :code:`statistics`): folder where to save the statistics if :code:`saveStatistics` is set to :code:`yes`.
    - :code:`regularized` (values: :code:`{no,yes}` , default value: :code:`no`): if set to :code:`yes` regularization is enabled. 
    - :code:`regularizationType` (values: integer in :code:`[0,3]` , default value: :code:`0`):  :code:`0` to disable regularization, :code:`1`, :code:`2` and :code:`3` for L1, L2 and Bayesian regularization respectively.
    - :code:`gamma` (values: real number, default value: :code:`10`): regularization coefficient for L1 and L2. Is equal to the Dirichlet coefficient :code:`a` for Bayesian regularization. 
    - :code:`gammaCount` (values:real number , default value: 0): Dirichlet coefficient :code:`b` for Bayesian regularization and typically 0 for L1 and l2 regularization.

* SLEAHP hyper-parameters

    - :code:`megaex_bottom` (values: integer, default value: 1, valid for SLEAHP): number of mega-examples on which to build the bottom clauses.
    - :code:`initial_clauses_per_megaex` (values: integer, default value: 1, valid for SLEAHP): number of bottom clauses to build for each mega-example (or model or interpretation).
    - :code:`d` (values: integer, default value: 1, valid for SLEAHP): number of saturation steps when building the bottom clause.
    - :code:`neg_ex` (values: :code:`given`, :code:`cw`, default value: :code:`cw`): if set to :code:`given`, the negative examples in training and testing are taken from the test folds interpretations, i.e., those examples :code:`ex` stored as :code:`neg(ex)`; if set to :code:`cw`, the negative examples in training and testing are generated according to the closed world assumption, i.e., all atoms for target predicates that are not positive examples. The set of all atoms is obtained by collecting the set of constants for each type of the arguments of the target predicate, so the target predicates must have at least one fact for :code:`modeh/2` or :code:`modeb/2` also for parameter learning.
    - :code:`probability` (values:real number in :code:`[0,1]` , default value: :code:`1.0`): initial value which indicates the probability to go from the current layer to the next when generating the initial HPLP.
    - :code:`rate` (values: real number in :code:`[0,1]`, default value: :code:`0.95`): at each layer during the tree generation, the probability to go deeper is equal to :code:`probability=probability*rate`. The deeper the layer the lower is the probability to go deeper.
    - :code:`min_probability` (values: real number in :code:`[0,1]`, default value: :code:`1e-5`):  probability threshold under which a clause is dropped out.
    - :code:`use_all_mega_examples` (values: :code:`{no,yes}` , default value::code:`yes`): if set to :code:`yes` all the the bottom clauses are used to create the tree otherwise only one of them is used (typically the first).
    - :code:`saveHPLP` (values: :code:`{no,yes}`, default value: :code:`no`): if set to :code:`yes` the initial and the learned hierarchical programs are saved in the current folder.
    - :code:`saveFile` (values: string, default value: "hplp"): file name where to save the initial hierarchical programs if :code:`saveHPLP`  is set to :code:`yes`. If the file name is :code:`hplp`, the learned program is saved in file :code:`hplp_learned`.
    - :code:`max_layer` (values: integer, default value: -1): maximum layer of clauses. If set to -1, no maximum is imposed.
    - :code:`verbosity` (values: integer in :code:`[1,3]`, default value: :code:`1`): level of verbosity of the algorithms.

	

Example Files
=============
The :code:`pack/phil/prolog/examples` folder in SWI-Prolog home contains some example programs. 
The subfolder :code:`learning` contains some learning examples.
The :code:`pack/phil/docs` folder contains this manual in latex, html and pdf.

Manual in PDF
==================
A PDF version of the manual is available at `https://arnaudfadja.github.io/phil/_build/latex/phil.pdf <https://arnaudfadja.github.io/phil/_build/latex/phil.pdf>`_.

License
=======
phil follows the Artistic License 2.0 that you can find in phil root folder. 
The copyright is by Arnaud Nguembang Fadja.

References
==========
.. bibliography:: newbib.bib
