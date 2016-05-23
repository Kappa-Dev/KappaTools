########################
The Kappa Tutorial, v0.2
########################
Kappa is a language to write computational models; it is intended for
biochemical signaling cascades, but not constrained to that
use-case. Currently, there are two main platforms to develop and run
Kappa-based models:

* Command-line binaries & script files
* Browser-based proto-IDE_

Serious use of the tools require to install them. Procedure to achieve
that is detailled in second part. Let's begin by the creation of a
model and some analysis of it. the proto-IDE_ is enouth for that
prupose and first playing with your own script.

****************
The Kappa Script
****************
Kappa script files customarily have the ``.ka`` file extension, e.g.:
``foo.ka``. Each line of code is treated independently, so the order of
the instructions doesn't matter (except for variables, who must be
declared prior to being used). While KaSim will not complain about
the structure of the code, your advisor might: understandable scripts
do have a structure. Moreover, they have comments and explanations. In
Kappa, there are 2 kinds of comments:

:line comments: start with the pound symbol ``#`` and ènd with the end
  of the line
:block comments: start by ``/*`` and end by ``*/``

You can split your model into different files, and tell the simulator
to read them in order. You can also put everything on the same
file. This last one is the usage will be following here.

Roughly speaking, a Kappa script has the following sections:

:Variables: These are symbols that represent a value used during the
            simulation, e.g. the binding rates, the volume, some
            complicated algebraic expression.
:Agent Signatures: Here we define the agents and tokens in our
                   model. An agent (e.g. a protein) has sites it uses
                   to bind and/or to signal it's state (e.g. site Y3
                   in state phosphorylated).
:Rules: These are the interactions between agents. There are four
        operations in Kappa, change site's bond state (i.e. bind or
        unbind), change site's state (e.g. phosphorylate it, methylate
        it, ubiquitinate it), degrade an agent or set of agents,
        create an agent or set of agents. These operations can be
        combined into a single rule, or split into multiple, depending
        on the behavior one wants to simulate.
:Observables: These define the entities for which the simulator will
              output time-course abundances. If we define no
              observables, the simulator will not ouput any abundance.
:Perturbations: These modify the simulation, e.g. we add X amount of
                an agent, change the rate of a rule, or flip the state
                of all sites to some other state.

Agent signature
===============
Files customarily begin with the agent signatures, i.e. how we define
the interaction sites of a specified agent. The syntax is::

%agent: [agent name]([agent's sites])

For example, let's define an agent named ``Protein1`` that has two
sites called ``bindingSiteForProtein2`` and ``bindingSiteForProtein3``,
and a site called ``Serine12``, which can be in state ``Unmodified``,
``Phosphorylated``, or ``Mutated``, with ``Unmodified`` being the default
state. The agent would be defined as::

%agent: Protein1(bindingSiteForProtein2,bindingSiteForProtein3, Serine12~Unmodified~Phosphorylated~Mutated)

Or more succinctly as::

%agent: P1(P2, P3, S12~u~p~m)

Do note however that single letter codes quickly become opaque: if
``U`` is for unmodified, what's for ubiquitinated? If ``P`` is for
phosphorylated, what's for palmitoylated? If ``M`` is for mutated,
what's for methylated, di-methylated, or tri-methylated? Moreover, the
English alphabet contains 26 letters, but biochemistry knows of at
least 27 post-translational modifications. I therefore recommend using
at least two letters for state (e.g. ``ub``, ``ph``, ``py``, ``1m``,
``2m``, ``3m``, ``xx``).  Let's stick with::

%agent: Prot1(P2, P3,S12~un~ph~xx)

In fact, let's define two more agents similar to
``Prot1``. ``Prot2``'s sites are called ``P1`` and ``P3``, while
``Prot3``'s are ``P1`` and ``P2``. And while we're at it, add a text
pod in our script to mark where we're at. So far in our script, we
would have::

  ############################################################
  # Here are my agent signatures
  ############################################################
  %agent: Prot1(P2, P3, S12~un~ph~xx)
  %agent: Prot2(P1, P3, S12~un~ph~xx)
  %agent: Prot3(P1, P2, S12~un~ph~xx)

Now, it's time to write how these guys play together.

Rules
=====
Here we will write the rules that dictate how can our agents interact.
A rule's syntax is::

'[rule name]' [left-hand side] [arrow] [right-hand side] @ [bimolecular forward rate] ([unimolecular forward rate]), [bimolecular reverse rate] ([unimolecular reverse rate])

Where the left-hand side (LHS) is the pattern of reactants, the
right-hand side (RHS) is the pattern of products, and the arrow marks
if it is a reversible (i.e. ``<->``) or irreversible
(``->``)reaction. In terms of the guts of the simulator, what is doing
is matching the LHS to whatever is in the reaction mixture, and
replacing that with what we wrote in the RHS. In a more formal speech,
left go the sufficient conditions to trigger a rule, and right goes
the pattern injected by said rule's application. The pace at which a
rule is triggered is, what would be the rule's activity, is governed
by mass action dynamics. In other words, the probability of rule i
being triggered is given by P(i) = Ai / Sum(j,Aj), where Ai is the LHS
of rule i multiplied by the respective forward rate of rule i (for
reverse reactions, it would be the LHS times the corresponding reverse
rate).

Rule Rates
----------
A rule can technically have up to 4 rates, though in practice 3 is the
most seen for reversible binding rules, or 2 for irreversible binding
rules, 1 for irreversible unbinding rules. The rates are used when:

:bimolecular forward rate: if the LHS has ambiguous molecularity, this
                           is the rate for bimolecular cases. Think of
                           it as the diffusion of two independent
                           things.
:unimolecular forward rate: if the LHS has ambiguous molecularity,
                            this is the rate for unimolecular
                            cases. Think of it as the interaction of
                            things already bound, possibly through a
                            third party.
:bimolecular reverse rate: if the RHS has ambiguous molecularity, the
                           rule is reversible, this sis the rate for
                           bimolecular cases.
:unimolecular reverse rate: if the RHS has ambiguous molecularity, the
                            rule is reversible, this sis the rate for
                            unimolecular cases.

What do we mean by ambiguous molecularity? It means we specify two
agents which may be already connected through a path not described in
a rule. Let's take a look at an example of this.

Ambiguous Molecularity
----------------------

We want to express the reversible binding relation between ``Prot1`` and
``Prot2``, who bind through their respective ``P2`` and ``P1`` sites. For the
rates, a determinstic binding rate is on the order of ``1.0e8``, an
unbinding rate around ``1.0e-2`` (this would mean a disassociation
constant KD of around 1.0e-10 molar, or around 100 picomolar). When
accounting for volume, let's use a mammalian erythrocyte's volume of
1.0e-12 liters, the binding rate becomes ``1.0e-4`` (notice the unbinding
rate doesn't care about volume, so the deterministic rule is the same
as the stochastic one). Thus we arrive at our stochastic rates, a
forward (i.e. bind) rate of ``1.0e-4`` and a reverse (i.e. unbind) rate of
``1.0e-2``. Let's call such a rule 'P1.P2', it would be written as::

'P1.P2' Prot1(P2), Prot2(P1) <-> Prot1(P2!1), Prot2(P1!1) @ 1.0e-4,1.0e-2

The usage of ``!n``, where ``n`` is a number, identifies the binding
endpoints; we could have just as validly used ``!99`` or ``!0``. Let's keep
going and add the other two binding rules, one for ``Prot1`` binding
``Prot3``, and one for ``Prot2`` binding ``Prot3``.

::

'P1.P3' Prot1(P3), Prot3(P1) <-> Prot1(P3!1), Prot3(P1!1) @ 1.0e-4, 1.0e-2
'P2.P3' Prot2(P3), Prot3(P2) <-> Prot2(P3!1), Prot3(P2!1) @ 1.0e-4, 1.0e-2

Having these three rules, we can render the contact map, which would
look something like this:

.. image:: img/contactMap.svg

Notice there are no unimolecular rates in the above writing of the
rules. This means that the simulator will always use the bimolecular
rate to bind those agents. Consider however what would happen if we
apply a binding rule to agents already bound through a third party!
For example, if we have ``Prot1`` bound to ``Prot2`` itself bound to
``Prot3``, and we apply the binding rule of that ``Prot1`` to that
``Prot3``, the simulator would use the only rate we gave it, even
though diffusion would play no role in things already bound. This
would invalidate our physical interpretation of the model. Thus we
would refine the rules by adding a unimolecular forward (i.e. binding)
rate that's much higher than the bimolecular one::

'P1.P2' Prot1(P2), Prot2(P1) <-> Prot1(P2!1), Prot2(P1!1) @ 1.0e-4 (1.0), 1.0e-2
'P1.P3' Prot1(P3), Prot3(P1) <-> Prot1(P3!1), Prot3(P1!1) @ 1.0e-4 (1.0), 1.0e-2
'P2.P3' Prot2(P3), Prot3(P2) <-> Prot2(P3!1), Prot3(P2!1) @ 1.0e-4 (1.0), 1.0e-2

Notice that the RHS of our rules has to be unimolecular: we have the
``!1`` bond right there. The simulator is smart enough to recognize
this and will use ``1.0e-2`` as the sole unbinding rate; there is no
point in giving a bimolecular reverse rate as the RHS can not be
bimolecular. For this reason, it is rare binding rules have more than
3 rates, a bimolecular binding, a unimolecular binding, and a single
unbinding rate.

Let's add another rule. Now we want to add the production of ``Prot1``.
Since we don't really care about gene regulation,
transcription, mRNA regulation, translation, protein folding, protein
transport, but just want to have a steady production of the protein,
we can write a simple zeroth-order rule. In this case, said rule could
be written as::

'creation of Prot1' -> Prot1() @ 1.0

Or more succinctly::

'P1/' -> Prot1() @ 1.0

This rule would add one copy of ``Prot1()``, fully unbound, and with sites
in their default state, at around 1 per simulated second. At time 10,
we would have 10 more copies of ``Prot1``, at time 100, we would have 100
more copies. So far, our script should look something like this::

  ############################################################
  # Here are my agent signatures
  ############################################################
  %agent: Prot1(P2, P3, S12~un~ph~xx)
  %agent: Prot2(P1, P3, S12~un~ph~xx)
  %agent: Prot3(P1, P2, S12~un~ph~xx)

  ############################################################
  # Here are my rules
  ############################################################
  'P1.P2' Prot1(P2), Prot2(P1) <-> Prot1(P2!1), Prot2(P1!1) @ 1.0e-4 (1.0), 1.0e-2
  'P1.P3' Prot1(P3), Prot3(P1) <-> Prot1(P3!1), Prot3(P1!1) @ 1.0e-4 (1.0), 1.0e-2
  'P2.P3' Prot2(P3), Prot3(P2) <-> Prot2(P3!1), Prot3(P2!1) @ 1.0e-4 (1.0), 1.0e-2
  'P1/' -> Prot1() @ 1.0

It is worth noting that the agents must be in the same order on both
sides of the arrow signs. If not, they can be taken as spontaneous
degradation and production.

Initial Conditions
------------------
So by now we have the rules defined. It is time to move on to the
initial conditions. The syntax is quite simple, you specify with the
``%init initial conditions/concentrations`` of the reacting mixture. The
number specifies the amount of molecules. Let's say we want to start
the simulation with five hundred copies of ``Prot2`` and ``Prot3``. We
could write this as::

 %init: 500 Prot2(), Prot3()

This would start the simulation with the above amounts of each agent,
with all sites unbound, and sites in their default state. If we
wanted to initialize with complexes, we could just as fairly write::

%init: 500 Prot2(P3!1), Prot3(P2!1)

This would add 500 dimers to the simulation. Let's keep these two
declarations of initial conditions. Adding the text pod declaring
the initial condition stage, our script so far would look like this::

  ############################################################
  # Here are my agent signatures
  ############################################################
  %agent: Prot1(P2, P3, S12~un~ph~xx)
  %agent: Prot2(P1, P3, S12~un~ph~xx)
  %agent: Prot3(P1, P2, S12~un~ph~xx)

  ############################################################
  # Here are my rules
  ############################################################
  'P1.P2' Prot1(P2), Prot2(P1) <-> Prot1(P2!1), Prot2(P1!1) @ 1.0e-4 (1.0), 1.0e-2
  'P1.P3' Prot1(P3), Prot3(P1) <-> Prot1(P3!1), Prot3(P1!1) @ 1.0e-4 (1.0), 1.0e-2
  'P2.P3' Prot2(P3), Prot3(P2) <-> Prot2(P3!1), Prot3(P2!1) @ 1.0e-4 (1.0), 1.0e-2
  'P1/' -> Prot1() @ 1.0

  ############################################################
  # Here are my initial conditions
  ############################################################
  %init: 500 Prot2(), Prot3()
  %init: 500 Prot2(P3!1), Prot3(P2!1)

It's now time to declare the observables.

Observables
-----------
This is one of the most important parts of the script as this dictate
the program's plotting output. If we specify the rules and initial
mixture perfectly, but forget to observe for something, then we will
see nothing. The syntax is quite simple, we begin with %obs:, then
assign a name to that tracking event with 'name', and finally the code
of what exactly is the program tracking. For example::

%obs: 'Amount of Protein 1' Prot1()

Or more succinctly::

%obs: '[P1]' Prot1()

This would report the total amount of agent Prot1 under label '[P1]',
in whatever state it is, bound, unbound, modified, etc.

This means that on the output file, one of the column headers will be
'[P1]', and for that column, each row will be the time-point indexed
abundance of the label's definition; i.e. how much Prot1() was there
at those times. Let's define three more observables, in this case the
dimers of the system.

::

%obs: '[P1.P2]' Prot1(P2!1,P3), Prot2(P1!1,P3)
%obs: '[P1.P3]' Prot1(P2,P3!1), Prot3(P1!1,P2)
%obs: '[P2.P3]' Prot2(P1,P3!1), Prot3(P1,P2!1)

From the contact map, we see this the system has the capacity to
generate a cycle. Let's add another observable to check how many of
these trimer cycles there are. We would be observing for a Prot1 bound
to a Prot2 that's bound to Prot3 itself bound to the initial Prot1.

::

%obs: '[P1.P2.P3]' Prot1(P2!1,P3!3), Prot2(P1!1,P3!2), Prot3(P1!3,P2!2)

So far, our script should look something like this::

  ############################################################
  # Here are my agent signatures
  ############################################################
  %agent: Prot1(P2, P3, S12~un~ph~xx)
  %agent: Prot2(P1, P3, S12~un~ph~xx)
  %agent: Prot3(P1, P2, S12~un~ph~xx)

  ############################################################
  # Here are my rules
  ############################################################
  'P1.P2' Prot1(P2), Prot2(P1) <-> Prot1(P2!1), Prot2(P1!1) @ 1.0e-4 (1.0), 1.0e-2
  'P1.P3' Prot1(P3), Prot3(P1) <-> Prot1(P3!1), Prot3(P1!1) @ 1.0e-4 (1.0), 1.0e-2
  'P2.P3' Prot2(P3), Prot3(P2) <-> Prot2(P3!1), Prot3(P2!1) @ 1.0e-4 (1.0), 1.0e-2
  'P1/' -> Prot1() @ 1.0

  ############################################################
  # Here are my initial conditions
  ############################################################
  %init: 500 Prot2(), Prot3()
  %init: 500 Prot2(P3!1), Prot3(P2!1)

  ############################################################
  # Here are my observables
  ############################################################
  %obs: '[P1]' Prot1()
  %obs: '[P1.P2]' Prot1(P2!1,P3), Prot2(P1!1,P3)
  %obs: '[P1.P3]' Prot1(P2,P3!1), Prot3(P1!1,P2)
  %obs: '[P2.P3]' Prot2(P1,P3!1), Prot3(P1,P2!1)
  %obs: '[P1.P2.P3]' Prot1(P2!1,P3!3), Prot2(P1!1,P3!2), Prot3(P1!3,P2!2)

Execution
---------
Now let's execute the simulation! If you're using the browser based
IDE, put 5000 in the seconds field and hit run, leaving the 150 plot
points. If you're running the command-line executable, save your file
(e.g. "MyFile.ka") and invoke KaSim? with input-file "MyFile.ka", to
simulate 5000 seconds, and output 150 plot points to a file called
"MyOutput.out", i.e.::

$KaSim.exe -i MyFile.ka -t 5000 -p 150 -o MyOutput.out

This should generate a plot like this:

.. image:: img/Trajectories_all.svg

Notice that, as expected, the amount of P1 steadily increases. Notice
also that the amount of trimer increases up to a point, and then
decreases. In other words, in early times, the amount of Prot1 was
limiting the assembly of the trimer: there was not enough to go
around. However, at late times, there was too much. Notice the amount
of the dimers that contain Prot1, i.e. P1.P2 and P1.P3, steadily
increase. Thus, although Prot2 and Prot3 are still binding
independently Prot1, the likelihood that they bind the same Prot1
decreases as it accumulates. This inhibitory phenomenon is called a
prozone, and is very well known in immunology as the Hook effect. It
is a product of the concurrency between the binding of Prot2 and prot3
for Prot1.

Let's keep playing! Now let's think of what would happen if we set the
unimolecular binding rates to zero. That is, we disallow entities that
are already bound, from further binding. If we set the rates to zero,
and hit run with the same plotting parameters, we would get something
like this:

.. image:: img/Trajectories_all_zeroed.svg

The amount of trimer cycle is now zero, as we expected. However, the
system is not dominated by the dimers we defined. There are a thousand
copies of Prot2 and Prot3, but the amount of dimers does not add up to
this. What is happening? We can take a look at the reaction mixture by
using perturbations.

Perturbations and Modifications
-------------------------------


Let's start by checking the state of the reaction mixture, in what is
called a snapshot. We can tell KaSim? to produce a snapshot at any
given time with::

%mod: [T]>4500 do $SNAPSHOT

This will ouput a snapshot when the simulation advances past
timepoint 4500. In the IDE, such a snapshot would look like this:

TODO .. image:: img/Snapshot.svg

As we can see, the system has produced polymers! Instead of having
dimers, we have much bigger oligomers. How did this happen? Well, when
we made the rules, we did only mentioned some sites. For example, the
binding of Prot1 and Prot2 only mentions their P2 and P1 sites. Thus
event is independent of whatever else may be happening to the other
sites, those that go unmentioned. This illustrates Kappa's don't care,
don't write philosophy. We only write the sites that we care about,
and by omitting everything we don't care about, claim independence of
it. Our three dimerization events are therefore all independent, so
there are no geometric constraints.

If we wanted a system with geometric constrains, that means the sites
would be constrained. To make a 3 agent system where the biggest
entity is the trimer, one would have to write the collision events of
the respective obligate monomers, in addition to the collision events
of monomers with dimers. In effect, one ends up writting molecular
species (i.e. where every site is declared) instead of patterns
(i.e. where some things are omitted for independence), to include the
geometric constrains.

*******************
Glossary of Symbols
*******************
:#: start comment
:%agent\:: command to define an agent
:%obs\:: command to define an observable
:%var\:: command to define a variable
:%mod\:: command to define a modification or perturbation
:%def\:: command to define something, like a file name or the
       graphical format of a snapshot
:'': internal naming quotations, for rule names (' vs. ")
:"": external naming quotations, for file names (' vs. ")
:@: specify the reaction's rate
:@ X,Y: forward, reverse rate for the reversible reaction
:@ X(Y): bi(uni) -molecular rate for the rule with a molecularly
         ambiguous LHS
:Smith(foo): Specifies a site foo on agent Smith
:Y!x: Where x is a number, it indicates the bond's identity ending on
      site Y
:Y!_: Indicates site Y in any bond status (useful in observables)
:Y?: Indicates it doesn't matter if site Y is bound, to what, or not
     (notice the absence of !)
:Y~foo: Specifies site Y 's state as foo

.. _proto-IDE : https://dev/executableknowledge.org/try/
