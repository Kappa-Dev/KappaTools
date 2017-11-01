# From KaSim 3 to KaSim 4

## Kappa syntax has completely changed

Sincerely sorry for the inconvienence. We strongly believe that the new
one will be less error prone...

You can specify `-syntax 3` in the command line) to use KaSim4 with a
model in KaSim3 syntax. In addition, the ouptut log file "inputs.ka"
will be a copy of your model in KaSim4 syntax.

### Site specification

The general picture is now the following:

  * linking states are specified in between square brackets ` []`
  * internal states in between curly brackets `{}`
  * internal states cannot be integer anymore
  * Being free has an explicit syntax: `[.]`
  * syntax for whatever is `#` instead of `?`.

Here is a table of correspondance:

KaSim < 4 | Syntax V4
---------------------
    x     |   x[.]
   x!1    |   x[1]
   x!_    |   x[_]
  x!y.B   |  x[y.B]
    x?    | x[#] or x
---------------------
   y~p?   |   y{p}

### Agent order in rules

There is no longuest prefix convension anymore. The i-th agent on the
left hand side correspond to the i-th agent on the right hand side of
the rule. There must be exactly as many agents on both side. Agents
degraded by the rule are replaced by a dot `.` on the right. Agent
synthesised are replaced by a dot on the left.

Agents must specify the same sites and the same states on both side.
For example, You must put an explicit `#` in `A(x{#}) -> A(x{p})` to
say that you wrote a force internal state change on purpose (that's
why `#` have been introduced for).

### Commas in list of sites and states are facultative

Commas remains mandatory in lists of agents.

### List of tokens in rules are separated by `,` instead of `+`

### No more `:` in rules between the quantity and the token name.

### Rates for unary instances of rule involving ambiguous molecularity
  are in `{}` instead of `()`

### Repetitive perturbation syntax is now

`%mod: [precondition] do [modifications] repeat [condition]`

Beware: the second condition is the negation of the old until clause.

### Syntax of $PRINTF is
```
$PRINTF "what you print" > "Where_you_print.txt"
```

The `>` can be thought as the shell redirect operator.

### complex print expresion must be putted in parenthesis

In modifications that takes string as arguments (`$SNAPSHOT`, `$FLUX`,
`$PRINT`, ...), as soon as the expression is more complex than a
single string (it contains concatenations), it must be put in
parenthesis.

Example: `%mod: |A(a)| < 100 do $SNAPSHOT ("snap_at_".[T].".ka")`

### tokens are initialised like species:

`%init: [alg_expr] [token_name]`.

### There is no special case for "observables"

Number of occurrences of the pattern pat is now written |pat| and it can
be placed in any algebraic expression.

To be concrete, you need to write `%var: 'o' |A(x!1),B(x!1)|` instead
of `%var: 'o' A(x!1),B(x!1)`.

But you can write directly `%var: 'o' 1 + |A(x!1),B(x!1)| / 2` instead
of
```
%var: 'tmp_o' A(x!1),B(x!1) %var: 'o' 1 + 'tmp_o' / 2
```

As a (more or less direct) consequence, you can write
```
%init: "any
algebraic expression" "mixture"
```
and `$TRACK "pattern" [true]`

## Options '-t' '-e' and '-p' are removed!

You should specify the limit of simulation by '-l' and the plot period
(how often you want a data line) by the "new" '-p'. These options are
by default in "simulated time unit" but you can switch to event using
'-u event'.

The motivation is interactivity. If you don't specify any limit, the
simulation will run forever.  (Use Ctrl-c to stop).

## Data files are printed in comma separated value format

(instead of space separated value) by default.

The default name of the output file is therefore 'data.csv'.

KaSim can also generate (based on the extension of the output file)
tsv or svg files.

The motivation is to deal with weird values (e.g. infinity, not a number) in a
way understood by (at least) Gnuplot, Python, Matlab, ...

## Modification $STOP without argument does not dump a snapshot

Use $STOP "something" to get one

## Ctrl-c does not stop the simulation

It pauses and launches a toplevel in which you can do modification
(as in perturbations) and then (optionally) continue the simulation.

You can use '-mode batch' to get a Ctrl-c that does kill the simulation
(and more generally to get a KaSim that never asks questions) or '-mode
interactive' to pause the simulation just after the initialization
phase.

Concretely the old behavior of Ctrl-c and then y(es I want a snapshot)
is recovered by Ctrl-c and the modification $STOP "dump.ka"
