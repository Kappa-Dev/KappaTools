# From KaSim 3 to KaSim 4

## The Kappa syntax has completely changed

We are sincerely sorry for the inconvenience. We strongly believe that
the new syntax will be less error prone.

For legacy kappa code written in ye olde Kappa, the KaTools can be
called from the command line with the option `-syntax 3`. The output log
file "inputs.ka" will be a copy of your model, but updated to KaSim4
syntax.

### Comments follow the C standard

* `//` start a line comment
* `/*` and `*/` flank a block comment
* `#` no longer starts a line comment; it is the new wildcard for site
state

### Sentences split over several lines no longer require a backslash `\`

You can go to a new line anytime anywhere, don't put `\`. For example:
```
%mod: [T] > 500 do (
    $ADD 50 A() ;
    $DEL 20 B() ;
    $SNAPSHOT "snap.ka"
    )
```

### Site specification uses new symbols

The general picture is now the following:

  * linking states are specified between square brackets ` []`
  * internal states between curly brackets `{}`
  * internal states cannot be integers anymore
  * free sites have an explicit syntax: `[.]`
  * the syntax for whatever is `#` instead of `?`

Here is a correspondence table:

Syntax < 4 | Syntax 4
----------|----------
`A(x)` | `A(x[.])`
`A(x!1)` | `A(x[1])`
`A(x!_)` | `A(x[_])`
`A(x!y.B)` | `A(x[y.B])`
`A(x?)` | `A(x[#])` or `A(x)`
`A(y~p?)` | `A(y{p}[#])` or `A(y{p})`

A(x) -> A(x!1) | A(x[./1]
A(x!1) -> A(x) | A(x[1/.]
A(x~u) -> A(x~p) | A(x{u/p})

A(x),B(x) <-> A(x!1), B(x!1) | A(x[./1]),B(x[./1])
A(x!1),B(x!) <-> A(x), B(x) | A(x[1/.]),B(x[1/.]
C(x1~u!1), A(c!1) -> C(x1~p),A(c) | C(x1{u/p}[1/.]),A(c[1/.])

### Agent order in rules must be maintained

There is no longest prefix convention anymore. The _i_-th agent on the
left hand side must correspond to the _i_-th agent on the right hand
side of the rule. There must be exactly as many agents on both sides.
Agents degraded by the rule are replaced by a dot `.` on the right.
Conversely, agents synthesised are replaced by a dot on the left. For
example:
```
// Agent A spawns a copy of agent B
A(),. -> A(),B() @ 'r'

// When dimerized, agent B degrades agent A
A(x[1]),B(x[1]) -> .,B(x[.]) @ 'r'
```

If a site's internal state is modified, it must appear on both sides in
arrow notation. I.e. Agents must specify the same sites and the
corresponding states on both sides. For example:

```
// Change from state u to state p
A(x{u}) -> A(x{p}) @ 'r'
// Force internal state to change from whatever to p
A(x{#}) -> A(x{p}) @ 'r'
```

You must put an explicit `#` in `A(x{#}) -> A(x{p})` to
say that you wrote a force internal state change on purpose (that's
why the "whatever" `#` has been introduced for).

### Certain uses of comma are now optional

Commas in internal state lists, or site lists, are optional. E.g.
```
%agent: A(x{u p b q} y z)
```

Commas remain mandatory in lists of agents. E.g.
```
A(x[.] y[.]), B(x[.] y[.]) -> A(x[1] y[2]), B(x[1] y[2])
```

### Tokens in a list are separated by `,` instead of `+`

```
 | 1 U -> | 1 Kr, 1 Ba @ 'r'
```

### Colon `:` no longer separates tokens and their quantity
This change also standardizes the syntax for initial conditions between
agents and tokens. The syntax to introduce tokens is:
 `%init: [alg_expr] [token_name]`

```
%init: 5 A(x[.])
%init: 9 ATP
```

### Unary rates in ambiguous cases are in `{}` instead of `()`
In situations where a rule's left-hand side can be embedded both into a
connected graph and a disconnected graph, two rates are required. Usage
is:
```
[LHS] <-> [RHS] @ [binary rate] {unary rate}, [reverse rate]
```

### Repetitive perturbation syntax has changed
Usage is now if _precondition_ do _modification(s)_ and repeat as long
as _condition_.

```
%mod: [precondition] do [modifications] repeat [condition]
```

Beware: the second condition is the negation of the old until clause.
I.e. it is a _repeat as long as_ rather than the old _repeat until this
is true_.

### Syntax of $PRINTF has changed
The new syntax allows one to redirect output to a file.

```
$PRINTF "what you print" > "Where_you_print.txt"
```

The `>` can be thought as the shell redirect operator.

### Complex print expressions must be in parenthesis

In modifications that take strings as arguments (e.g. `$SNAPSHOT`,
`$FLUX`, `$PRINT`), as soon as the expression is more complex than a
single string (i.e. it contains concatenations), it must be placed in
parenthesis. E.g.
```
%mod: |A(a)| < 100 do $SNAPSHOT ("snap_at_".[T].".ka")
```

This stacks with the parenthesis for multi-effect perturbations.
```
%mod: |A(a)| < 100 do (
    $SNAPSHOT ("snap_at_".[T].".ka") ;
    $PRINT (|A()|." present at time ".[T]) > "myfile.txt" ;
    $ADD 10 A()
    )
```

### Syntax for "observables" is no longer a special case

The number of occurrences of the pattern `pat` is now written `|pat|`
and it can be placed in any algebraic expression.

Thus, a variable is defined as:
```
%var: '[name]' |[pattern]|
```
For example
```
%var: 'o' |A(x[.]),B(x[.])| // Correct

%var: 'o' A(x[.]),B(x[.])   // Syntax error, lack of pipes
%var: 'o' A(x!1),B(x!1)     // Syntax error, lack of pipes and legacy bond marks
```


This changes allows more concise statements, for example
```
%var: 'o' 1 + |A(x!1),B(x!1)| / 2 // Oneliner in KaSim4 syntax

%var: 'tmp_o' A(x!1),B(x!1)       // Twoliner in KaSim3 syntax
%var: 'o' 1 + 'tmp_o' / 2
```

As a (more or less direct) consequence, you can write
```
%init: "any algebraic expression" "mixture"

$TRACK "pattern" [true]
```


## Options `-t` and `-e` are gone, and `-p` has changed!

You should specify the limit of simulation by `-l` and the plot period
(how often you want a data line) by the _new_ `-p`. These options are
by default in _simulated time unit_ but you can switch to event-based
using `-u event`.

The motivation for this change is interactivity. If you don't specify
any limit, the simulation will run forever (use Ctrl-c to stop it).

## Data files are printed in CSV (comma separated value) format
Previously, the default was to produce an `data.out` file with space
separated values.  The default is now to produce a _csv_ file
`data.csv`.

In addition to comma separated files, KaSim can also generate tab
separated files (`.tsv`), and even a vector-based graphical
representation of the time traces, indexed by time (`.svg`).
This is done by specifying the file extension of the output file with
the option `-o [filename]`.

The motivation for these changes is to deal with weird values
(e.g. infinity, not a number) in a way understood by (at least)
GnuPlot, Python, Matlab, etc.

## `$STOP` without argument does not dump snapshot

Use `$STOP "[filename]"` to get a final-state snapshot with the right
filename.

## Ctrl-c does not kill the simulator

Ctrl-c now pauses the simulation and launches a toplevel environment in
which you can do modifications and perturbations interactively, and
then (optionally) continue the simulation.

You can use `-mode batch` to get a Ctrl-c that does kill the simulator
(and more generally to get a KaSim that never asks questions and assumes
default answers) or `-mode interactive` to pause the simulator just
after the initialization phase has completed (i.e. before the first
event happens).

Concretely the old behavior of Ctrl-c and then `y`(es I want a snapshot)
is recovered by Ctrl-c and the modification `$STOP "dump.ka"`
