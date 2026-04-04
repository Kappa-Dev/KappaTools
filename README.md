# Incremental Kasa: Static Analysis of Kappa Models at Edit Time

The KaSa tool is a static analyzer for Kappa models. This branch implements an incremental version of KaSa, where the analysis result is updated incrementally for each change in the model, instead of recomputing it from scratch.

The [online webapp](https://tools.kappalanguage.org/try/?model=https%3A//raw.githubusercontent.com/Kappa-Dev/KappaTools/master/examples/abc.ka) unfortunately does not support the incremental KaSa yet, therefore to try it, the app needs to be built locally.
## Installation

### Prerequisites
- Install [opam](https://opam.ocaml.org/doc/Install.html) (the OCaml package manager). Run `opam init` to initialize it. It works best with ocaml version 4.14.2.
- For the benchmarks: install the latest version of `python3` and `python3-venv` (I tested it with python 3.10.12)
### Build
- Download the code from GitHub and navigate to the root directory of the project:
```bash
git clone https://github.com/Kappa-Dev/KappaTools.git
cd KappaTools
git checkout incremental-KaSa
```
- Install the dependencies by `opam install . --update-invariant --deps-only`
- Build the project with `dune build`
- Build the CLI: `make all`
- Build the electron app:
	- **on Linux**: `make Kappapp`
	- **on Windows**: `make KappappWin`
	- **on MacOS**: `make Kappapp.app`
## Kappapp: A GUI for KaSa

After having built the electron app, you can launch it with `./build/Kappapp/kappapp`.
A window should open with a text editor on the left side and the analysis result on the right half of the application.

### Text editor for the Kappa files
The Kappa model consists of one or more files, that can be imported or created by pressing the "File" button. 
The files can be modified in the in-app text editor.
The currently open file represents the *current chapter* of rules and initial states that can be incrementally modified.
Each rule and initial state in the editor has a checkbox that can be clicked on to disable or enable the corresponding element.
The static analysis result is updated accordingly.

If needed, the button "restart KaSa" can be used to restart the whole analysis from scratch.

### Visualization of the analysis result

1. **The contact map**: It shows the agents that are defined in the model with all their sites and all possible bonds between sites. If the accuracy "high" is chosen, the contact map is refined by removing all agents and bonds that are unreachable according to the reachability analysis.
2. **The rule influences**: For each rule, it shows which other rules it influences and which ones may be influenced by it. A rules is influenced by a second rule if the latter can create a biomolecular species that matches the left-hand side of the former rule. As before, the accuracy "high" means that unreachable rules are removed from the output.
3. **The constraints derived from the reachability analysis**: The reachability analysis computes some relationships between sites of the proteins. For example, it prints for each site all the possible states it can be in ("Non-relational properties") and it computes the relationship between sites of an agent ("Relational properties") and between sites of two bound agents ("Connected agents"). 
4. **Unbounded polymer formation**: The last tab informs the user if it is possible that chains of arbitrary length can be formed by the model.
5. **Dead rules/dead agents**: If some rules or agents are unreachable, they are underlined in the text editor and the UI warns that a dead rule or dead agent was detected.

### Example model

To test the GUI, open the file `examples/incremental_analysis/simple_example.ka` with the button `File -> Open`.

This model contains four agent types: A, B, C and D. The initial state contains the agents D and B. The agent A can be created by the rule `'create.A.if.D'`, if an agent D is present. The contact map shows the four types of agents. If the "high accuracy" contact map is chosen, we see that the agent C can never be formed. To solve this, we can add the initial state `%init: 100 C(c{u})`. If we do that, the analysis is updated incrementally and the warning "dead agent" disappears.

By disabling the checkboxes in front of the rules or initial states, we can see how the result of the analysis changes when some rules are removed.
For example, when disabling the rule `'bind.A_A'`, the model cannot form any unbounded polymers anymore.
And when disabling the first initial state of the agent D, the agents D and A become unreachable.

## KaSaIncremental: the Interactive CLI for KaSa

Note: for windows, use the command `.\_build\install\default\bin\KaSaIncremental.exe` instead of `./bin/KaSaIncremental`.

The CLI can be lauched with an example model by running:
```
./bin/KaSaIncremental --current-chapter examples/incremental_analysis/simple_example.ka --do-restart-fixpoint-iterations
```
The analysis prints the constraints that were calculated by the reachability analysis. For example, it says that C cannot occur in the model. This can be fixed by adding the inital state `%init: 100 C(c{u})` in the file `examples/incremental_analysis/simple_example.ka` and updating the analysis. 

The analysis can be updated by typing:
```
update file examples/incremental_analysis/simple_example.ka
```
in the interactive agent. After this, the analysis says "all agents may occur in the model".
Rules can be disabled by their label, for example:
```
disable 'create.A.if.D'
```
Subsequently, they can be enabled again:
```
enable 'create.A.if.D'
```

The command `help` prints more information about available commands.
## Benchmarks

The Table 1 and Fig. 4 in the in the tool paper `Incremental Reachability Analysis for the Rule-Based Modeling Language Kappa` were generated, respectively, by the scripts `script_disable_and_add_rules` and `script_compare_working_set_size`.

These scripts are written in bash, therefore they will unfortunately not work on Windows.

### Table 1: compare the incremental and non-incremental runtimes

Run the whole analysis with:
```
./examples/benchmarks/incremental-KaSa/script_disable_and_add_rules
```
This computes the runtimes of the non-incremental analysis, of the incremental analysis, of disabling some rules and of adding a rule, for 6 large Kappa models. The default timeout is of 50 minutes and each test is repeated 10 times. The number of rules that are added to the current chapter is 10.
These three values can be modified with the options `-t <timeout>`, `-r <reps>` and `-s <current_chapter_size>`, respectively. See also the help message (`-h`).

To run a small subset of the analysis, that only takes a few seconds to run, there is an option `-d`:
```
./examples/benchmarks/incremental-KaSa/script_disable_and_add_rules -d
```
This version runs the analysis with smaller models and only 2 repetitions.

The output of the script is a latex table with the mean runtimes in the file `examples/benchmarks/incremental-KaSa/output/experiments_output.tex`. The same runtimes are available as a csv file: `examples/benchmarks/incremental-KaSa/output/experiments_output.csv`.

### Fig. 4: compare the analysis overhead for different sizes of the current chapter

Run the whole analysis with:
```
./examples/benchmarks/incremental-KaSa/script_compare_working_set_size
```
This computes the runtimes of the incremental analysis for the working set sizes of 0, 10, 20, 30, 40, 50 and 60. 
As before, the default timeout is of 50min and the number of repetitions is 10.
These values can be modified with the options `-t <timeout>` and `-r <reps>`. See also the help message (`-h`).

To run a small subset of the analysis, that only takes a few seconds to run, there is an option `-d`:
```
./examples/benchmarks/incremental-KaSa/script_compare_working_set_size -d
```
This version runs the analysis with smaller models, only 2 repetitions and smaller current chapters.

The output of the script is a latex and a csv table with the mean runtimes in the files `examples/benchmarks/incremental-KaSa/output/experiments_output_compare_ws.tex` or `.csv`. The runtimes are also shown in a plot: `examples/benchmarks/incremental-KaSa/output/experiments_output_compare_ws_plot.png`.
