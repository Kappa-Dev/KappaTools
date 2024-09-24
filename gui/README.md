In this dir is the code for the _Kappapp_ webapp and electron app!

### Directory structure

The `.ml` files in this directory , as `KaSimWorker.ml` are executable files, as indicated by their capitalization.

`JsSim.ml` defines the main executable, with the window DOM and its different components,
while the other are the different workers that can be ran as webworkers/threads in the app.

`ui` contains the different panels and tabs making the interface, along with their logic. The logic tries to match in limited fashion the view and controller roles in a Control/Model/View architecture (controller for controller_* files in `ui/tab_editor`)

Files in `state` manages state logic from different app part, similar to a `model` in a Control/Model/View architecture.

`entry_point` contains the entry point info for the html main page of the webapp and electron app config

`lib`, `lib_no_jsoo`, `js_lib` contain libs used by the webapp. Those in `lib_no_jsoo` do not depend on js_of_ocaml. Libs in `js_lib` are written in javascript`, while the other are in ocaml.

`resources` contains resources used to build the webapp and electron app.

### Runtime
The app is divided in :
- the interface
- the agents: kasim, kamoha, kasa, kastor. Which are handling respectively the simulation, the project files, the static analysis, the analysis of traces and computation of stories

There are two different runtimes for the agents (see `state/state_runtime.ml`) :
- webworkers/threads for the agents (`state/runtime_web_workers.ml`). 
This is the mode ran in the browser. The app uses a matching client to communicate with each thread e.g. `kasa_client` which are writing _messages_ to the agents. The threads are spawned as _workers_ e.g. `KaSaWorker`, which use the _message passing interface_ logic e.g. `kasa_mpi`, which parses the messages and compute what is asked by the clients. 
There is a special case for the kasim agent. There is an `KaSimAsEmbedded` option that run it in the main thread and not in a separated web worker thread. The logic for the agent is then not in the `_mpi` but in `kasim_runtime` as it is used in both the _worker_ and in the main thread. Mode with kasim in a separated thread is called `KasimAsWebWorker`
- processes for the agents  (`state/runtime_processes.ml`)
This is the mode ran in the electron app. The app uses clients to communicate with agent processes.
Agents processes are defined in `core/agent` with a name starting with a capital letter. `mpi` files are in in different directories of `core` .
Kamoha agent and kastor agent have names that doesn't end in agent, which can be misleading. (TODO: change this?) 
The app here spawns two processes, Kastor and KappaSwitchman. The KappaSwitchman one then spawns the other agents and manages the communication between them and the app.
Kastor is not included with the others as it can be communicated to in raw, and it doesn't have an internal state.

### Misc notes

This code uses js_of_ocaml, the React lib for reactive programming, and lwt. 
Some changes in the code might cause parts of the code to not be called. This happens even with the removal of the dead code elimination in js_of_ocaml. A way to fix these issues is to use the Hooked lib in `lib_no_jsoo`, that is meant to replace the React lib with hooks, which don't have this issue. Replacing `React` by `Hooked` in place, and converting the signals to Hooked and back should help if this issue arise again.
Feel free to add an implementation for needed `React` functions that are not yet implemented in `lib_no_jsoo/hooked.ml`.

`secret_method_name` indicate a method that should be called only by inherited classes. However this was not respected thoroughly so it has not be replaced 
by `inherit parent as super  ... super#private_method` which fills this need in the language

`_mpi` indicates the message passing interface of the different processes and web workers (threads) used 

