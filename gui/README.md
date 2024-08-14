In this dir is the code for the webapp and electron app

`.ml` files starting by a capital, as `KaSimWorker.ml` are executable files.

TODO: revamp this as it's really imprecise and incomplete
`viz/main.js` contains the code for the main electron process
`runtime_processes.ml` setups the manager and runs the local subprocesses on electron
`JsSim.ml` defines the window DOM and its different components
    - Panel_tab contains all tabs from a project, other panels manages settings, projects...
    - tabs are the tabs of different tools in a given project, some are in fact subtabs in the right hand side of the editor, which is called subpanel_editor
- state_* files manages in part the logic in the given app parts


`secret_method_name` indicate a method that should be called only by inherited classes. However this was not respected thoroughly so it has not be replaced 
by `inherit parent as super  ... super#private_method` which fills this need in the language

`_mpi` indicates the message passing interface of the different processes and web workers (threads) used 

### Runtime
The app is divided in :
- the interface
- the agents: kasim, kamoha, kasa, kastor. Which are handling respectively the simulation, the project files, the static analysis, the analysis of traces and computation of stories

There are two different runtimes for the agents (see `state_runtime.ml`) :
- webworkers/threads for the agents (`runtime_web_workers.ml`). 
This is the mode ran in the browser. The app uses a matching client to communicate with each thread e.g. `kasa_client` which are writing _messages_ to the agents. The threads are spawned as _workers_ e.g. `KaSaWorker`, which use the _message passing interface_ logic e.g. `kasa_mpi`, which parses the messages and compute what is asked by the clients. 
There is a special case for the kasim agent. There is an `KaSimAsEmbedded` option that run it in the main thread and not in a separated web worker thread. The logic for the agent is then not in the `_mpi` but in `kasim_runtime` as it is used in both the _worker_ and in the main thread. Mode with kasim in a separated thread is called `KasimAsWebWorker`
- processes for the agents  (`runtime_processes.ml`)
This is the mode ran in the electron app. The app uses clients to communicate with agent processes.
Agents processes are defined in `core/agent` with a name starting with a capital letter. `mpi` files are in in different directories of `core` .
Kamoha agent and kastor agent have names that doesn't end in agent, which can be misleading. (TODO: change this?) 
The app here spawns two processes, Kastor and KappaSwitchman. The KappaSwitchman one then spawns the other agents and manages the communication between them and the app.
Kastor is not included with the others as it can be communicated to in raw, and it doesn't have an internal state.

