Files in `state` manages state logic from different app part, similar to a `model` in a Control/Model/View architecture.

`state_error.ml` manages errors at the webapp level

`state_ui.ml` call _init_ and _sync_ for `state_*` files

`state_file.ml` mainly manages editor & kasa stuff, but is accessed too from settings, state_ui

`state_perturbation.ml` manages simulation perturbations, only used from panel preferences

`state_runtime.ml` defines worker runtime, used from other `states_*` + preferences + projects controller

`state_simulation.ml` sets simulation controls and functions to interact with simulation data
