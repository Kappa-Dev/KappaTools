In this dir is the code for the webapp and electron app

`viz/main.js` contains the code for the main electron process
`JsNode.ml` setups the manager and runs the local subprocesses
`JsSim.ml` defines the window DOM and its different components
    - Panel_tab contains all tabs from a project, other panels manages settings, projects...
    - tabs are the tabs of different tools in a given project, some are in fact subtabs in the right hand side of the editor, which is called subpanel_editor
- state_* files manages in part the logic in the given app parts
