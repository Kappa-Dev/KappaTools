# Tests webapp

For now, tests to be done manually in the app

Should be done in each considered brower and in the electron app, and importantly at least on windows and mac

## TODOs : to add to the procedure
Doesn't show up at the time of writing:
- outputs

## Not tested in the procedure
- file menu
- new project
- in contact map subpanel: accuracy, export, hide guide lines
- in influences subpanel: navigation, graph parameters
- no check for missing test data in subpanels: 3 last constraints + polymers
- in sim controls: pause if, all outputs, get trace
- plot export
- DIN export


## Test procedure

First level of dashes: what to do

Second level: what to check additionnally

#### Load and editor
- open the app
- load file https://tools.kappalanguage.org/try/?model=https%3A//raw.githubusercontent.com/Kappa-Dev/KappaTools/master/examples/abc.ka
- make an error in the file, for instance delete last character in line 21 > %obs: 'Cuu' |C(x1{u},x2{u})| 
  check that the error is displayed in the gutter, in the code underlined in red, at the bottom right : > 1/1[abc.ka] Illegal definition of variable 'Cuu' 
- undo the error e.g. ctrl+z
- add at the end of the file: > %agent: D(a{u p})
  - check that warning dead agent is shown in the gutter and at the bottom right
- add > 'd' D(a{p}) -> D(a{u}) @ 1
  - check that the dead rule warning show up in the same way
- delete those lines
- click twice on toggle to check the opening/closure

#### Right subpanel
- Contact map is present, see that it changes when clicked on - interactive mode - show all states
- Go to influences: is it populated?
- Change rendering to graph, change accuracy to high, see that it changes
- Change back to Tabular, click Track cursor. click on different rules in the editor to see that it changes in the subpanel
- Go to constraints subpanel, see that the two views domain are populated
- Go to polymers, see that it shows > The size of biomolecular compounds is uniformly boundedG.

#### Simulation
- Click on start, then on pause. See that it showed "running" with events and time, and now Paused
- change to tab log, see that it shows lines with + Building initial simulation conditions...   and random seed used
- Change to plot, click on the different plot axes options and see the changes
- Continue/pause the simulation and see the plot changing, move the plot position with the slider
- Change to DIN tab, check that a table is populated and that we can switch to the second table
- Change to Snapshot tab, enter $SNAPSHOT in the Simulation perturbation and click intervention, see that the snapshot shows up
- Click on kappa and change display to graph, play with the graph settings
- Change back to kappa, continue/stop the simulation, make a new snapshot, and test to switch between snapshot
- TODO: this currently doesn't work, adapt this when it does : - Go to outputs, run > $PRINT "time: ".[T]  and  > $PLOTENTRY   see that it shows up  
- Go back to log tab and see the previous interventions show up

#### Stories
- Copy/paste in the editor https://www.di.ens.fr/~feret/teaching/2023-2024/MPRI.2.19/activities/causality/causality_slide_10.ka
- Go to preferences, enable store trace, click Set
- clear/start/pause the simulation
- Go to the stories tab, click launch
- See that the computation log show up, that a story graph appears on the right
- Click on story_info log, see that it shows up
- Change the story, see the log and graph changes
- Test causal and strongly compressed stories the same way