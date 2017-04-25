rm log.txt;

for g in  *.net;

do

  exec 3>&1 4>&2

  foo=$( {
    echo $g;
    rm -f $g;});

   exec 3>&- 4>&-; done

for f in  *.ka;

do echo ---------------------$f--------------->> log.txt;

 exec 3>&1 4>&2

 foo=$( {
 echo ----------without_symmetries-----------;
 time  ~/KaSim/bin/KaDe $f --ode-backend DOTNET --dotnet-output network_$(basename $f .ka)_wo_sym.net --propagate-constants 2>&1 |
  cat >> log.txt 1>&3 2>&4; } 2>&1 | cat >> log.txt ) # change some_command
# exec 3>&- 4>&-; done

### with symmetries forward

 foo=$( {
 echo ----------with_symmetries_forward-----------;
 time  ~/KaSim/bin/KaDe $f --ode-backend DOTNET --with-symmetries Forward  --propagate-constants --dotnet-output network_$(basename $f .ka)_with_fsym.net 2>&1 |
  cat >> log.txt 1>&3 2>&4; } 2>&1 | cat >> log.txt ) # change some_command

### with symmetries backward
foo=$( {
echo ----------with_symmetries_backward-----------;
time  ~/KaSim/bin/KaDe $f --ode-backend DOTNET --with-symmetries Backward --propagate-constants --dotnet-output network_$(basename $f .ka)_with_bsym.net 2>&1 |
 cat >> log.txt 1>&3 2>&4; } 2>&1 | cat >> log.txt ) # change some_command

### compute symmetries

foo=$( {
  echo ----------compute_symmetries-----------;
   time  $HOME/KaSim/bin/KaSa $f --compute-symmetries  2>&1 |
 cat >> log.txt;
} 2>&1 | cat >> log.txt ) # change some_command

 foo=$( {
   echo ----------BNGL_symmetries---------$(HOME)-;
   time $HOME/KaDe_tool_paper/BioNetGen/BNG2.pl $(basename $f .ka)_sym.bngl  2>&1 |
 cat >> log.txt; } 2>&1 | cat >> log.txt ) # change some_command

foo=$( {
  echo ----------BNGL_without_symmetries-----------;
  time $HOME/KaDe_tool_paper/BioNetGen/BNG2.pl $(basename $f .ka).bngl  2>&1 |
cat >> log.txt; } 2>&1 | cat >> log.txt ) # change some_command


 exec 3>&- 4>&-; done
