rm log.txt;

for g in  *.net;

do

  exec 3>&1 4>&2

  foo=$( {
    echo $g;
    rm -f $g;});

   exec 3>&- 4>&-; done

for f in  *.ka;

do
echo -------------------------------------- >> log.txt;
echo - $f >> log.txt;
echo -------------------------------------->> log.txt;

 exec 3>&1 4>&2

 foo=$( {
 echo ----------without_symmetries-----------;
  ( /usr/local/bin/gtimeout 2m ~/KaSim/bin/KaDe $f --print-efficiency --ode-backend DOTNET --dotnet-output network_$(basename $f .ka)_wo_sym.net --propagate-constants);

  2>&1 |  cat >> log.txt; } 2>&1 | cat >> log.txt; ) # change some_command
# exec 3>&- 4>&-; done

### with symmetries forward

 foo=$( {
 echo ----------with_symmetries_forward-----------;
/usr/local/bin/gtimeout 2m  ~/KaSim/bin/KaDe $f --print-efficiency --ode-backend DOTNET --with-symmetries Forward  --propagate-constants --dotnet-output network_$(basename $f .ka)_with_fsym.net ;
 2>&1 |
  cat >> log.txt; } 2>&1 | cat >>  log.txt ) # change some_command

### with symmetries backward
foo=$( {
echo ----------with_symmetries_backward-----------;
  /usr/local/bin/gtimeout 2m ~/KaSim/bin/KaDe $f --print-efficiency --ode-backend DOTNET --with-symmetries Backward --propagate-constants --dotnet-output network_$(basename $f .ka)_with_bsym.net 2>&1 |
 cat >>  log.txt; } 2>&1 | cat >> log.txt ) # change some_command

### compute symmetries 1>&3 2>&4

foo=$( {
  echo ----------compute_symmetries-----------;
    /usr/local/bin/gtimeout 2m ~/KaSim/bin/KaSa $f --print-efficiency --compute-symmetries
    2>&1 | cat >> log.txt;} 2>&1 | cat >> log.txt ) # change some_command

 foo=$( {
   echo ----------BNGL_symmetries----------;
   /usr/local/bin/gtimeout 2m ~/KaDe_tool_paper/BioNetGen/BNG2.pl $(basename $f .ka)_sym.bngl
     2>&1 | cat >> log.txt; } 2>&1 | cat >> log.txt ) # change some_command

foo=$( {
  echo ----------BNGL_without_symmetries-----------;
  /usr/local/bin/gtimeout 2m ~/KaDe_tool_paper/BioNetGen/BNG2.pl $(basename $f .ka).bngl
2>&1 | cat >> log.txt; } 2>&1 | cat >> log.txt ) # change some_command

#foo=$( {
  #echo ----------Summary-----------;
 #grep "CPU " -A 5 log.txt  2>&1 |
#cat >> log.txt; } 2>&1 | cat >> log.txt )

 exec 3>&- 4>&-;



done
