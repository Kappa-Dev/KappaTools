rm log.txt;
for f in  *.ka;

do echo ---------------------$f--------------->> log.txt;

 exec 3>&1 4>&2
 foo=$( {
 echo ----------without_symmetries-----------;
 time  ~/KaSim/bin/KaDe $f --ode-backend DOTNET --propagate-constants 2>&1 |
  cat >> log.txt 1>&3 2>&4; } 2>&1 | cat >> log.txt ) # change some_command
# exec 3>&- 4>&-; done

### with symmetries forward

 foo=$( {
 echo ----------with_symmetries_forward-----------;
 time  ~/KaSim/bin/KaDe $f --ode-backend DOTNET --with-symmetries Forward  --propagate-constants 2>&1 |
  cat >> log.txt 1>&3 2>&4; } 2>&1 | cat >> log.txt ) # change some_command

### with symmetries backward
foo=$( {
echo ----------with_symmetries_backward-----------;
time  ~/KaSim/bin/KaDe $f --ode-backend DOTNET --with-symmetries Backward --propagate-constants 2>&1 |
 cat >> log.txt 1>&3 2>&4; } 2>&1 | cat >> log.txt ) # change some_command

### compute symmetries

foo=$( {
  echo ----------compute_symmetries-----------;
   time  ~/KaSim/bin/KaSa $f --compute-symmetries  2>&1 |
 cat >> log.txt;
} 2>&1 | cat >> log.txt ) # change some_command

 exec 3>&- 4>&-; done
