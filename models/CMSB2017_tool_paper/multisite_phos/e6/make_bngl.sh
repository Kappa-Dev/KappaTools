rm log_bngl.txt;
for f in  *.bngl;

do echo ---------------------$f--------------->> log_bngl.txt;

 exec 3>&1 4>&2
 foo=$( {
 echo ----------BNGL-----------;
 time  ~/KaDe_tool_paper/BioNetGen/BNG2.pl $f 2>&1 |
  cat >> log_bngl.txt; } 2>&1 | cat >> log_bngl.txt ) # change some_command
# exec 3>&- 4>&-; done

 exec 3>&- 4>&-; done
