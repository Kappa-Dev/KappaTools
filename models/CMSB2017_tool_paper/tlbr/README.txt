
***********************************************************************************

In this directory, you will find the Trivalent-ligand, bivalent-receptor system
that provides a general framework for studying receptor aggregation in the immune
system.

***********************************************************************************


To run the model from this directory, enter the command prompt and change
directories to this folder.  Then call BioNetGen to generate an NFsim
readable XML file with the command:

   > perl ../../BNG/BNG2.pl AN.bngl

Then, we have configured to RNF (run NF) scripts to set up a basic run.  You
can modify these scripts to run simulations for longer or modify the output.
To execute the RNF script in linux or mac, at the prompt, type:

  > ../../bin/NFsim_[version] -rnf tlbr.rnf

Where [version] is the version of the NFsim executable, which will be
specific to your operating system.  In windows, type:

  > ../../bin/NFsim_vX.XX.exe -rnf tlbr.rnf

Again [version] is the version of your NFsim executable, which you will find 
in the "bin" directory.

For more help on running NFsim, or if you run into problems, consult the user
manual available from the NFsim website.



