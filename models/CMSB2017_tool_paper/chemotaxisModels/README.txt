
***********************************************************************************

In this directory, you will find two models.  The first is the original Assistance
Neighborhood model of chemoreceptor adaptation in bacterial chemotaxis that includes multiple receptors with explicit CheR/CheB.  The second, named ANx.bngl, is the extended model that
includes an updated receptor topology.

***********************************************************************************


To run the models from this directory, enter the command prompt and change
directories to this folder.  Then call BioNetGen to generate an NFsim
readable XML file with the command:

   > perl ../../BNG/BNG2.pl AN.bngl

Then, we have configured to RNF (run NF) scripts to set up a basic run.  You
can modify these scripts to run simulations for longer or modify the output.
To execute the RNF script in linux or mac, at the prompt, type:

  > ../../bin/NFsim_[version] -rnf run_AN.rnf

Where [version] is the version of the NFsim executable, which will be
specific to your operating system.  In windows, type:

  > ../../bin/NFsim_vX.XX.exe -rnf run_AN.rnf

Again [version] is the version of your NFsim executable, which you will find 
in the "bin" directory.  You can run the extended model in the same way.  To 
get complete output from the AN models, be sure to edit the RNF file to create 
system dumps.

For more help on running NFsim, or if you run into problems, consult the user
manual available from the NFsim website.



