
***********************************************************************************

In this directory, you will find two models.  The actin_simple.bngl model
is a simple model that includes dynamic assembly and severing.  The 
actin_branch.bngl model is the full model that includes branching and capping,
in addition to assembly and severing. 

***********************************************************************************


To run the models from this directory, enter the command prompt and change
directories to this folder.  Then call BioNetGen to generate an NFsim
readable XML file with the command:

   > perl ../../Perl2/BNG2.pl actin_simple.bngl

Then, we have configured to RNF (run NF) scripts to set up a basic run.  You
can modify these scripts to run simulations for longer or modify the output.
To execute the RNF script in linux or mac, at the prompt, type:

  > ../../bin/NFsim_[version] -rnf run_actin_simple.rnf

Where [version] is the version of the NFsim executable, which will be
specific to your operating system.  In windows, type:

  > ../../bin/NFsim_[version].exe -rnf run_actin_simple.rnf

Again [version] is the version of your NFsim executable, which you will find 
in the "bin" directory.  You can run the branching model in the same way.  To get 
complete output from the branching model, be sure to edit the RNF file to create 
system dumps.

For more help on running NFsim, or if you run into problems, consult the user
manual available from the NFsim website.