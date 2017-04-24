
***********************************************************************************

In this directory, you will find the models that correspond to the multi-site
phosphorylation model used for NFsim performance testing.

***********************************************************************************


To run the models from this directory, enter the command prompt and change
directories to this folder.  Then call BioNetGen to generate an NFsim
readable XML file with the command:

   > perl ../../BNG/BNG2.pl e1.bngl

Then, you can run the model by typing in mac or linux with the command:

  > ../../bin/NFsim_[version] -xml e1.xml

Where [version] is the version of the NFsim executable, which will be
specific to your operating system.  In windows, type:

  > ../../bin/NFsim_[version].exe -xml e1.xml

Again [version] is the version of your NFsim executable, which you will find 
in the "bin" directory.  For more help on running NFsim, or if you run into 
problems, consult the user manual available from the NFsim website.
