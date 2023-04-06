#algaeClassify install stub
#Vijay Patil 2022-03-02

#first, install the devtools package if not already available
#install.packages('devtools')
require(devtools)

#now install algaeClassify
install_github("vppatil/GEISHA_phytoplankton/package builds/algaeClassify",ref="working")

#should be algaeClassify v 2.0.0
#if asked to update packages, choose 3 (none) or hit enter to skip updates

#load the package
library(algaeClassify)

##view the functions
help(package="algaeClassify")
#in the help window, click on a function for a short example script using that function
#or view the help documentation for a function directly:
help("genus_species_extract")
help("species_to_mfg")
help("traits_to_mfg")



