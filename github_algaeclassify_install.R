#short script to install preliminary phytoDiv package on github
#comment the next line if you have already installed devtools

# install.packages('devtools')
library(devtools)

library(httr)
set_config(config(ssl_verifypeer = 0L))

#the next line will only need to be run once, or whenever you need an updated version of the code
install_github("vppatil/GEISHA_phytoplankton/package builds/algaeClassify")
library(algaeClassify)

citation("algaeClassify") #preliminary citation before the manuscript comes out.

#view package description. All available functions listed at the bottom:
library(help="algaeClassify")

#view help documentation for functions
help(traits_to_mfg)
help(traits_to_mfg_df)
