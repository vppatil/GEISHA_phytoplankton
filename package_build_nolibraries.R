###############################################################################
# package creation & maintenance file
# M.J. Lajeunesse, R course 7/5/16
###############################################################################

# define the package name
thePackage <- "PhytoDiv"

# set directory to the package development folder
setwd("~/gleon/Geisha/phyto_package/GEISHA_phytoplankton_github_shared/phytoDiv/")

# create new bare-bones package in development folder
# NOTE: only run package creation script once
#install.packages("devtools"); # make sure your have the most recent version
#install.packages("roxygen2"); # make sure your have the most recent version
library(devtools); library(roxygen2)
create(thePackage)
roxygenize(thePackage)

# STEP 2, add functionality ###################################################
# (1) update DESCRIPTION file (package description, authors, dependencies)
# (2) add functions (*.R) in R folder of package
# (3) within *.R files should include roxygen2 markdown of function and 
#     parameter txt (e.g., what you see in manual) 
###############################################################################

# STEP 3, error check and compile package for CRAN ############################
# (1) update DESCRIPTION file
# (2) add functions (*.R) in R folder of package
###############################################################################
# creates bundle to submit to CRAN (*.tar.zip file found in development folder)
build(thePackage, manual = FALSE)
# error checks prior to submission (all errors and warnings need to be addressed)
check(thePackage)
warning()

library(devtools);
document(thePackage)
path <- find.package(thePackage)
system(paste(shQuote(file.path(R.home("bin"), "R")), "CMD", "Rd2pdf", shQuote(path)))

##vijay note- need to @export all function in the roxygen header
##also @examples works but @examples does not.
#with no function definition above, it will try to execute and hit an error otherwise.

# STEP 4, Build example PDF manual (will not be same as CRAN, but close) ######
###############################################################################

# BEFORE STARTING##############################################################
# 1) Download & install MiKTeX to generate manual & vignettes
# 2) through MikTeX /Maintenace/packages install: url, inconsolata, upquote
###############################################################################
# # constructs binaries 




