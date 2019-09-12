###############################################################################
# package creation & maintenance file
# M.J. Lajeunesse, R course 7/5/16
###############################################################################

# define the package name
thePackage <- "algaeClassify"

# set directory to the package development folder
setwd("~/gleon/Geisha/phyto_package/GEISHA_phytoplankton_github_shared/package builds/")

# create new bare-bones package in development folder
# NOTE: only run package creation script once
#install.packages("devtools"); # make sure your have the most recent version
#install.packages("roxygen2"); # make sure your have the most recent version
library(devtools); library(roxygen2)
# create(thePackage)
setwd('algaeClassify')
roxygenize('.')


# STEP 2, add functionality ###################################################
# (1) update DESCRIPTION file (package description, authors, dependencies)
# (2) add functions (*.R) in R folder of package
# (3) within *.R files should include roxygen2 markdown of function and 
#     parameter txt (e.g., what you see in manual) 
###############################################################################

# add dataset to package
lakegeneva <- read.csv("~/gleon/Geisha/datasets/phyto_data/lakegeneva_stub.csv", header = TRUE,stringsAsFactors = F)
# use_data(library_MFG, pkg = thePackage, internal = TRUE, overwrite = TRUE)
use_data(lakegeneva, overwrite = TRUE)


# setwd('~/gleon/Geisha/phyto_package/GEISHA_phytoplankton_github_shared/package builds/')# use_data(library_MFG, pkg = thePackage, internal = TRUE, overwrite = TRUE)
# 
# load('sppMFG.rda')
# setwd('data/')
# load('species.mfg.library.rda')
# setwd('..')
# use_data(species.mfg.library, overwrite = TRUE)

setwd('data/')
load('traitranges.rda')
traitranges=traitranges[1:3,]
traitranges$units=c('um^-1','um','')
setwd('..')
use_data(traitranges, overwrite = TRUE)

# STEP 3, error check and compile package for CRAN ############################
# (1) update DESCRIPTION file
# (2) add functions (*.R) in R folder of package
###############################################################################

# constructs binaries 
library(devtools); 
# creates bundle to submit to CRAN (*.tar.zip file found in development folder)
build('.', manual = FALSE)
# error checks prior to submission (all errors and warnings need to be addressed)
check('.')
warning()

# STEP 4, Build example PDF manual (will not be same as CRAN, but close) ######
###############################################################################

# BEFORE STARTING##############################################################
# 1) Download & install MiKTeX to generate manual & vignettes
# 2) through MikTeX /Maintenace/packages install: url, inconsolata, upquote
###############################################################################


setwd('..')
document(thePackage)
path <- find.package(thePackage)
system(paste(shQuote(file.path(R.home("bin"), "R")), "CMD", "Rd2pdf", shQuote(path)))


#release code
setwd('algaeClassify/')
# use_readme_md()
# spell_check()
# check_rhub()
#check_win_devel
usethis::use_build_ignore('./cran-comments.md')

release()
