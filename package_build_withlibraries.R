###############################################################################
# package creation & maintenance file
# M.J. Lajeunesse, R course 7/5/16
###############################################################################
rm(list=ls())
# define the package name
thePackage <- "algaeClassify"

# set directory to the package development folder
setwd("C:/Users/vpatil/OneDrive - DOI/Geisha_main/phyto_package/GEISHA_phytoplankton_github_shared/package builds/")

# create new bare-bones package in development folder
# NOTE: only run package creation script once
#install.packages("devtools"); # make sure your have the most recent version
#install.packages("roxygen2"); # make sure your have the most recent version
library(devtools); library(roxygen2)
# create(thePackage)
setwd('algaeClassify')
rm(list='traitranges')
roxygenize('.')


# STEP 2, add functionality ###################################################
# (1) update DESCRIPTION file (package description, authors, dependencies)
# (2) add functions (*.R) in R folder of package
# (3) within *.R files should include roxygen2 markdown of function and 
#     parameter txt (e.g., what you see in manual) 
###############################################################################

# add dataset to package
lakegeneva <- read.csv('../../lakegeneva.csv')
# use_data(library_MFG, pkg = thePackage, internal = TRUE, overwrite = TRUE)
use_data(lakegeneva, overwrite = TRUE)

rimet_mfgtraits<-read.csv('C:/Users/vpatil/OneDrive - DOI/Geisha_main/phyto_package/GEISHA_phytoplankton_github_shared/Frederic_MFGtraits.csv')
mfgTraits=rimet_mfgtraits[!is.na(rimet_mfgtraits$MFG.fromtraits),]

.simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s, 2),
        sep = "", collapse = " ")}
  
names(mfgTraits)<-sapply(names(mfgTraits),.simpleCap)
names(mfgTraits)[1]='phyto_name'
names(mfgTraits)[2:3]=c('genus','species')
use_data(mfgTraits,overwrite=TRUE)

fred.csrtraits<-read.csv('C:/Users/vpatil/OneDrive - DOI/Geisha_main/phyto_package/GEISHA_phytoplankton_github_shared/Fred_CSRtraits_numericClassCorrection.csv')
csrTraits<-fred.csrtraits
csrTraits<-genus_species_extract(csrTraits,'phyto_name')
csrTraits$phyto_name<-paste(csrTraits$genus,csrTraits$species)
csrTraits$phyto_name<-trimws(csrTraits$phyto_name)

use_data(csrTraits,overwrite=TRUE)

mfg_csr_library<-read.csv('C:/Users/vpatil/OneDrive - DOI/Geisha_main/phyto_package/mee ms/mfg_csr_library.csv')
use_data(mfg_csr_library,overwrite=TRUE)


# setwd('C:/Users/vpatil/OneDrive - DOI/Geisha_main/phyto_package/GEISHA_phytoplankton_github_shared/package builds/')# use_data(library_MFG, pkg = thePackage, internal = TRUE, overwrite = TRUE)
# 
# load('sppMFG.rda')
# setwd('data/')
# load('species.mfg.library.rda')
# setwd('..')
# use_data(species.mfg.library, overwrite = TRUE)

setwd('C:/Users/vpatil/OneDrive - DOI/Geisha_main/phyto_package/GEISHA_phytoplankton_github_shared/')
load('traitranges.rda')
traitranges=traitranges[1:3,]
traitranges$units=c('um^-1','um','')

use_data(traitranges, overwrite = TRUE)

setwd('C:/Users/vpatil/OneDrive - DOI/Geisha_main/jennie MFG/')
species_mfg_library<-read.csv('expanded_mfg_library_2_1_2021.csv')
species_mfg_library<-subset(species_mfg_library,select=c('genus','species','MFG','source'))
species_mfg_library$MFG<-gsub('6a-LargeCent','6a1-LargeCent',species_mfg_library$MFG)
use_data(species_mfg_library,overwrite=TRUE)
setwd('C:/Users/vpatil/OneDrive - DOI/Geisha_main/phyto_package/GEISHA_phytoplankton_github_shared/package builds/algaeClassify/')

rm(list='traitranges')
# STEP 3, error check and compile package for CRAN ############################
# (1) update DESCRIPTION file
# (2) add functions (*.R) in R folder of package
###############################################################################

# constructs binaries 
library(devtools); 
# creates bundle to submit to CRAN (*.tar.zip file found in development folder)
build('.', manual = FALSE)
# error checks prior to submission (all errors and warnings need to be addressed)



rm(list=ls())
# set directory to the package development folder
setwd("C:/Users/vpatil/OneDrive - DOI/Geisha_main/phyto_package/GEISHA_phytoplankton_github_shared/package builds/")

# create new bare-bones package in development folder
# NOTE: only run package creation script once
#install.packages("devtools"); # make sure your have the most recent version
#install.packages("roxygen2"); # make sure your have the most recent version
library(devtools); library(roxygen2)
# create(thePackage)
setwd('algaeClassify')

check('.',cran=FALSE)
warning()
# STEP 4, Build example PDF manual (will not be same as CRAN, but close) ######
###############################################################################

# BEFORE STARTING##############################################################
# 1) Download & install MiKTeX to generate manual & vignettes
# 2) through MikTeX /Maintenace/packages install: url, inconsolata, upquote
###############################################################################

setwd('..')
thePackage='algaeClassify'
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
