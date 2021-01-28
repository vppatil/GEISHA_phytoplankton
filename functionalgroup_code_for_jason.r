#load helper library for installing packages from github
# install.packages('devtools')
library(devtools)

#install and load the most recent package version from vijay patil's github account
install_github('vppatil/GEISHA_phytoplankton/package builds/algaeClassify',ref='working',force=TRUE)

library(algaeClassify)

#read in the data file you want to classify
##will need to change this to the data set you want to use. 
setwd('~/gleon/Geisha/datasets/phyto_data/')
lakephyto<-read.csv('RawPhytoData_Acton_QAQC_18Jan2018.csv')


#create a list of unique species names from the lake
lakephytospp<-data.frame(phyto_name=unique(lakephyto$phyto_name))

#separate out genus and species, and clean up name formatting
lakephytospp<-genus_species_extract(lakephytospp,'phyto_name')

#load csr and mfg trait databases
data("csrTraits")
data("mfgTraits")

#merge trait data into species data
csrTraits<-subset(csrTraits,select=c('phyto_name','SAV','MLD','MSV'))
mfgTraits<-mfgTraits[,!names(mfgTraits) %in% c('genus','species','MFG.fromtraits')]

#keep all species- assign NA if no trait values are available
lakephytospp<-merge(lakephytospp,csrTraits,all.x=T)
lakephytospp<-merge(lakephytospp,mfgTraits,all.x=T)

#assign morphofunctional groups based on Traits
lakephytospp$MFGfromTraits<-traits_to_mfg_df(lakephytospp,arg.names = c('Mobility.apparatus','Size','Colonial','Filament','Centric','Gelatinous','Aerotopes','Class','Order'))

#classify the remaining species based on pre-defined libraries.
data("species_mfg_library")
lakephytospp<-species_to_mfg_df(lakephytospp,flag=2) #flag = 2 allows for manual resolution of ambiguous classifications
#flag =1 will just leave the ambiguous classifications = NA
#note that if a match isn't found in the library, it will check for a genus only match
#so if there are many species within the genus, and flag =2, it will appear to ask you to classify the same
#genus multiple times. this is not an error.

#use trait-based classification if available, or library-based classification otherwise
lakephytospp$MFGfromLibrary<-lakephytospp$MFG
lakephytospp$MFG<-ifelse(!is.na(lakephytospp$MFGfromLibrary),lakephytospp$MFGfromLibrary,lakephytospp$MFGfromTraits)

##CSr classification based on trait database
lakephytospp$CSRfromTraits<-traits_to_csr_df(lakephytospp,sav='SAV',msv='MSV')

#classify the remainder based on MFG-CSR cross-map
lakephytospp<-mfg_csr_convert_df(lakephytospp,mfg='MFG')

#use trait-based classification if available, or library-based classification otherwise
lakephytospp$CSRfromLibrary<-lakephytospp$CSR
lakephytospp$CSR<-ifelse(!is.na(lakephytospp$CSRfromLibrary),lakephytospp$CSRfromLibrary,lakephytospp$CSRfromLibrary)

#view the data
lakephytospp
View(lakephytospp)

#write it out
write.csv(lakephytospp,'lakephyto_MFG_CSR_classifications.csv')
