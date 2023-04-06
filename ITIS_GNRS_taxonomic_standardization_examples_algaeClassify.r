#Vijay Patil 
#3/16/2022

#demo algaeClassify script for standardizing taxonomic names, looking for current synonyms, 
#and pulling currently accepted taxonomy from ITIS

#install the devtools package if not already available
#comment out next line after first install.
# install.packages('devtools')
library(devtools)

#now install algaeClassify- comment out next line after install.
install_github("vppatil/GEISHA_phytoplankton/package builds/algaeClassify",ref="working")
library(algaeClassify) #must be version 1.4.0
library(taxize)
library(ritis)

#view documentation for the new functions:

#these use the global names resolution service (GNRS)
#and the ITIS taxonomic database

#they also rely on functions from the ritis and taxize R packages.

?algaeClassify::gnr_simple #single species name: check for full or partial match in many taxonomy databases
?algaeClassify::gnr_simple_df #apply gnr_simple to a data.frame with many species names
?algaeClassify::genus_search_itis #search ITIS for a genus name. 
                                  #get current accepted name, synonyms, and higher taxonomy
?algaeClassify::species_search_itis #search ITIS for a binomial species name. 
                                    #get current accepted name, synonyms, and higher taxonomy
?algaeClassify::itis_search_df #appply species_search_itis or genus_search_itis
                                #to a data.frame containing many names.
data("lakegeneva")#example dataset

phyto.data<-lakegeneva
phyto.names<-data.frame(phyto_name=unique(phyto.data$phyto_name))

dim(phyto.names)
phyto.names<-genus_species_extract(phyto.names,"phyto_name")#cleanup names
#removes prefixes, suffixes, but retains binomial names plus var/subspp if present.

#paste genus and species back together in a single column, separated by space.
phyto.names$genus_species<-trimws(paste(phyto.names$genus,phyto.names$species))

#look at the data
head(phyto.names)

#1- check for binomial name matches in itis- all species.
start=Sys.time()
speciesmatch.itis<-itis_search_df(phyto.names,namecol = "genus_species",genus.only = FALSE)
#or set higher=TRUE to extract higher taxonomic classifications
#speciesmatch.itis<-itis_search_df(phyto.names,namecol = "genus_species",genus.only = FALSE,higher=TRUE)

stop=Sys.time()

elapse=stop-start
elapse

#about 1 sec per record. So, it will take some time for a long species list.

#accepted.name is best result- indicates currently accepted. will be NA if not present
#orig.name.accepted=0 indicates that the accepted name is different from originally entered name
# orig.name.accepted=1 if original name is a currently accepted taxonomic name.
#check out the result
head(speciesmatch.itis)
table(speciesmatch.itis$match)
table(speciesmatch.itis$orig.name.accepted) 
#so only about half the names have exact matches in the itis database.
#but of those, all are currently accepted taxonomically.

#2- check for genus matches in itis- all taxa
genusmatch.itis<-itis_search_df(phyto.names,namecol = "genus",genus.only = TRUE)
#higher=TRUE argument works for genus-only searches too.

head(genusmatch.itis)
table(genusmatch.itis$match)
table(genusmatch.itis$orig.name.accepted)

#all but 1 genus can be found in ITIS.

#3- check for partial/fuzzy matches in ITIS using gnrs
species.namecheck.gnrs.itis<-gnr_simple_df(phyto.names,name.column = "genus_species",sourceid = 3)
head(species.namecheck.gnrs.itis)
table(is.na(species.namecheck.gnrs.itis$matched.name))
table(species.namecheck.gnrs.itis$exact.match)
#all but one submitted binomial name have a partial/fuzzy match in ITIS
#but proceed with caution- check to make sure the matched name makes sense.
#num hits = 1 for each name. because we only looked in one database (ITIS). score indicates reliability of match

#4 check for partial/fuzzy matches in ITIS using gnrs, genus only
genus.namecheck.gnrs.itis<-gnr_simple_df(phyto.names,name.column = "genus",sourceid = 3)
head(genus.namecheck.gnrs.itis)
table(is.na(genus.namecheck.gnrs.itis$matched.name))
table(genus.namecheck.gnrs.itis$exact.match)

#still 30/31 for matches in ITIS when we only look at genus.
#so restricting to genus doesn't give us a match for that one taxa

#6 check for partial/fuzzy matches in all sources using gnrs, genus only
#for list of sources, see taxize::gnr_datasources()
#to get a complete list of matches in gnrs, use taxize::gnr_resolve()
genus.namecheck.gnrs.allsource<-gnr_simple_df(phyto.names,name.column = "genus",sourceid = NULL)
head(genus.namecheck.gnrs.allsource)
table(is.na(genus.namecheck.gnrs.allsource$matched.name)) #found a match for all names
table(genus.namecheck.gnrs.allsource$exact.match)

#let's look at that one hard to find taxa
tough.name=genus.namecheck.gnrs.allsource$matched.name[is.na(genus.namecheck.gnrs.itis$matched.name)]
phyto.names[grep(tough.name,phyto.names$genus_species),]
#Hyaloraphidium contortum

#try searching this name in gnrs to see where it was found
gnr_resolve("Hyaloraphidium contortum")


#########################################################################################
#####example workflow- once we have tried all the options,
##let's add the best names to the original dataset and extract higher taxonomy
phyto.names$best.name<-speciesmatch.itis$matched.name
    
#for the names without a perfect match, let's use the fuzzy matching results for itis
missing.itis.speciesmatch=is.na(phyto.names$best.name)
phyto.names$best.name[missing.itis.speciesmatch]=species.namecheck.gnrs.itis$matched.name[missing.itis.speciesmatch]
    
#if there wasn't a fuzzy itis match for the full binomial, use the genus only name from all gnrs sources    
missing.itis.genusmatch=is.na(phyto.names$best.name)
phyto.names$best.name[ missing.itis.genusmatch]=genusmatch.itis$matched.name[ missing.itis.genusmatch]

#finally, let's use the partial genus match from all gnrs sources.
missing.itis.genusmatch2=is.na(phyto.names$best.name)
phyto.names$best.name[ missing.itis.genusmatch]=genus.namecheck.gnrs.allsource$matched.name[ missing.itis.genusmatch]

#no NA's left
table(is.na(phyto.names$best.name))

#now, we can extract higher taxonomic names using itis_search_df
highertaxonomy.itis<-itis_search_df(phyto.names,namecol = "best.name",genus.only = FALSE,higher = TRUE)
head(highertaxonomy.itis)

  
