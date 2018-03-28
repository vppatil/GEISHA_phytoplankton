#functions to add nico functionalgroup classifications to a phytoplankton dataframe
#will need to load his crossmap as a data file associated with the package.

#change the next line to the working directory where the sppMFG.rda file is saved.
load('sppMFG.rda')

species.phyto.convert<-function(genus,species)
{
  #accepts two character arguments (a genus and species name), and returns a morphofunctional group classification
  
  species.mfg.library<-species.mfg.library[!duplicated(species.mfg.library),]
  
  genus=gsub('Unknown ','',genus)
  if(species %in% species.mfg.library$species==F){species=''}#replacing spp., sp. etc. with ''
  
  #check for genus and species match first.
  mfg=species.mfg.library$MFG[species.mfg.library$genus==genus & species.mfg.library$species==species]
  #go to genus match 
  mfg=ifelse(length(unique(mfg)==1),unique(mfg),species.mfg.library$MFG[species.mfg.library$genus==genus])
  #return NA if no match
  mfg<-ifelse(length(mfg)==0,NA,mfg)
  return(mfg)
}

phyto.convert.df=function(phyto.df)
{
  #this function applies the nico.phyto.convert function to an entire data frame, but that data frame must have columns named 'genus' and 'species'
  mfgs<-vector(length=dim(phyto.df)[1])
  for(i in 1:dim(phyto.df)[1])
  {
    mfgs[i]=species.phyto.convert(phyto.df$genus[i],phyto.df$species[i])
  }
  phyto.df$MFG=mfgs
  #phyto.df$MFG.number=sapply(as.character(mfgs),function(x) sapply(strsplit(x,split='-',fixed=T),"[",1))
  return(phyto.df)
}

genus.species.extract<-function(phyto.df,phyto.name)
{
  spp.list<-as.character(phyto.df[[phyto.name]])
  
  orig.spp.list=spp.list
  
  spp.list=iconv(spp.list, to='ASCII//TRANSLIT')
  
  spp.list=gsub('Cfr. ','',spp.list,ignore.case=T)
  spp.list=gsub('cf ','',spp.list,ignore.case=T)
  
  #spp.list=gsub('comb. nov.','',spp.list,ignore.case=T) #what the hell does this mean
  
  ###cleaning up genus only records
  genus.only.flag=rep(0,length(spp.list)) #flag for species names with spp. or sp. in them
  genus.only.flag[grep(' sp | sp. | spp | spp. | sp.$| spp.$| sp$| sp1| spp$',spp.list,ignore.case=T)]=1 #doesn't account for sp or spp at end of string
  
  ###flagging and cleaning up records with subspecies or vars
  ###good god these are all the different abbreviations for variety subspecies that I found in 1 dataset!
  ###have to account for abbreviations with and without capitalization and upper/lower case
  var.flag=rep(0,length(spp.list)) #vector to indicate if a species name includes a var/subsp/etc.
  var.flag[grep(' var. | subsp. | ssp. | v. | morph. | gr. | mor. | var | subsp | ssp | v | morph | gr | mor | aff. | aff | f | f. ',spp.list,ignore.case=T)]=1
  spp.list=gsub(' var. | subsp. | ssp. | v. | morph. | gr. | mor. | var | subsp | ssp | v | morph | gr | mor | aff. | aff | f | f. ',' ',spp.list,ignore.case=T)
  
  ###trimming leftover trailing and leading whitespace
  spp.list=trimws(spp.list,'both')
  
  genus=sapply(spp.list,function(x) strsplit(x,split=' ')[[1]][1])
  species=sapply(spp.list,function(x) strsplit(x,split=' ')[[1]][2])
  species[is.na(species)]=''
  species=ifelse(species %in% c('sp.','spp.','sp','spp'),'',species)
  
  #for the search url, species and var are pasted together with a plus sign. will have to be stripped out if you are returning the search species name.
  
  var=sapply(spp.list,function(x) strsplit(x,split=' ')[[1]][3])
  #var[grep('comb. nov.',orig.spp.list)]='comb.+nov.'
  
  #second var flag. will catch instances when a var/subspecies is listed with no preceeding abbreviation. but could also pick up instances when the type designator is listed. 
  # var.flag2=rep(0,length(spp.list)) 
  # var.flag2[!is.na(var)]=1
  
  species[var.flag==1 & !is.na(var)]=paste(species[var.flag==1 & !is.na(var)],var[var.flag==1 & !is.na(var)],sep=' ')
  #for the search url, species and var are pasted together with a plus sign. will have to be stripped out if you are returning the search species name.
  
  genus[is.na(genus)]=''
  species[is.na(species)]=''
  species[genus.only.flag==1]=''
  
  ###now for the comparison with nico's original mfg/species library
  ####need to fix nico function so you can specify the name of the genus and species columns
  phyto.df$genus=genus
  phyto.df$species=species
  
  return(phyto.df)
}



