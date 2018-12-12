setwd('~/gleon/Geisha/datasets/phyto_data/')
unique.spp<-vector()
lakes=dir(pattern='RawPhyto')
library(taxize)
library(algaeClassify)

for(i in 1:length(lakes))
{
    lake<-read.csv(lakes[i],stringsAsFactors=F)
    lake<-genus_species_extract(lake,'phyto_name')
    lake$phyto_name=paste(lake$genus,lake$species)
    lake$phyto_name<-trimws(lake$phyto_name,'both')
    spp=unique(lake$phyto_name)
    unique.spp=c(unique.spp,spp)
}
unique.spp.tab<-table(unique.spp)
unique.spp<-data.frame(unique.spp.tab)
unique.spp$unique.spp=as.character(unique.spp$unique.spp)
unique.spp=unique.spp[order(unique.spp$Freq),]

unique.genera=unique.spp[-grep(' ',unique.spp[,1]),]

################################################################################
#firstpart- get class and order from ncbi
#extend to families too

test=unique.spp[1:10,1]

unique.class=vector(length=dim(unique.spp)[1])
unique.order=vector(length=dim(unique.spp)[1])
unique.family=vector(length=dim(unique.spp)[1])

for(i in 1:length(unique.class))
{
  print(i)
  class=tax_name(unique.spp[i,1],get='class',db='both')
  order=tax_name(unique.spp[i,1],get='order',db='both')
  family=tax_name(unique.spp[i,1],get='family',db='both')
  
  class=unique(na.omit(class))[1]
  order=unique(na.omit(order))[1]
  family=unique(na.omit(order))[1]
  
  unique.class[i]=class
  unique.order[i]=order
  unique.family[i]=family
  
}

# unique.spp<-cbind(unique.spp,unique.class,unique.order)

highertaxa=cbind(unique.spp[,1],unique.class,unique.order,unique.family)
setwd('../uniquesppnames/')
write.csv(highertaxa,'uniquesppclassorder.csv')
save.image('uniqueclassorderTaxize.RData')
###############################################################################################
#namecheck1
gnr.names=gnr_resolve(unique.spp[,1],canonical=T,best_match_only = T)
names(gnr.names)[1]='phyto_name'
gnr.names=gnr.names[,c(1,3,4,5)]
names(unique.spp)[1]='phyto_name'
gnr.names<-merge(unique.spp,gnr.names,all.x=T)
gnr.names$perfectmatch=ifelse(gnr.names$phyto_name==gnr.names$matched_name2)
write.csv(gnr.names,'uniquespp_gnrnames.csv')
save.image('gnrsNames.RData')

############################################################################################
###algaebase check for mismatches
source('~/gleon/Geisha/phyto_package/GEISHA_phytoplankton_github_shared/algaebase_query_v2.r')
mismatches=gnr.names[gnr.names$perfectmatch==0,]
mismatches<-genus_species_extract(mismatches,'phyto_name')

###pull out size information and colony information if present
names(unique.spp)[1]='phyto_name'
larges=grep('large',unique.spp$phyto_name)
small=grep('small',unique.spp$phyto_name)
num.indices=grep('[0-9]',unique.spp$phyto_name)
num.max<-function(x) {
  x<-gsub('?"','-',x,fixed=T)
  splits=strsplit(x,split='-',fixed=T)[[1]]
  splits=gsub("[^0-9]","",splits)
  splits=as.numeric(splits)
  max.split=max(splits)
  max.split[is.na(max.split)]=''
  return(as.numeric(max.split))
}

numbers=sapply(unique.spp$phyto_name,num.max)
unique.spp$maxsize=numbers
unique.spp$size=''
unique.spp$size[larges]='large'
unique.spp$size[small]='small'
unique.spp$size[!is.na(unique.spp$maxsize) & unique.spp$maxsize<35]='small'
unique.spp$size[!is.na(unique.spp$maxsize) & unique.spp$maxsize>=35]='large'

setwd('../uniquesppnames/')
write.csv(unique.spp,'uniqueSizeinfo.csv')
