#' Split a dataframe column with binomial name into genus and species columns.
#'
#' @param phyto.df Name of data.frame object
#' @param phyto.name Character string: field containing binomial name.
#'
#' @export genus_species_extract
#' 
#' @return A data.frame with new character fields 'genus' and 'species'
#' 
#' @examples
#' data(lakegeneva)
#' #example dataset with 50 rows
#' 
#' head(lakegeneva) #need to split the phyto_name column
#' new.lakegeneva=genus_species_extract(lakegeneva,'phyto_name')
#' 
#' head(new.lakegeneva)

genus_species_extract<-function(phyto.df,phyto.name) 
#phyto.name is character string indicating column containing phytoplankton binomial names
{
  spp.list<-as.character(phyto.df[[phyto.name]])
  
  orig.spp.list=spp.list
  
  spp.list=iconv(spp.list, to='ASCII//TRANSLIT')
  
  spp.list=gsub('Cfr. ','',spp.list,ignore.case=T)
  spp.list=gsub('cf ','',spp.list,ignore.case=T)
  
  ###cleaning up genus-only records
  genus.only.flag=rep(0,length(spp.list)) #flag for species names with spp. or sp. in them
  genus.only.flag[grep(' sp | sp. | spp | spp. | sp.$| spp.$| sp$| sp1| spp$',
					   spp.list,ignore.case=T)]=1 
					   #doesn't account for sp or spp at end of string
  
  ###flagging and cleaning up records with subspecies or vars
  ###good god these are all the different abbreviations for variety subspecies that I found!
  ###have to account for abbreviations with and without capitalization and upper/lower case
  var.flag=rep(0,length(spp.list)) #vector to indicate if a species name includes a var/subsp/etc.
  var.flag[grep(' var. | subsp. | ssp. | v. | morph. | gr. | mor. | var | subsp | ssp | v | morph |
				  gr | mor | aff. | aff | f | f. ',spp.list,ignore.case=T)]=1
				  
  spp.list=gsub(' var. | subsp. | ssp. | v. | morph. | gr. | mor. | var | subsp | ssp | v | morph |
				  gr | mor | aff. | aff | f | f. ',' ',spp.list,ignore.case=T)
  
  ###trimming leftover trailing and leading whitespace
  spp.list=trimws(spp.list,'both')
  
  genus=sapply(spp.list,function(x) strsplit(x,split=' ')[[1]][1])
  species=sapply(spp.list,function(x) strsplit(x,split=' ')[[1]][2])
  species[is.na(species)]=''
  species=ifelse(species %in% c('sp.','spp.','sp','spp'),'',species)
  
  #for the search url, species and var are pasted together with a plus sign. 
  #will have to be stripped out if you are returning the search species name.
  
  var=sapply(spp.list,function(x) strsplit(x,split=' ')[[1]][3])
  #not using line below
  #var[grep('comb. nov.',orig.spp.list)]='comb.+nov.'
  
  #pasting species and subspecies/var into a single string.
  species[var.flag==1 & !is.na(var)]=paste(species[var.flag==1 & !is.na(var)],
		  var[var.flag==1 & !is.na(var)],sep=' ')
  
  genus[is.na(genus)]=''
  species[is.na(species)]=''
  species[genus.only.flag==1]=''
  
  phyto.df$genus=genus
  phyto.df$species=species
  
  return(phyto.df)
}
