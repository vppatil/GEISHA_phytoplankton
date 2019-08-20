#' fuzzy partial matching between a scientific name and a list of possible matches
#'
#' @param enteredName Character string with name to check
#' @param possibleNames Character vector of possible matches
#' @param maxErr maximum number of different bits allowed for a partial match
#'
#' @export bestmatch
#'
#' @return a character string with the best match, or 'multiplePartialMatches' if no best match
#'
#' @examples
#' possibleMatches=c('Viburnum edule','Viburnum acerifolia')
#' bestmatch(enteredName='Viburnum edulus',possibleNames=possibleMatches)

bestmatch=function(enteredName,possibleNames,maxErr=3)
{
  for(i in 0:maxErr)
  {
    match=agrep(enteredName,possibleNames,max.distance=i,value=TRUE,trunc=FALSE)
    if(length(match)==1) {return(match)}
    if(length(match)>1)
    {
      if(i==0)
      {
		exact.match=(match==enteredName)
		if(sum(exact.match)==1)
		{
			return(enteredName)
		}
      }else
      {
        return('multiplePartialMatch')

      }
    }
  }
  ##strip last three letters and try again
  if(trunc==FALSE)
  {
    len=nchar(enteredName)
    truncName=substr(enteredName,1,len-3)
    trunc=TRUE
    bestmatch(truncName,possibleNames,max.distance=i,value=T,trunc=TRUE)
  }else{
    return(NA)
  }
}

#' Compare a genus and species name against the algaebase online database
#'
#' @param genus Character string
#' @param species Character string
#' @param long if TRUE, returns higher taxonomy (Kingdom through Family)
#'
#' @export algae_search
#'
#' @import RCurl
#' @import httr
#' @import XML
#' @importFrom rvest html_nodes
#' @importFrom rvest html_text
#' @import plyr
#' @import taxize
#' @importFrom utils write.csv
#' @importFrom stats rnbinom
#' @importFrom xml2 read_html
#'
#' @return A data.frame with the original name, binary flags indicating an exact match and an
#' currently accepted name, potential accepted synonyms, and the best accepted match as a single
#' string and as separate genus and species strings. Higher taxonomy may also be appended if long=T
#'
#' @examples
#' algae_search(genus='Anabaena',species='flos-aquae',long=TRUE)
#'
#' @seealso \url{http://www.algaebase.org} for phytoplankton taxonomy database,
#'     \url{https://powellcenter.usgs.gov/geisha} for project information

algae_search=function(genus,species='',long=F)
{
	#if only a genus is entered, or species is blank, it will execute a genus search.
	status=0 #this variable is used to flag search results based on whether a single page or a table of results is returned by the website.
	species=ifelse(species %in% c('sp.','spp.','sp','spp'),'',species)

		URL=paste('www.algaebase.org/search/?species=',
        genus,'%20',
        species,sep='')

	groups = c('Empire','Kingdom','Phylum','Class','Order','Family')

    if(is.na(genus)|genus=='NA'|genus=='NaN'|genus=='na'|genus=='')
    {
      res.df=data.frame(genus=NA,species=NA,exact.match=0,synonyms=NA,orig.name=paste(genus,species),match.name=NA)
      if(long)
      {
        res.df$Empire=res.df$Kingdom=res.df$Phylum=res.df$Class=res.df$Order=res.df$Family=NA
      }
      return(res.df)
    }

	url.get=GET(URL)

	parsed=htmlParse(url.get)
	plain.text <- xpathSApply(parsed, "//p", xmlValue)
	status=plain.text[grep('Status',plain.text)]
	if(length(status)==0)
	{
		status=0
	}else if(length(grep("This name is of an entity that is currently accepted taxonomically",status))==1) #single page result with exact match
	{
		status=1
	}else if(length(grep('This name is currently regarded as a synonym of ',status))==1) #single page result. entered name is synonym
	{
	  match.name=status
	  status=2
	}else
	{
	  status=0
	}

	if(status==1)
	{
		res.genus=genus
		res.species=species
		exact.match=1
		orig.name=paste(genus,species)
		match.name=paste(genus,species)
		res.synonyms=plain.text[grep('Synonym',plain.text)]
		res.synonyms=ifelse(grepl('Homotypic',res.synonyms,fixed=T),
							gsub('Homotypic Synonym(s)','',res.synonyms,fixed=T),
							gsub('Synonym(s)','',res.synonyms,fixed=T))
		res.synonyms=gsub('Heterotypic ','',res.synonyms,fixed=T)
		res.synonyms[grepl('No synonym',res.synonyms,fixed=T)]=''
		res.df=data.frame(genus=res.genus,species=res.species,exact.match=exact.match,synonyms=paste(res.synonyms,collapse=','),orig.name=orig.name,match.name=match.name)
		if(long)
		{
		  details.parsed=xml2::read_html(url.get)
		  classification.node<-html_nodes(details.parsed,xpath="//p")[[1]]
		  taxa.levels = html_text(html_nodes(classification.node,"i"))
		  taxa=	html_text(html_nodes(classification.node,"a"))
		  df=data.frame(rbind(taxa))
		  names(df)=taxa.levels
		  df=df[,match(groups,names(df))] #make sure there is a consistent set of names

		  res.df<-cbind(res.df,df)
		}
		return(res.df)
	}else if(status==2)###single page match, but submitted name is a synonym of an accepted name
	{
		match.name=gsub('Status of nameThis name is currently regarded as a synonym of ','',match.name)
		res.genus=strsplit(match.name,split=' ')[[1]][1]
		res.species=strsplit(match.name,split=' ')[[1]][2]

		exact.match=0
		orig.name=paste(genus,species)
		match.name=paste(res.genus,res.species)
		res.synonyms=orig.name

		res.df=data.frame(genus=res.genus,species=res.species,exact.match=exact.match,synonyms=paste(res.synonyms,collapse=','),orig.name=orig.name,match.name=match.name)
		if(long)
		{
		  details.parsed=xml2::read_html(url.get)
		  classification.node<-html_nodes(details.parsed,xpath="//p")[[1]]
		  taxa.levels = html_text(html_nodes(classification.node,"i"))
		  taxa=	html_text(html_nodes(classification.node,"a"))
		  df=data.frame(rbind(taxa))
		  names(df)=taxa.levels
		  df=df[,match(groups,names(df))] #make sure there is a consistent set of names

		  res.df<-cbind(res.df,df)
		}

		return(res.df)
	}

	tabs=readHTMLTable(parsed)
	if(length(tabs)==0)
	{
	  res.df=data.frame(genus=NA,species=NA,exact.match=0,synonyms=NA,orig.name=paste(genus,species),match.name=NA)
      if(long)
      {
        res.df$Empire=res.df$Kingdom=res.df$Phylum=res.df$Class=res.df$Order=res.df$Family=NA
      }
      return(res.df)

	}

	results.tab=data.frame(tabs[[1]],stringsAsFactors = F)

	genus.match=grepl(genus,results.tab[[1]])
	if(sum(genus.match)==0)
	{
	  res.df=data.frame(genus=NA,species=NA,exact.match=0,synonyms=NA,orig.name=paste(genus,species),match.name=NA)
      if(long)
      {
        res.df$Empire=res.df$Kingdom=res.df$Phylum=res.df$Class=res.df$Order=res.df$Family=NA
      }
      return(res.df)

	}
	results.tab<-results.tab[genus.match,]

	if(dim(results.tab)[1]==0)
  {
	  res.df=data.frame(genus=NA,species=NA,exact.match=0,synonyms=NA,orig.name=paste(genus,species),match.name=NA)
	  if(long)
	  {
	    res.df$Empire=res.df$Kingdom=res.df$Phylum=res.df$Class=res.df$Order=res.df$Family=NA
	  }
	  return(res.df)
	};

	 colnames(results.tab)[2]='Current name if different'
	 results.tab2=results.tab

	 if(length(grep('Unchecked',as.character(results.tab$Name)))>0)
	 {
	   results.tab2=results.tab[-grep('Unchecked',results.tab$Name),]
	 }

	 res.names=as.character(results.tab2$Name)
	 if(species=='')
	 {
	                  res.names=sapply(res.names,function(x){
	                   return(strsplit(x,split=' ')[[1]][1])
	                  })
	 }else
	 {
	     res.names=sapply(res.names,function(x){return(paste(
	     strsplit(x,split=' ')[[1]][1],
	     strsplit(x,split=' ')[[1]][2]))
	   })
	 }

	 sub.name=ifelse(species=='',genus,paste(genus,species))

	 match.name=bestmatch(sub.name,unique(res.names))
	 match.rows=res.names %in% match.name
	 match.tab=results.tab2[match.rows,]

   res.synonyms=paste(unique(match.tab[,2]),collapse=',')
   if(species==''){res.synonyms=''}

   exact.match=ifelse(match.name==sub.name,1,0)

   res.genus=ifelse(match.name=='multiplePartialMatch','',strsplit(match.name,split=' ')[[1]][1])
   res.species=ifelse(match.name=='multiplePartialMatch','',strsplit(match.name,split=' ')[[1]][2])

	 #check if there are any verified names.
	 res.df=data.frame(genus=res.genus,species=res.species,exact.match=exact.match,synonyms=res.synonyms,orig.name=sub.name,match.name=match.name)

	if(long)
	{
	  if(status==1)
	  {
	    details.parsed=xml2::read_html(url.get)
	  }else
	  {
	    links.parsed=xpathSApply(parsed,"//a/@href")

	    links.parsed<-links.parsed[grep('results',links.parsed)]
	    links.parsed<-links.parsed[seq(1,length(links.parsed),by=2)]
		links.parsed=links.parsed[genus.match]
		good.link=links.parsed[grep(match.name,results.tab$Name)[1]]
	    good.link=paste('www.algaebase.org',good.link,sep='')

	    details=GET(good.link)
	    details.parsed=xml2::read_html(details)
	  }

	  classification.node<-html_nodes(details.parsed,xpath="//p")[[1]]
	  taxa.levels = html_text(html_nodes(classification.node,"i"))
	  taxa=	html_text(html_nodes(classification.node,"a"))
	  df=data.frame(rbind(taxa))
	  names(df)=taxa.levels
	  df=df[,match(groups,names(df))] #make sure there is a consistent set of names
	  res.df<-cbind(res.df,df)
	}

	if(is.na(species) | is.null(species) | species %in% c('','sp','sp.','spp.'))
	{
	  res.df$synonyms=res.df$species=''
	}

	return(res.df)
}


#' Wrapper for applying algae_search function to a data.frame that contains phytoplankton species
#'
#' @param phyto.df data.frame containing genus and species columns or a column with binomial names
#' @param lakename Character string with waterbody name for naming output files
#' @param phyto.name Name or number of column that contains binomial names
#' @param long TRUE/FALSE: should higher taxonomy (Kingdom:Family) be included in output?
#' @param write TRUE/FALSE: should output be written as .csv file?
#'
#' @export spp_list_algaebase
#'
#' @return A character string of the species' morphofunctional group
#'
#' @examples
#' data(lakegeneva)
#' lakegeneva=lakegeneva[1:3,] ##use 3 rows for testing
#' lakegeneva<-genus_species_extract(lakegeneva,phyto.name='phyto_name')
#' lakegeneva.algaebase<-spp_list_algaebase(lakegeneva,write=FALSE)
#'
#' @seealso \url{http://www.algaebase.org} for up-to-date phytoplankton taxonomy,
#'     \url{https://powellcenter.usgs.gov/geisha} for project information

spp_list_algaebase=function(phyto.df,lakename='',phyto.name='phyto_name',long=F,write=T)
{
  options(stringsAsFactors=F)

  if(!('genus' %in% names(phyto.df) & 'species' %in% names(phyto.df)))
  {

    spp.list<-as.character(phyto.df[[phyto.name]])
    spp.list<-unique(spp.list)

    orig.spp.list=spp.list
    spp.list=gsub('Cfr. ','',spp.list,ignore.case=T)
    #spp.list=gsub('comb. nov.','',spp.list,ignore.case=T) #what the hell does this mean

    ###cleaning up genus only records
    genus.only.flag=rep(0,length(spp.list)) #flag for species names with spp. or sp. in them
    genus.only.flag[grep(' sp | sp. | spp | spp. | sp.$| spp.$| sp$| spp$',spp.list,ignore.case=T)]=1 #doesn't account for sp or spp at end of string

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

    spp.list[genus.only.flag==1]=genus[genus.only.flag==1]

    var=sapply(spp.list,function(x) strsplit(x,split=' ')[[1]][3])
    var[grep('comb. nov.',orig.spp.list)]='comb.+nov.'

    #second var flag. will catch instances when a var/subspecies is listed with no preceeding abbreviation. but could also pick up instances when the type designator is listed.
    # var.flag2=rep(0,length(spp.list))
    # var.flag2[!is.na(var)]=1

    species[var.flag==1 & !is.na(var)]=paste(species[var.flag==1 & !is.na(var)],var[var.flag==1 & !is.na(var)],sep='+')
    #for the search url, species and var are pasted together with a plus sign. will have to be stripped out if you are returning the search species name.
  }else
  {
    genus=phyto.df$genus
    species=phyto.df$species
    orig.spp.list=trimws(paste(genus,species,sep=' '))
  }

    genus[is.na(genus)]=''
    species[is.na(species)]=''

  # sleep.times=rnbinom(n=length(genus),size=15,mu=15) #generating pauses between searches based on a neg binomial distribution.

  gen.spp=cbind(genus,species)

  algae_search_robust=function(genus,species,long)###function to try a name again if it hangs up for some weird reason. if it tries twice it will fill with NA and move on to the next name.
  {
    tryCatch(algae_search(genus,species,long),warning=function(w)
      {
        print("retrying function")
      },error= function(e)
                          {
                              Sys.sleep(1);
                              spellcheck=gnr_resolve(paste(genus,species),best_match_only=T,canonical=T,with_context=T)$matched_name2
                              spellcheck.df=data.frame(phyto_name=spellcheck)
                              try(spp_list_algaebase(spellcheck.df,lakename=lakename,long=long))})
  }

agg.list=algae_search_robust(gen.spp[1,1],gen.spp[1,2],long=long)#need to make colclasses = character.
if(dim(gen.spp)[1]>1)
{
  for(i in 2:dim(gen.spp)[1])#doing it as a loop with a pause in the middle to avoid overloading servers.
  {
    temp=algae_search_robust(gen.spp[i,1],gen.spp[i,2],long=long)
    #re-attach original name in orig.name column
	temp$orig.name=orig.spp.list[i]

	agg.list[i,]=temp

    #uncomment the two lines below if there is an error and you want to see which rows ran successfully.
	#then re-source the function and try again.
	 print(i)
	 Sys.sleep(1)
  }
}

  if(write){
	  save(agg.list,file=paste(lakename,'.rda',sep=''))
	  write.csv(agg.list,paste(lakename,'AlgaebaseNames.csv',sep=''))
  }
  return(agg.list)
}


