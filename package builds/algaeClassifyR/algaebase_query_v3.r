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
    match=agrep(enteredName,possibleNames,max.distance=i,value=T)
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
#' @import xml2
#' @importFrom rvest html_nodes
#' @importFrom rvest html_text
#' @import plyr
#' @import taxize
#' @importFrom utils write.csv
#' @importFrom stats rnbinom
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
  options(stringsAsFactors=F)
  species.sub=species #keeping track of whether you had to remove something like sp., spp. from entered name

	#if only a genus is entered, or species is blank, it will execute a genus search.
	status=0 #this variable is used to flag search results based on whether a single page or a table of results is returned by the website.
	species=ifelse(is.na(species)|species %in% c(' sp.',' spp.',' sp',' spp'),'',species)

	species=gsub("\\s*\\([^\\)]+\\)","",species)#remove parentheses numbers
	species=gsub('[:0|1|2|3|4|5|6|7|8|9:]','',species) #remove other stray numbers

  if(species==''|is.na(species))
  {
    URL=paste('www.algaebase.org/search/?genus=',genus,sep='')
  }else
  {
    URL=paste('www.algaebase.org/search/?species=',
              genus,'%20',
              species,sep='')
  }
	URL=gsub(' ','+',URL)

	groups = c('Empire','Kingdom','Phylum','Class','Order','Family')

    if(is.na(genus)|genus=='NA'|genus=='NaN'|genus=='na'|genus=='')
    {
      res.df=data.frame(orig.name=trimws(paste(genus,species)),exact.match=0,is.accepted=0,
	  current.accepted.synonyms=NA,match.name=NA,genus=NA,species=NA)
      if(long)
      {
        res.df$Empire=res.df$Kingdom=res.df$Phylum=res.df$Class=res.df$Order=res.df$Family=NA
      }
      return(res.df)
    }

	url.get=GET(URL)

	parsed=htmlParse(url.get)
	plain.text <- xpathSApply(parsed, "//p", xmlValue)
	if(species=='')
	{
	  status=plain.text[grep('Taxonomic status',plain.text)]
	}else
	{
	  status=plain.text[grep('Status',plain.text)]
	}

	if(length(status)==0) #change this to reflect possible hits for genus search
	{
		if(length(grep('This is a preliminary entry',plain.text))>=1)
			{
			  status=3 #unchecked, perfect match with single page of results
			}else
			{
			  status=0
			}
	}else if(length(grep("This name is of an entity that is currently accepted taxonomically",status))==1 |
	         length(grep("recognized as a distinct genus",status))==1) #single page result with exact match
	{
		status=1
	}else if(length(grep('This name is currently regarded as a synonym of ',status))==1) #single page result. entered name is synonym
	{
	  match.name=status
	  status=2
	  has.accepted.synonym=1
	}else
	{
	  status=0
	}

	if(status==1)
	{
		species=gsub('+',' ',species,fixed=T)
		res.genus=genus
		res.species=species
		exact.match=1
		orig.name=trimws(paste(genus,species))
		match.name=trimws(paste(genus,species))

		#not including synonyms, because status = 1 means that the original name is accepted.
		res.synonyms=''

		res.df=data.frame(orig.name=orig.name,exact.match=exact.match,is.accepted=1,current.accepted.synonyms=res.synonyms,match.name=match.name,genus=res.genus,species=res.species)
		if(long)
		{
		  details.parsed=read_html(url.get)
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
		species=gsub('+',' ',species,fixed=T)
		comb.nov=ifelse(length(grep('comb.',species,fixed=T))>0,1,0)
		match.name=gsub('Status of nameThis name is currently regarded as a synonym of ','',match.name)
		res.genus=strsplit(match.name,split=' ')[[1]][1]
		res.species=match.name
		res.species=gsub(res.genus,'',match.name);res.species=trimws(res.species)
		res.species=gsub("\\s*\\([^\\)]+\\)","",res.species)
		#parsing out names with parentheses

		#need to grab var if present.
		res.species.var=length(grep('var. |subsp. |ssp. |v. |morph. |gr. |mor. |var |subsp |ssp |v |morph |gr |mor |aff. |aff |f |f. ',res.species,ignore.case=T))>=1

		if(comb.nov==1)
		{
			res.species=paste(strsplit(res.species,split=' ')[[1]][1],'comb. nov.')
		}else if(res.species.var)
		{
			res.species=gsub('var. |subsp. |ssp. |v. |morph. |gr. |mor. |var |subsp |ssp |v |morph |gr |mor |aff. |aff |f |f. ','',res.species,ignore.case=T)
			res.species=paste(strsplit(res.species,split=' ')[[1]][1],strsplit(res.species,split=' ')[[1]][2],sep=' ')
		}else
		{
			res.species=strsplit(res.species,split=' ')[[1]][1]
		}

		exact.match=1
		orig.name=trimws(paste(genus,species))
		match.name=paste(res.genus,res.species)
		res.synonyms=orig.name

		res.df=data.frame(orig.name=orig.name,exact.match=exact.match,is.accepted=0,current.accepted.synonyms=match.name,match.name=match.name,genus=res.genus,species=res.species)
		if(long)
		{
		  details.parsed=read_html(url.get)
		  classification.node<-html_nodes(details.parsed,xpath="//p")[[1]]
		  taxa.levels = html_text(html_nodes(classification.node,"i"))
		  taxa=	html_text(html_nodes(classification.node,"a"))
		  df=data.frame(rbind(taxa))
		  names(df)=taxa.levels
		  df=df[,match(groups,names(df))] #make sure there is a consistent set of names

		  res.df<-cbind(res.df,df)
		}

		return(res.df)
	}else if(status==3) #exact but unchecked match.
	{
		species=gsub('+',' ',species,fixed=T)
		res.genus=genus
		res.species=species
		exact.match=1
		orig.name=trimws(paste(genus,species))
		match.name=trimws(paste(genus,species))

		res.synonyms=plain.text[grep('Synonym',plain.text)]
		res.synonyms=ifelse(grepl('Homotypic',res.synonyms,fixed=T),
							gsub('Homotypic Synonym(s)','',res.synonyms,fixed=T),
							gsub('Synonym(s)','',res.synonyms,fixed=T))
		res.synonyms=gsub('Heterotypic ','',res.synonyms,fixed=T)
		res.synonyms[grepl('No synonym',res.synonyms,fixed=T)]=''

		res.synonyms=paste(res.synonyms,collapse=',')
		res.synonyms=gsub('^,|,$','',res.synonyms) #strip leading and trailing commas

		is.accepted=0 #unchecked/unverified match.

		res.df=data.frame(orig.name=orig.name,exact.match=1,is.accepted=0,current.accepted.synonyms=res.synonyms,match.name,genus=res.genus,species=res.species)
		if(long)
		{
		  details.parsed=read_html(url.get)
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
	  res.df=data.frame(orig.name=trimws(paste(genus,species)),exact.match=0,is.accepted=0,current.accepted.synonyms=NA,match.name=NA,genus=NA,species=NA)
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
	  res.df=data.frame(orig.name=trimws(paste(genus,species)),exact.match=0,is.accepted=0,current.accepted.synonyms=NA,match.name=NA,genus=NA,species=NA)
      if(long)
      {
        res.df$Empire=res.df$Kingdom=res.df$Phylum=res.df$Class=res.df$Order=res.df$Family=NA
      }
      return(res.df)

	}

	results.tab<-results.tab[genus.match,]

	if(dim(results.tab)[1]==0)
	{
	  res.df=data.frame(orig.name=trimws(paste(genus,species)),exact.match=0,is.accepted=0,current.accepted.synonyms=NA,match.name=NA,genus=NA,species=NA)
	  if(long)
	  {
	    res.df$Empire=res.df$Kingdom=res.df$Phylum=res.df$Class=res.df$Order=res.df$Family=NA
	  }
	  return(res.df)
	};

	 if(species!=''){colnames(results.tab)[2]='Current name if different'}
	 colnames(results.tab)[1]='Name'
	 results.tab2=results.tab

	 unchecked.flag=rep(FALSE,length(results.tab$Name))
	 unchecked.flag[grep('Unchecked',as.character(results.tab$Name))]=T

	res.names=as.character(results.tab2$Name)

	#removing any stuff in parentheses and any numbers
	res.names=gsub("\\s*\\([^\\)]+\\)","",res.names)#remove parentheses numbers
    res.names=gsub('[:0|1|2|3|4|5|6|7|8|9:]','',res.names) #remove other stray numbers

	 #res.names should have a prefix abbreviation for any var, spp, etc. so, process them to match the entered species names.
	 res.var.flag=rep(0,length(res.names))
	 res.var.flag[grep(' var. | subsp. | ssp. | v. | morph. | gr. | mor. | var | subsp | ssp | v | morph | gr | mor | aff. | aff | f | f. ',res.names,ignore.case=T)]=1
	 res.names=gsub(' var. | subsp. | ssp. | v. | morph. | gr. | mor. | var | subsp | ssp | v | morph | gr | mor | aff. | aff | f | f. ',' ',res.names,ignore.case=T)
	 res.names=trimws(res.names)

	 if(species=='')
	 {
	                  res.names=sapply(res.names,function(x){
	                   return(strsplit(x,split=' ')[[1]][1])
	                  })
	 }else
	 {
		res.names[res.var.flag==0]=sapply(res.names[res.var.flag==0],function(x){return(paste(
	     strsplit(x,split=' ')[[1]][1],
	     strsplit(x,split=' ')[[1]][2]))
	   })
	   res.names[res.var.flag==1]=sapply(res.names[res.var.flag==1],function(x){return(paste(
	     strsplit(x,split=' ')[[1]][1],
	     strsplit(x,split=' ')[[1]][2],
		 strsplit(x,split=' ')[[1]][3]))
	   }) #adding the var names if present, but without the abbreviation prefix. should match the original entered name.
	   res.names=unlist(res.names)
	 }

	 res.names=gsub('flosaquae','flos-aquae',res.names) #inelegant fix for mismatch error for Aphanizomenon flos-aquae. only some of the algaebase names include the hyphen.

	 #also need to parse/process synonyms
	 sub.name=ifelse(species=='',genus,trimws(paste(genus,species)))
	 sub.name=gsub('+',' ',sub.name,fixed=T)

	 match.name=bestmatch(sub.name,unique(res.names))
	 match.name.checked=1
	 if(sum(unchecked.flag)>0)
	{
		if(match.name %in% res.names[unchecked.flag])
		{
			match.name.checked=0
		}
	}

	#if the match name is the same as a name listed as unchecked, it will be flagged as not currently accepted.
	 match.rows=res.names %in% match.name
	 match.tab=results.tab2[match.rows,]

   res.synonyms=unique(as.character(match.tab[,2]))
   if(species==''){res.synonyms=''}

   if(match.name=='multiplePartialMatch')
   {
     res.synonyms=as.character(unique(results.tab2[,2]))
   }

   res.synonyms=res.synonyms[res.synonyms!='']

   res.synonyms=gsub("\\s*\\([^\\)]+\\)","",res.synonyms)#remove parentheses numbers
   res.synonyms=gsub('[:0|1|2|3|4|5|6|7|8|9:]','',res.synonyms) #remove other stray numbers

   res.syn.var.flag=rep(0,length(res.synonyms))
	 res.syn.var.flag[grep(' var. | subsp. | ssp. | v. | morph. | gr. | mor. | var | subsp | ssp | v | morph | gr | mor | aff. | aff | f | f. ',res.synonyms,ignore.case=T)]=1
	 res.synonyms=gsub(' var. | subsp. | ssp. | v. | morph. | gr. | mor. | var | subsp | ssp | v | morph | gr | mor | aff. | aff | f | f. ',' ',res.synonyms,ignore.case=T)
	 res.synonyms=trimws(res.synonyms)

	 	res.synonyms[res.syn.var.flag==0 & res.synonyms!='']=sapply(res.synonyms[res.syn.var.flag==0 & res.synonyms!=''],function(x){return(paste(
	     strsplit(x,split=' ')[[1]][1],
	     strsplit(x,split=' ')[[1]][2]))
	   })
	   res.synonyms[res.syn.var.flag==1 & res.synonyms!='']=sapply(res.synonyms[res.syn.var.flag==1 & res.synonyms!=''],function(x){return(paste(
	     strsplit(x,split=' ')[[1]][1],
	     strsplit(x,split=' ')[[1]][2],
		 strsplit(x,split=' ')[[1]][3]))
	   }) #adding the var names if present, but without the abbreviation prefix. should match the original entered name.
	   res.synonyms=unlist(res.synonyms)
	   res.synonyms=unique(res.synonyms)



	   if(length(res.synonyms[res.synonyms!=sub.name]>0))
	   {
	        res.synonyms=res.synonyms[res.synonyms!=sub.name]
	   }

     num.synonyms=length(res.synonyms)

   if(num.synonyms==1)
   {
	match.name=res.synonyms
   }else if(num.synonyms>1)
   {
		res.synonyms=paste(res.synonyms,collapse=',')
		res.synonyms=gsub('^,|,$','',res.synonyms) #strip trailing and leading commas from list of snynonyms
   }else
	{
		res.synonyms=''
	}

   #now clean up the match name so it is only genus, species, and var (if present)
   match.name=gsub('var. |subsp. |ssp. |v. |morph. |gr. |mor. |var |subsp |ssp |v |morph |gr |mor |aff. |aff |f |f. ','',match.name,ignore.case=T)

   res.genus=ifelse(match.name=='multiplePartialMatch','',strsplit(match.name,split=' ')[[1]][1])
   res.species=ifelse(match.name=='multiplePartialMatch','',strsplit(match.name,split=' ')[[1]][2])
   res.var=ifelse(match.name=='multiplePartialMatch','',strsplit(match.name,split=' ')[[1]][3])
   res.species[is.na(res.species)]=''

   if(!is.na(res.var)){res.species=paste(res.species,res.var)}

   match.name=ifelse(res.species=='',res.genus,paste(res.genus,res.species))

   species.sub=gsub('+','',species.sub,fixed=T)
   exact.match=ifelse(match.name==sub.name & species.sub==species,1,0)

   is.accepted=ifelse((num.synonyms>0 & match.name!=sub.name)|match.name.checked==0,0,1) #flag the name as having accepted synonyms if there is more than one non-blank result in the synonym list. also, list the name as not accepted if it is listed as 'Unchecked' in the result page.

	 res.df=data.frame(orig.name=sub.name,exact.match=exact.match,is.accepted=is.accepted,potential.synonyms=res.synonyms,match.name=match.name,genus=res.genus,species=res.species)

	if(long)
	{
	  if(status==1)
	  {
	    details.parsed=read_html(url.get)
	  }else
	  {
	    links.parsed=xpathSApply(parsed,"//a/@href")

	    links.parsed<-links.parsed[grep('results',links.parsed)]
	    links.parsed<-links.parsed[seq(1,length(links.parsed),by=2)]
		links.parsed=links.parsed[genus.match]
		good.link=links.parsed[grep(match.name,results.tab$Name)[1]]
	    good.link=paste('www.algaebase.org',good.link,sep='')

	    details=GET(good.link)
	    details.parsed=read_html(details)
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
	  res.df$species=''
	  #res.df$current.accepted.synonyms=''

	  #uncomment this second name if it makes sense to blank out synonyms returned for a genus only search.
	}

	return(res.df)
}


#' Wrapper for applying algae_search function to a data.frame that contains phytoplankton species
#'
#' @param phyto.df data.frame containing genus and species columns or a column with binomial names 
#' @param lakename Character string with waterbody name for naming output files
#' @param long TRUE/FALSE: should higher taxonomy (Kingdom:Family) be included in output?
#' @param write TRUE/FALSE: should output be written as .csv file?
#'
#' @export spp_list_algaebase
#' 
#' @return A character string of the species' morphofunctional group
#' 
#' @examples
#' data(lakegeneva)
#' lakegeneva<-genus_species_extract(lakegeneva)
#' lakegeneva.algaebase<-spp_list_algaebase(lakegeneva,long=T)
#' 
#' @seealso \url{http://www.algaebase.org} for up-to-date phytoplankton taxonomy,
#'     \url{https://powellcenter.usgs.gov/geisha} for project information

spp_list_algaebase=function(phyto.df,lakename='',long=F,write=T)
{
  options(stringsAsFactors=F)

  if(!('genus' %in% names(phyto.df) & 'species' %in% names(phyto.df)))
  {

    spp.list<-as.character(phyto.df$phyto_name)
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

  sleep.times=rnbinom(n=length(genus),size=15,mu=15) #generating pauses between searches based on a neg binomial distribution.

  gen.spp=cbind(genus,species)

  algae_search_robust=function(genus,species,long)###function to try a name again if it hangs up for some weird reason. if it tries twice it will fill with NA and move on to the next name.
  {
    tryCatch(algae_search(genus,species,long),warning=function(w)
      {
        print("retrying function")
      },error= function(e)
                          {
                              Sys.sleep(2);
                              spellcheck=gnr_resolve(paste(genus,species),best_match_only=T,canonical=T,with_context=T)$matched_name2
                              spellcheck.df=data.frame(phyto_name=spellcheck)
                              try(spp_list_algaebase(spellcheck.df,lakename=lakename,long=long))})
  }

agg.list=algae_search_robust(gen.spp[1,1],gen.spp[1,2],long=long)#need to make colclasses = character.
if(dim(gen.spp)[1]>1)
{
  for(i in 2:dim(gen.spp)[1])#doing it as a loop with a pause in the middle to avoid overloading servers.
  {
    temp=algae.search.robust(gen.spp[i,1],gen.spp[i,2],long=long)
    #re-attach original name in orig.name column
	temp$orig.name=orig.spp.list[i]

	agg.list[i,]=temp

    #uncomment the two lines below if there is an error and you want to see which rows ran successfully.
	#then re-source the function and try again.
	 print(i)
	 Sys.sleep(sleep.times[i])
  }
}
  save(agg.list,file=paste(lakename,'.rda',sep=''))

  if(write){
	  write.csv(agg.list,paste(lakename,'AlgaebaseNames.csv',sep=''))
  }
  return(agg.list)
}


