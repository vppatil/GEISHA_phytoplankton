#functions for querying a genus + species in algaebase and scraping the resulting web
#page for a match

#website is www.algaebase.org
#for any resulting publications or presentations, please cite as: 
#Guiry, M.D. & Guiry, G.M. 2017. AlgaeBase. World-wide electronic publication, 
#National University of Ireland, Galway. http://www.algaebase.org;



library(RCurl)
library(httr)
library(XML)
library(rvest)
library(plyr)

bestmatch=function(enteredName,possibleNames,maxErr=3,trunc=TRUE)
{
  for(i in 0:maxErr)
  {
    match=agrep(enteredName,possibleNames,max.distance=i,value=TRUE)
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
  if(trunc==TRUE)
  {
    len=nchar(enteredName)
    truncName=substr(enteredName,1,len-3)
    trunc=TRUE
    bestmatch(truncName,possibleNames,trunc=FALSE)
  }else{
    return(NA)
  }
}

algae.search=function(genus,species='',b=F,long=F)
{
	synonym.swapped=0
	#if only a genus is entered, or species is blank, it will execute a genus search.
	status=0 #this variable is used to flag search results based on whether a single page or a table of results is returned by the website.
	species=ifelse(species %in% c('sp.','spp.','sp','spp'),'',species)
	species<-gsub('flos-aquae','flosaquae',species)
	
    ##replacing gaps in species subspp name
	species<-gsub(' ','+',species)
    if(species=='')
	{
		URL=paste('www.algaebase.org/search/?species=',
        genus,sep='')
	}else
	{
		URL=paste('www.algaebase.org/search/?species=',
        genus,'%20',
        species,sep='')
	}
	
	groups = c('Empire','Kingdom','Phylum','Class','Order','Family')
	
    if(is.na(genus)|genus=='NA'|genus=='NaN'|genus=='na'|genus=='')
    {
      res.df=data.frame(genus=NA,species=NA,exact.match=0,accepted=0,synonyms=NA,orig.name=paste(genus,species),match.name=NA)
      if(long)
      {
        res.df$Empire=res.df$Kingdom=res.df$Phylum=res.df$Class=res.df$Order=res.df$Family=NA
      }
      return(res.df)
    }
	
	url.get=GET(URL)
	
	parsed=htmlParse(url.get)
	plain.text <- xpathSApply(parsed, "//p", xmlValue)
	status=plain.text[grep('Status',plain.text)] ##need to work on this part.
	if(length(grep('click on the name',plain.text))>0)
	{
		status=0
	} #first, see if multiple results were returned.
	
	if(length(grep('This is a preliminary entry and has not been subject to full verification',plain.text))==1)
	{
		status=3
		accepted=0
		exact.match=1
	}else if
	(length(status)==0)
	{
		status=0
		accepted=0
		exact.match=0
		
	    #try again with a truncated species name (without last 4 letters
	  sp.name.len=nchar(species)
	  if(sp.name.len>5)
	  {
		sp.trunc=substr(species,1,sp.name.len-5)
	  }else(sp.trunc=substr(species,1,2))
	  
	  	URL=paste('www.algaebase.org/search/?species=',
        genus,'%20',
        sp.trunc,sep='')
		url.get=GET(URL)
	
		parsed=htmlParse(url.get)
		plain.text <- xpathSApply(parsed, "//p", xmlValue)
		status=plain.text[grep('Status',plain.text)] ##need to work on this part.
		if(length(grep('This is a preliminary entry and has not been subject to full verification',plain.text))==1){status=3}else if
		(length(status)==0)
		{
			status=0
			accepted=0
			exact.match=0
		}else if(length(grep("This name is of an entity that is currently accepted taxonomically",status))==1) #single page result with exact match
		{
			status=1
			exact.match=0
			accepted=1
		}else if(length(grep('This name is currently regarded as a synonym of ',status))==1) #single page result. entered name is synonym
		{
		  match.name=status
		  status=2
		  exact.match=0
		  accepted=0
		}else
		{
		  status=0
		  accepted=0
		  exact.match=0
		  res.species=gsub('+',' ',species,fixed=T)

		}
	}else if(length(grep("This name is of an entity that is currently accepted taxonomically",status))==1) #single page result with exact match
	{
			status=1
			exact.match=1
			accepted=1
	}else if(length(grep('This name is currently regarded as a synonym of ',status))==1) #single page result. entered name is synonym
	{
	  match.name=status
		  status=2
		  exact.match=1
		  accepted=0	
		  print("status=2\n")
	}else
	{
	  status=0
	}
	
	if(status==1)
	{
		species=gsub('+',' ',species,fixed=T)
		orig.name=paste(genus,species)
		
		match.name=plain.text[grep('Publication details',plain.text)]
		match.name<-gsub('Publication details',"",match.name)
		match.name<-algaeClassify::genus_species_extract(data.frame(match.name),"match.name")
		
		res.genus=match.name$genus
		res.species=match.name$species
		match.name<-paste(match.name$genus,match.name$species)
		
		res.synonyms='' #avoid returning homotypic synonyms to avoid confusion
		res.df=data.frame(genus=res.genus,species=res.species,exact.match=exact.match,accepted=accepted,synonyms=paste(res.synonyms,collapse=','),orig.name=orig.name,match.name=match.name)
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
        
		match.name=gsub('Status of nameThis name is currently regarded as a synonym of ','',match.name)
		#if exact match with 1 synonym, then it uses synonym as match.name
		match.name=data.frame(match.name=match.name)
		match.name<-algaeClassify::genus_species_extract(match.name,'match.name')
		res.genus=match.name$genus
		res.species=match.name$species
		
		orig.name=paste(genus,species)
		match.name=paste(res.genus,res.species)
		res.synonyms=match.name
		
		res.df=data.frame(genus=res.genus,species=res.species,exact.match=exact.match,accepted=accepted,synonyms=res.synonyms,orig.name=orig.name,match.name=match.name)
		if(long)##currently still grabs taxonomy for original name, not match name.
		{
			synonym.name.df=data.frame(syn=match.name)
			synonym.name.df<-algaeClassify::genus_species_extract(synonym.name.df,"syn")
			long.df<-algae.search(synonym.name.df$genus,synonym.name.df$species,long=T)[,8:13]
			#retrieve higher taxonomy for synonym match
			res.df<-cbind(res.df,long.df)
			return(res.df)
		}
		
		return(res.df)
	}else if(status==3)###unverified/unaccepated match
	{
		res.genus=genus
		res.species=gsub('+',' ',species,fixed=T)
		orig.name=paste(genus,species)
		match.name=paste(genus,species)
		res.synonyms=plain.text[grep('Synonym',plain.text)]
		res.synonyms=ifelse(grepl('Homotypic',res.synonyms,fixed=T),
							gsub('Homotypic Synonym(s)','',res.synonyms,fixed=T),
							gsub('Synonym(s)','',res.synonyms,fixed=T))
		res.synonyms=gsub('Heterotypic ','',res.synonyms,fixed=T)
		res.synonyms[grepl('No synonym',res.synonyms,fixed=T)]=''
		res.df=data.frame(genus=res.genus,species=res.species,exact.match=exact.match,accepted=accepted,synonyms=paste(res.synonyms,collapse=','),orig.name=orig.name,match.name=match.name)
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
	  res.df=data.frame(genus=NA,species=NA,exact.match=0,accepted=0,synonyms=NA,orig.name=paste(genus,species),match.name=NA)
      if(long)
      {
        res.df$Empire=res.df$Kingdom=res.df$Phylum=res.df$Class=res.df$Order=res.df$Family=NA
      }
      return(res.df)
		
	}
	
	results.tab=data.frame(tabs[[1]],stringsAsFactors = F)
	
	#first, check for genus only match
	genus.match=grepl(genus,results.tab[[1]])
	if(sum(genus.match)==0)
	{
	  res.df=data.frame(genus=NA,species=NA,exact.match=0,accepted=0,synonyms=NA,orig.name=paste(genus,species),match.name=NA)
      if(long)
      {
        res.df$Empire=res.df$Kingdom=res.df$Phylum=res.df$Class=res.df$Order=res.df$Family=NA
      }
      return(res.df)
	
	}
	results.tab<-results.tab[genus.match,]
	
	#exit out if results table is size 0
	if(dim(results.tab)[1]==0)
    {
	  res.df=data.frame(genus=NA,species=NA,exact.match=0,accepted=0,synonyms=NA,orig.name=paste(genus,species),match.name=NA)
	  if(long)
	  {
	    res.df$Empire=res.df$Kingdom=res.df$Phylum=res.df$Class=res.df$Order=res.df$Family=NA
	  }
	  return(res.df)
	};
	
		
	 colnames(results.tab)[2]='Current name if different'
	 results.tab2=results.tab
	 results.tab2[,2]=as.character(results.tab2[,2])
	
	##not excluding unchecked names for now. but flagging them.
	 
	 results.tab2$Unchecked=0
	 results.tab2$Unchecked[grep('Unchecked', results.tab2$Name)]=1
	 results.tab2<-algaeClassify::genus_species_extract(results.tab2,'Name')#cleaning up results names            
	 results.tab2$Name=paste(results.tab2$genus,results.tab2$species)
	 results.tab2<-results.tab2[!duplicated(results.tab2),]
	 
	 if(species=='') ##only return genus matches if only genus is entered.
	 {
					    results.tab2$Name=sapply(results.tab2$Name,function(x){
	                   return(strsplit(x,split=' ')[[1]][1]) 
	                  })
					  results.tab2[,2]=sapply(results.tab2[,2],function(x){
	                   return(strsplit(x,split=' ')[[1]][1]) 
	                  })
					  results.tab2[,2][is.na(results.tab2[,2])]=''
					  results.tab2$species=''
	 }else #clean up synonym names
	 {
	 
		synonym.tab=data.frame(synonyms=results.tab2[,2])
		synonym.tab<-algaeClassify::genus_species_extract(synonym.tab,'synonyms')
		results.tab2[,2]=paste(synonym.tab$genus,synonym.tab$species)
	 }
	 res.names=as.character(results.tab2$Name)

	 sub.name=ifelse(species=='',genus,paste(genus,species))
	 sub.name<-gsub('+',' ',sub.name,fixed=T)
	 
	 match.name=bestmatch(sub.name,unique(res.names))
	 match.rows=res.names == match.name
	 match.tab=results.tab2[match.rows,]
	 
	 match.tab[,2]=ifelse(match.tab[,2]==match.tab[,1]," ",match.tab[,2])
	 match.tab<-match.tab[!duplicated(match.tab),]

    res.synonyms=paste(unique(match.tab[,2]),collapse=',')
    res.synonyms=ifelse(res.synonyms==' ','',res.synonyms)
	res.synonyms=gsub(",,","",res.synonyms,fixed=T)
	res.synonyms<-gsub("^[,]","",res.synonyms) #remove preceeding commas
	res.synonyms<-gsub("[,]$","",res.synonyms) #remove trailing commas

	#not automatically deleting synonyms for genus only matches

   exact.match=ifelse(match.name==sub.name,1,0)
   if(res.synonyms !='' & exact.match==1 & length(grep(',',res.synonyms))==0)
   {	
		synonym.swapped=1
		match.name=res.synonyms
		print("status=2\n")
	} #swap synonym in for match.name
   
   if(res.synonyms=='' & sum(match.tab$Unchecked)==0 & match.name != 'multiplePartialMatch'){accepted=1}else{accepted=0}
   
	 #not excluding unchecked names, just flagging them for now.

	if(match.name=='multiplePartialMatch')
	{
		res.gen.spp=data.frame(genus='',species='')
	}else
	{
		res.gen.spp=algaeClassify::genus_species_extract(data.frame(res=match.name),'res')
	}
	
	res.genus=res.gen.spp$genus
	res.species=res.gen.spp$species
   
	 #check if there are any verified names.
	 res.df=data.frame(genus=res.genus,species=res.species,exact.match=exact.match,accepted=accepted,synonyms=res.synonyms,orig.name=sub.name,match.name=match.name)
	
	if(long)
	{
	  if(synonym.swapped==1)
	  {
		#swapping name with synonym in results tab for higher taxonomy lookup
		results.tab2$Name=res.synonyms
		
		synonym.name.df=data.frame(syn=match.name)
		synonym.name.df<-algaeClassify::genus_species_extract(synonym.name.df,"syn")
		long.df<-algae.search(synonym.name.df$genus,synonym.name.df$species,long=T)[,8:13]
		#retrieve higher taxonomy for synonym match
		res.df<-cbind(res.df,long.df)
		return(res.df)
		
	  }
	  if(status==1 | status==3) ##single match
	  {
	    details.parsed=read_html(url.get)
	  }else
	  {
	    links.parsed=xpathSApply(parsed,"//a/@href")
	    
	    links.parsed<-links.parsed[grep('results',links.parsed)]
	    links.parsed<-links.parsed[seq(1,length(links.parsed),by=2)]
		links.parsed=links.parsed[genus.match]
		good.link=links.parsed[grep(match.name,results.tab2$Name)[1]]
	    good.link=paste('http://www.algaebase.org',good.link,sep='')
	    
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
	
	##lines below force return of clean matches for genus, even if some species within have since been assigned to different genera
	# if(is.na(species) | is.null(species) | species %in% c('','sp','sp.','spp.'))
	# {
	  # res.df$synonyms=res.df$species=''
	# }
	
	if(res.df$exact.match==1 & res.df$species=='' & res.df$synonyms=='')
	{
		res.df$accepted=1 ##if genus only match exists, and no synonyms returned, 
						  #make sure the genus name is flagged as accepted.
	}
	  
	return(res.df)
}	

#now to create a wrapper for this.
#requires a data.frame with columns named genus and species
#use genus_species_extract if necessary first

spp.list.algaebase=function(phyto.df,phyto.name,lakename='',long=F,sleep.time=2) 
{
  phyto.df<-genus_species_extract(phyto.df,phyto.name)
  genus<-phyto.df$genus
  species<-phyto.df$species
  
  agg.list=vector("list",length=dim(phyto.df)[1])
  
  for(i in 1:dim(phyto.df)[1])#doing it as a loop with a pause in the middle to avoid overloading servers.
  {
    agg.list[[i]]=algae.search(genus[i],species[i],long=long)
    #uncomment the two lines below if there is an error and you want to see which rows ran successfully.
	#then re-source the function and try again.
	
	 print(i)
     print(agg.list[[i]])
    Sys.sleep(sleep.time)
  }

  agg.list<-ldply(agg.list)
  agg.list<-cbind(phyto.df[[phyto.name]],agg.list) 
  #add in the binomial names from the original dataset
  #this will facilitate merging the function results with the original data
  
  names(agg.list)[1]=phyto.name
  
  write.csv(agg.list,paste(lakename,'AlgaebaseNames.csv',sep=''))
  return(agg.list)
}

 
