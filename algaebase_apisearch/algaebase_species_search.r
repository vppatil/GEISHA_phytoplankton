##looks good. now, what about species matches?
algaebase_species_search<-function(genus,species,apikey=NULL,handle=NULL,
                                 higher=TRUE,print.full.json=FALSE,
                                 newest.only=TRUE,long=FALSE,print.df=FALSE,exact.matches.only=FALSE,
								 api_file=NULL){
  ##must include either a handle object with an api key
  #or the api key itself
  
  #it is inefficient, but the only curl request that works is by specifying the genus
  #so, grab the results of a genus search string, then subset to those with species matches.
  
  ##requires:
  #curl
  #jsonlite

  #may eventually replace with httr-based code based on 
  # https://cran.r-project.org/web/packages/httr/vignettes/secrets.html
  # https://cran.r-project.org/web/packages/httr/vignettes/api-packages.html
  
  #first, throw error if there is no genus name
  #this prevents the code from trying to search every possible name in the
  #database.
  if(genus==''|is.na(genus)|is.null(genus))
  {
    stop("No genus name supplied") 
  }
  
  #parse infraspecific names from specificEpithet, if present
  #and create appropriate search query string.
  #genus_species_extract strips out infraspecific category names
  #so we will just look for the infraspecific name within the full scientific name.
  if(length(grep(" ",species)>0)){
    species.split=strsplit(species,split=' ')[[1]]
    sp=species.split[1]
    infrasp=species.split[2]
    

    
    species.search.string<-paste0("https://api.algaebase.org/v1.3/species?genus=",
                                  genus,"&dwc:specificEpithet=",sp,
                                  "&dwc:scientificName=",infrasp)
                                
    }else{
     species.search.string<-paste0("https://api.algaebase.org/v1.3/species?genus=",genus,
                                  "&dwc:specificEpithet=",species)
}
  


  #read api key if necessary
  if(!is.null(api_file)){
	apikey<-apikey_from_file(api_file)
  }
  
  #set curl handle if not supplied as an argument
  if(is.null(handle)){
    handle<-set_algaebase_apikey_header(apikey)
  }
  
  #submit query
  con <- curl(species.search.string, handle = handle)
  
  #parse query results
  results<-try(readLines(con),silent=TRUE)
  if(class(results)=="try-error")
  {
    close(con)
    stop("No matches") #throw error for now. 
    #will need to modify in wrapper so that it fills with NA instead.
  }
  
  results<-jsonlite::prettify(results)
  close(con) #need to close the connection asap.
  
  if(print.full.json){ #can just return the raw output if that is what user wants
    print(results)
    return(results)
    
  }
  #transform to r list of lists
  result.list<-fromJSON(results)
  
  #objects
  
  #so, if no match in the first page of output, you have to keep going.
  #need to deal with the issue of multiple pages more explicitly at some point
  #but it shouldn't be a huge problem most of the time.
  #for now you could return a flag or warning if there are multiple pages of results.
  
  pagination<-result.list[[1]]
  results.output<-result.list[[2]]
  
  
  # num.results<-pagination$"_total_number_of_results" 
  # num.pages<-pagination$"_total_number_of_pages"
  
  #dealing with infraspecific name
  output.infraspname<-ifelse(results.output$"dwc:taxonRank"=="forma",
                                    results.output$infraspecificEpithet_forma,
                                    ifelse(results.output$"dwc:taxonRank"=="variety",
                                           results.output$infraspecificEpithet_variety,
                                           ifelse(results.output$"dwc:taxonRank"=="subspecies",
                                                  results.output$infraspecificEpithet_subspecies,"")))
  output.clean.names<-paste(results.output$"dwc:genus",results.output$"dwc:specificEpithet",output.infraspname)
  output.clean.names<-trimws(output.clean.names)
  
input.clean.name<-paste(genus,species)
  
output.match.indices<-output.clean.names==input.clean.name

#only retain exact matches if asked.
if(exact.matches.only){
  if(sum(output.match.indices)==0){stop("No exact matches found")
    }else{
            results.output<-results.output[output.match.indices,];
                        output.match.indices=TRUE
    }
}

long.name<-algaebase_output_parse(results.output,"dwc:scientificName")
taxonomic.status<-algaebase_output_parse(results.output,"dwc:taxonomicStatus")
taxonomic.genus<-algaebase_output_parse(results.output,"dwc:genus")
taxonomic.species<-algaebase_output_parse(results.output,"dwc:specificEpithet")
taxonRank<-algaebase_output_parse(results.output,"dwc:taxonRank")
authorship<-algaebase_output_parse(results.output,"dwc:scientificNameAuthorship")
mod.date<-algaebase_output_parse(results.output,"dcterms:modified")
mod.date<-ymd(mod.date) #set as date type so you can sort output based on latest modification
currently.accepted=ifelse(taxonomic.status=='currently accepted taxonomically',1,0)
#need to clean up accepted name section.
accepted.name<-ifelse(currently.accepted==1,
                      algaebase_output_parse(results.output,"dwc:scientificName"),
                      algaebase_output_parse(results.output,"dwc:acceptedNameUsage"))
for(i in 1:length(accepted.name)){
  accepted.name[i]<-trimws(gsub(authorship[i],"",accepted.name[i],fixed = TRUE))
}

input.name=input.clean.name
input.match=ifelse(output.match.indices,1,0)

output<-data.frame(genus=taxonomic.genus,species=taxonomic.species,infrasp=NA,taxonomic.status,currently.accepted,accepted.name,input.name=input.clean.name,
                   input.match,taxon.rank=taxonRank,mod.date,long.name,authorship)

for(i in 1:nrow(output)){
  output$infrasp[i]<-ifelse(output$taxon.rank[i]=='variety',results.output$infraspecificEpithet_variety[i],
                         ifelse(output$taxon.rank[i]=='forma',results.output$infraspecificEpithet_forma[i],
                                ifelse(output$taxon.rank[i]=='subspecies',results.output$infraspecificEpithet_subspecies[i],NA)))
}


  
  if(higher){ #merge higher taxonomy and reorder names of output variable
    higher.taxonomy<-algaebase_genus_search(genus,
                                            return.higher.only = TRUE,
                                            handle=handle,exact.matches.only = TRUE,newest.only = TRUE);
    output<-merge(higher.taxonomy,output,all.y=TRUE,by='genus',sort=FALSE);
    output<-subset(output,select= c('accepted.name','input.name','input.match','currently.accepted','kingdom','phylum','class','order','family','genus','species','infrasp',
                                    'long.name','taxonomic.status','taxon.rank','mod.date','authorship'))}else{
    output<-subset(output,select=c('accepted.name','input.name','input.match','currently.accepted','genus','species','infrasp',
                                                                     'long.name','taxonomic.status','taxon.rank','mod.date','authorship') )  
   
                                    }
  if(newest.only){
    output<-output[output$mod.date==max(output$mod.date),] #only retain the most recent edit
  }else{
    output<-output[order(output$mod.date,decreasing=TRUE),]
  }
  

  if(!long){output<-output[,names(output) %in% c('long.name','authorship','taxonomic.status','mod.date')==FALSE]}
  

  
  return(output)

}