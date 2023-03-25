
algaebase_genus_search<-function(genus,apikey=NULL,handle=NULL,
                                 higher=TRUE,print.full.json=FALSE,
                                 newest.only=TRUE,long=FALSE,
                                 exact.matches.only=FALSE,
                                 return.higher.only=FALSE,
								 api_file=NULL){
  ##must include either a handle object with an api key
  #or the api key itself
  
  ##requires:
  #jsonlite
  #curl
  
  genus.search.string<-paste0('https://api.algaebase.org/v1.3/genus?genus=',genus)
  
  #read api key if necessary
  if(!is.null(api_file)){
	apikey<-apikey_from_file(api_file)
  }
  
  #set curl handle if not supplied as an argument
  if(is.null(handle)){
    handle<-set_algaebase_apikey_header(apikey)
  }
  
  con <- curl(genus.search.string, handle = handle)
  results<-try(readLines(con),silent=TRUE)
  if(class(results)=="try-error")
  {
    close(con)
    stop("No matches") #throw error for now. 
    #will need to modify in wrapper so that it fills with NA instead.
  }
  
  results<-jsonlite::prettify(try(readLines(con),silent=TRUE))
  close(con)
  
  if(print.full.json){ #can just return the raw output if that is what user wants
    print(results)
    return(results)
    
  }
  
  #transform to r list of lists
  result.list<-fromJSON(results)
  
  
  #objects
  pagination<-result.list[[1]]
  results.output<-result.list[[2]]
  num.results<-pagination$`_total_number_of_results` 
  print(num.results) #remove this later- how many results are there?
  names(results.output) #field names for each hit
  
  #all of this could be condensed in later versions of the function
  #but if it runs quickly then who cares.
  #can still add a delay between api requests if necessary?

  #adding extra option to be extract higher taxonomy for species searches
  if(return.higher.only){
    higher=TRUE; exact.matches.only=TRUE; newest.only=TRUE
    
  }
  
  if(higher==TRUE){
    kingdom<-algaebase_output_parse(results.output,"dwc:kingdom")
    phylum<-algaebase_output_parse(results.output,"dwc:phylum")
    taxonomic.class<-algaebase_output_parse(results.output,"dwc:class")
    taxonomic.order<-algaebase_output_parse(results.output,"dwc:order")
    taxonomic.family<-algaebase_output_parse(results.output,"dwc:family")
    taxonomic.genus<-algaebase_output_parse(results.output,"dwc:genus")
    
    higher.taxonomy<-data.frame(kingdom=kingdom,phylum,class=taxonomic.class,
                                  order=taxonomic.order,
                                  family=taxonomic.family)
    if(return.higher.only==TRUE){
      higher.taxonomy$genus=taxonomic.genus
      return(higher.taxonomy)
    }

  }
  
  long.name<-algaebase_output_parse(results.output,"dwc:scientificName")
  taxonomic.status<-algaebase_output_parse(results.output,"dwc:taxonomicStatus")
  taxonRank<-algaebase_output_parse(results.output,"dwc:taxonRank")
  authorship<-algaebase_output_parse(results.output,"dwc:scientificNameAuthorship")
  mod.date<-algaebase_output_parse(results.output,"dcterms:modified")
  mod.date<-ymd(mod.date) #set as date type so you can sort output based on latest modification
  accepted.name<-algaebase_output_parse(results.output,"dwc:acceptedNameUsage")
  input.name=genus
  input.match=ifelse(genus==taxonomic.genus,1,0)
  currently.accepted=ifelse(taxonomic.status=='currently accepted taxonomically',1,0)
  accepted.name<-ifelse(currently.accepted==1,taxonomic.genus,accepted.name)
  
  output<-data.frame(genus=taxonomic.genus,species=NA,infrasp=NA,taxonomic.status,currently.accepted,accepted.name,input.name=genus,
                     input.match,taxon.rank=taxonRank,mod.date,long.name,authorship)
  if(higher){output<-cbind(higher.taxonomy,output)}
  
  #only retain exact matches if asked.
  if(exact.matches.only){
    if(sum(output$input.match)==0){stop("No exact matches found")
    }else{output<-output[output$input.match==1,]}
  }
  
  if(newest.only){
    output<-output[output$mod.date==max(output$mod.date),] #only retain the most recent edit
  }else{
    output<-output[order(output$mod.date,decreasing=TRUE),]
  }
  
  if(!long){output<-output[,names(output) %in% c('long.name','authorship','taxonomic.status','mod.date')==FALSE]}
  


  
    
      
    return(output)
}
##Arguments:
# genus=input genus name
# apikey= key from algaebase
# handle= curl handle containing key, if already set up
# higher= boolean (return higher taxonomic classifications?)
# pring.full.json=boolean (return full api output in json format?)
# newest.only=boolean (only return most recently entered match?)
# long=boolean (include additional information including full taxonomic name, authorship, 
#               database modification date, and detailed taxonomic status information?)
# print.df=boolean (print the output to the console or not?)
# exact.matches.only (only return database entries that exactly match the input genus name?)