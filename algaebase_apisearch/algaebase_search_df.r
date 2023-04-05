algaebase_search_df<-function(df,apikey=NULL,handle=NULL,genus.only=FALSE,
                              genus.name='genus',species.name='species',
                              higher=TRUE,print.full.json=FALSE,
                              long=FALSE,exact.matches.only=TRUE,
                              api_file=NULL,sleep.time=1)
{
  
  #for testing:
  # phytodat.sub<-phytodat[sample(1:nrow(phytodat),10,replace=FALSE),]
  
  #requires:
  # library(curl)
  # library(jsonlite)
  # library(lubridate)
  #note- cannot print full json, and returns newest_exact_matches only
  #if species is not supplied, will search genus only
  #if no species match is found, default to genus search
  #if no genus match is found, return a data frame row with input name+
  #NAs
  algaebase_df<-data.frame()

  nrows=nrow(df)
  err.row.func<-function(df,index=0,genus.only=FALSE,
                         genus.name='genus',
                         species.name='species',higher=TRUE,long=TRUE){

      if(genus.only){input.name<-df[[genus.name]][index]}else{
      input.name<-trimws(paste(df[[genus.name]][index],df[[species.name]][index]))
    }
                       
    err.df.row<-data.frame(kingdom=NA,phylum=NA,class=NA,order=NA,family=NA,
                           genus=NA,species=NA,infrasp=NA,taxonomic.status=NA,
                           currently.accepted=NA,accepted.name=NA,genus.only=NA,
                           input.name=input.name,input.match=0,taxon.rank=NA,
                           mod.date=NA,long.name=NA,authorship=NA)

    
    if(higher==FALSE){
      err.df.row<-err.df.row[,names(err.df.row)%in% c('kingdom','phylum','class','order','family')==FALSE]
    }
    
    if(long==FALSE){
      err.df.row<-err.df.row[,names(err.df.row)%in% c('long.name','authorship','taxonomic.status','mod.date')==FALSE]
    }
    
    return(err.df.row)
  }
    
    for(i in 1:nrows){
      
      
      Sys.sleep(sleep.time)
      genus=df[[genus.name]][i]
      
      #if no species match, try genus
      #if no genus match, fill row with NAs
      #if no exact matches specified, will return newest hit
      #not smart enough yet to return closest non-exact match.
      #don't get stuck trying to find all genera ever if no genus name supplied.
      if(genus==''|is.na(genus)|is.null(genus)){
                tmp<-err.row.func(df=df,index=i,
                        genus.only=genus.only,
                        genus.name=genus.name,
                        species.name=species.name,
                        higher=higher,long=long)}else if(genus.only|species==''|is.na(species)|is.null(species)){

        tmp<-try(algaebase_genus_search(genus=genus,apikey=apikey,handle=handle,
                                    higher=higher,
                                    print.full.json=FALSE,
                                    newest.only=TRUE,long=long,
                                    exact.matches.only=exact.matches.only,
                                    api_file=api_file,
                                    return.higher.only=FALSE),silent=TRUE)
        
      }else{
        species=df[[species.name]][i]
        tmp<-try(algaebase_species_search(genus=genus,species=species,apikey=apikey,handle=handle,
                                    higher=higher,
                                    print.full.json=FALSE,
                                    newest.only=TRUE,long=long,
                                    exact.matches.only=exact.matches.only,
                                    api_file=api_file),silent=TRUE)
        if(class(tmp)=='try-error'){
          tmp<-try(algaebase_genus_search(genus=genus,apikey=apikey,handle=handle,
                                          higher=higher,
                                          print.full.json=FALSE,
                                          newest.only=TRUE,long=long,
                                          exact.matches.only=TRUE,
                                          api_file=api_file,
                                          return.higher.only=FALSE),silent=TRUE)
        }
      }
      
      if(class(tmp)=='try-error'){
        tmp<-err.row.func(df=df,index=i,
                                  genus.only=genus.only,
                                  genus.name=genus.name,
                                  species.name=species.name,
                                  higher=higher,long=long)
      }
      
      print(paste0(round(100*i/nrows),"% complete"))
      algaebase_df<-rbind(algaebase_df,tmp)
    }
    

    return(algaebase_df)
}
  
#only difference is return.higher.only
