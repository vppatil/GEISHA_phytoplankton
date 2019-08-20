#' Wrapper for applying algae_search function to a data.frame that contains phytoplankton species
#'
#' @param phyto.df data.frame containing a character column with binomial names
#' @param lakename Character string for naming output files
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
#' lakegeneva.algaebase<-
#' spp_list_algaebase(lakegeneva,phyto.name='phyto_name',long=TRUE,write=FALSE)
#'
#' @seealso \url{http://www.algaebase.org} for up-to-date phytoplankton taxonomy,
#'     \url{https://powellcenter.usgs.gov/geisha} for project information
   
spp_list_algaebase=function(phyto.df,phyto.name=1,lakename='',long=FALSE,write=FALSE) 
{
  phyto.df<-genus_species_extract(phyto.df,phyto.name) #clean up names
  genus<-phyto.df$genus
  species<-phyto.df$species
  
  agg.list=vector("list",length=dim(phyto.df)[1])
  
  sleep.times=rep(.5,length(genus)) #give the servers a break in between searches.
  
  percent.seq<-seq(1,dim(phyto.df)[1],len=11)
  percent.seq=round(percent.seq)
  percents=paste0(seq(0,100,by=10),"% completed")
  
  for(j in 1:dim(phyto.df)[1])#doing it as a loop with a pause in the middle 
  {
    
    agg.list[[j]]=algaeClassify::algae_search(genus[j],species[j],long=long)
    
    #manually convert to character- avoids error if first row is all NA's
    agg.list[[j]]<-sapply(agg.list[[j]],function(x) 
                                        {
                                          if(is.numeric(x))
                                          {
                                            x<-as.numeric(x)
                                          }else
                                          {
                                            x<-as.character(x)
                                          }; 
                                          return(x)
                                        })
    
    #create data.frame
    agg.df=ldply(agg.list)
    
    #append in the binomial names from the original dataset
    agg.df<-cbind(phyto.df[[phyto.name]][1:j],agg.df) 
    
    names(agg.df)[1]=phyto.name
    
    #create output files.
    #writing output with each iteration- this is slower, 
    #but it means you don't have to start over if it throws an error unexpectedly.
    if(write)
    {
      write.csv(agg.df,paste(lakename,'AlgaebaseNames.csv',sep=''))
      save.image(paste(lakename,'AlgaebaseNames.RData',sep=''))
    }
    if(j %in% percent.seq) {cat(percents[match(j,percent.seq)]); cat("\n")}
    Sys.sleep(sleep.times[j])
  }
  
  return(agg.df)
}
