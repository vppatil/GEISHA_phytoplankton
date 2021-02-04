#' Wrapper function to apply species_phyto_convert() across a data.frame
#'
#' @param phyto.df Name of data.frame. Must have character fields named 'genus' and 'species'
#' @param flag Resolve ambiguous MFG: 1 = return(NA), 2 = manual selection
#' @param mfgDbase specify library of species to MFG associations.
#'
#' @export species_to_mfg_df
#'
#' @return input data.frame with a new character column of MFG classifications
#' and diagnostic information
#'
#'
#' @examples
#' data(lakegeneva)
#' #example dataset with 50 rows
#'
#' new.lakegeneva <- genus_species_extract(lakegeneva,'phyto_name')
#' new.lakegeneva <- species_to_mfg_df(new.lakegeneva)
#' head(new.lakegeneva)

species_to_mfg_df <- function(phyto.df,flag=1,mfgDbase=NA)
{
  #phyto.len<-dim(phyto.df)[1]
  #na.vec<-rep(NA,length=phyto.len)
  #mfgs<-data.frame(MFG=na.vec,ambiguous.mfg=na.vec,genus.classification=na.vec,partial.match=na.vec,flag=na.vec)

  # Instead of the code above, create the first row here
  mfgs <- species_to_mfg(phyto.df$genus[1],phyto.df$species[1],flag=flag,mfgDbase=mfgDbase)

  # Then bind the other rows, starting at row 2.
  for(i in 2:phyto.len)
  {
    mfgs <- rbind(mfgs,
                  species_to_mfg(phyto.df$genus[i],phyto.df$species[i],flag=flag,mfgDbase=mfgDbase))
  }

  # phyto.df<-phyto.df[,names(phyto.df) %in% c('MFG','ambiguous.mfg','genus.classification','partial.match','flag')==FALSE,]
  phyto.df$MFG=c(as.character(mfgs[,1]))
  phyto.df$ambiguous.mfg=c(as.character(mfgs[,2]))
  phyto.df$genus.classification=c(as.character(mfgs[,3]))
  phyto.df$partial.match=c(as.character(mfgs[,4]))
  phyto.df$flag=c(as.character(mfgs[,5]))
  return(phyto.df)
}


