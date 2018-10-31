#' Wrapper function to apply species_phyto_convert() across a data.frame
#'
#' @param phyto.df Name of data.frame. Must have character fields named 'genus' and 'species'
#' @param flag Resolve ambiguous MFG: 1 = return(NA), 2 = manual selection
#'
#' @export phyto_convert_df
#' 
#' @return a single MFG classification as character string
#' 
#' @examples
#' data(lakegeneva)
#' #example dataset with 50 rows
#' 
#' new.lakegeneva <- genus_species_extract(lakegeneva,'phyto_name')
#' new.lakegeneva <- phyto_convert_df(new.lakegeneva)
#' head(new.lakegeneva)

phyto_convert_df <- function(phyto.df,flag=1)
{
  #this function applies the nico.phyto.convert function to an entire data frame, 
  #but that data frame must have columns named 'genus' and 'species'

  mfgs <- mapply(species_phyto_convert,phyto.df$genus,phyto.df$species,flag=flag)
  phyto.df$MFG <- mfgs
  return(phyto.df)
}
