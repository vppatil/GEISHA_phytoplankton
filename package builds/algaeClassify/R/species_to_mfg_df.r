#' Wrapper function to apply species_phyto_convert() across a data.frame
#'
#' @param phyto.df Name of data.frame. Must have character fields named 'genus' and 'species'
#' @param flag Resolve ambiguous MFG: 1 = return(NA), 2 = manual selection
#' @param mfgDbase specify library of species to MFG associations.
#'
#' @export species_to_mfg_df
#' 
#' @return input data.frame with a new character column of MFG classifications
#' 
#' @examples
#' data(lakegeneva)
#' #example dataset with 50 rows
#' 
#' new.lakegeneva <- genus_species_extract(lakegeneva,'phyto_name')
#' new.lakegeneva <- species_to_mfg_df(new.lakegeneva)
#' head(new.lakegeneva)

species_to_mfg_df <- function(phyto.df,flag=1,mfgDbase=species.mfg.library)
{
  mfgs <- mapply(species_to_mfg,phyto.df$genus,phyto.df$species,flag=flag,mfgDbase=mfgDbase)
  phyto.df$MFG <- mfgs
  return(phyto.df)
}
