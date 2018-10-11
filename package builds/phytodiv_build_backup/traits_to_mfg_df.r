#' Assign morphofunctional groups to a dataframe of functional traits and higher taxonomy
#'
#' @param dframe An R dataframe containing functional trait information and higher taxonomy
#' @param arg.names Character string of column names corresponding to arguments for traits_to_mfg()
#' @return A character vector containing morpho-functional group (MFG) designations
#'
#' @examples
#' \dontrun{traits_to_mfg_df(func.dframe,c('flag','size','col','fil','cent','gel','aer','cl','or'))}
traits_to_mfg_df<-function(dframe,arg.names=c('flagella',
                                              'size',
                                              'colonial',
                                              'filament',
                                              'centric',
                                              'gelatinous',
                                              'aerotopes',
                                              'class',
                                              'order'))
{
  mfg.from.traits=''
  for(i in 1:dim(dframe)[1])
  {
    mfg.from.traits[i]=traits.to.mfg(dframe[[arg.names[1]]][i],dframe[[arg.names[2]]][i],dframe[[arg.names[3]]][i],dframe[[arg.names[4]]][i],dframe[[arg.names[5]]][i],dframe[[arg.names[6]]][i],dframe[[arg.names[7]]][i],dframe[[arg.names[8]]][i],dframe[[arg.names[9]]][i])
  }
  
  return(mfg.from.traits)
}
