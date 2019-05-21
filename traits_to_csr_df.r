#' Add CSR functional group classifications to a dataframe of phytoplankton species, based on surface to volume ratio and
#' maximum linear dimension ranges proposed by Reynolds et al. 1988;2006
#' 
#' @param df name of dataframe
#' @param msv character string with name of column that contains maximum linear dimension * surface to volume ratio values
#' @param sv character string with name of column that contains surface to volume ratio values
#'
#' @export traits_to_csr_df
#' 
#' @return a character string with one of 3 return values: C,S, or R
#' 
#' @examples
#' 
#' csr.df<-data.frame(msv=10,sav=1)
#' 
#' traits_to_csr_df(csr.df,'msv','sav')
#' 
#' 
#' @seealso /url{https://powellcenter.usgs.gov/geisha} for project information


traits_to_csr_df=function(df,sav,msv)
{
  csr=vector(mode='character',length=dim(df)[1])
  
  for(i in 1:dim(df)[1])
  {
    csr[i]=traits_to_csr(df[[sav]][i],df[[msv]][i])
  }
  
  return(csr)
}
