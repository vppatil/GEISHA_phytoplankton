#' Returns a CSR classification based on Morphofunctional group (MFG).
#' Correspondence based on Salmaso et al. 2015 and Reynolds et al. 1988
#'
#' @param mfg Character string with MFG name, following Salmaso et al. 2015
#'
#' @export mfg_csr_convert
#' 
#' @return A character string with values 'C','S','R','CR','SC','SR', or NA
#' 
#' @examples
#' 
#' mfg_csr_convert("11a-NakeChlor")

#returns a CSR classification for a single MFG
mfg_csr_convert<-function(mfg)
{
  if(mfg %in% mfg.csr$MFG==F){csr=NA}
  else
  {
	csr=mfg.csr$CSR[mfg.csr$MFG==mfg]
	return(csr)
  }

}