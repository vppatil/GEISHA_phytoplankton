#' Assign phytoplankton species to CSR functional groups, based on surface to volume ratio and
#' maximum linear dimension ranges proposed by Reynolds et al. 1988;2006
#'
#' @param sav numeric estimate of cell or colony surface/volume ratio in um^2/um^3
#' @param msv numeric product of surface/volume ratio multiplied by maximum linear dimension (msv; um)
#'
#' @export traits_to_csr
#'
#' @return a character string with one of 3 return values: C,S, or R
#'
#' @examples
#'
#' traits_to_csr(sav=0.2,msv=10)
#'
#'
#' @seealso /url{https://powellcenter.usgs.gov/geisha} for project information


traits_to_csr=function(sav,msv)
{

  csr=NA
  #must be based on measurements in micrometers (^2, ^3)
  sav.vals=unlist(traitranges[traitranges$Measurement=='sav',2:7])
  msv.vals=unlist(traitranges[traitranges$Measurement=='msv',2:7])


  if(sav>=sav.vals[1] & sav < sav.vals[4] &
     msv >=msv.vals[1] & msv <msv.vals[4]){csr = 'C'}

  if(msv < msv.vals[3] & sav >= sav.vals[5]){csr='C'}

  if(msv < msv.vals[4] & sav >= sav.vals[6]){csr='C'}

  if(sav>=sav.vals[5] & msv >= msv.vals[4]){csr='R'}

  if(sav <sav.vals[5] &
     msv < msv.vals[5]){csr = 'S'}
  if(sav>=sav.vals[3] & sav < sav.vals[6] &
     msv>=msv.vals[3] & msv < msv.vals[6]){csr = 'R'}

  if(sav >=sav.vals[3] & sav < sav.vals[6] &
     msv >=msv.vals[3] & msv < msv.vals[4]){csr = 'CR'}

  if(msv >=msv.vals[5] & sav < sav.vals[3]) {csr='SR'}

  return(csr)

}
