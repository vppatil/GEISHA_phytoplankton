#' Conversion of a single genus and species name to a single MFG. Uses species.mfg.library
#'
#' @param genus Character string: genus name
#' @param species Character string: species name
#' @param flag Resolve ambiguous mfg: 1 = return(NA),2= manual selection
#' @param mfgDbase data.frame of species MFG classifications. Defaults to the supplied species.mfg.library data object
#'
#' @export species_to_mfg
#' 
#' @return a single MFG classification as character string
#' 
#' @examples
#' species_to_mfg('Scenedesmus','bijuga')
#' #returns "11a-NakeChlor"

species_to_mfg<-function(genus,species,flag=1,mfgDbase=species.mfg.library)#set flag to two if you want to 
													 #manually resolve ambiguous mfg class.
  #default behavior is to set ambiguous classes to NA (flag=1)
{  
  mfgDbase<-mfgDbase[!duplicated(mfgDbase),]
  
  genus=gsub('Unknown ','',genus)
  if(species %in% mfgDbase$species==F){species=''}#replacing spp., sp. etc. with ''
  
  #check for genus and species match first.
  mfg=mfgDbase$MFG[mfgDbase$genus==genus &
							  mfgDbase$species==species]
  #go to genus match 
 if(length(unique(mfg)==1))
 {
   mfg=unique(mfg)
 }else{
   mfg=mfgDbase$MFG[mfgDbase$genus==genus & mfgDbase$species=='']
 }
  #if there is no genus only match, see if there is another species with the same genus 
  if(length(unique(mfg))==0)
  {
    mfg=mfgDbase$MFG[mfgDbase$genus==genus]
  }

  if(length(unique(mfg))==2)#flag 2 means you can interactively 
							#choose among two possible mfgs for a particular genus or species
  {
    if(flag==1)
    {
      mfg=NA
    }else if (flag==2)
    {
      mfg=unique(mfg)
      cat(paste('\n two possible mfgs for the species: ',genus,species))
      cat()
      cat(paste('\n1:',mfg[1]))
      cat(paste('\n2:',mfg[2]))
      choice=as.numeric(readline(prompt='\nenter your choice: (1 or 2): \n'))
      mfg=mfg[choice]
    }
  }else if(length(mfg)==0)
  {
    mfg=NA
  }else
  {
    mfg=mfg[1]
  }
  
  return(mfg)
}