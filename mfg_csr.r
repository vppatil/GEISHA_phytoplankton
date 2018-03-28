##need to load mfg csr library
#must be in the working directory for the MFG_CSR.rda function
load('MFG_CSR.rda')

#contains the mfg_csr correspondence library developed by orlane, nico, et al at workshop 2.

#returns a CSR classification for a single MFG
mfg.csr.convert<-function(mfg)
{
  #requires loading MFG_CSR.rda
  #want to pull out the mfg number in an intelligent way.
  #if this is run with something returned from your library, no problem.
  
  if(mfg %in% mfg.csr$MFG.name==F){csr=NA}
  else
  {
	csr=mfg.csr$CSR[mfg.csr$MFG.name==mfg]
	return(csr)
  }

}

#return a new dataframe with a CSR column, assuming the dataframe has an MFG column to start.
mfg.csr.convert.df=function(phyto.df,mfg)
{
  csrs<-vector(length=dim(phyto.df)[1])
  for(i in 1:dim(phyto.df)[1])
  {
    csrs[i]=mfg.csr.convert(phyto.df[[mfg]][i])
  }
  phyto.df$CSR=csrs
  return(phyto.df)
}


##chain functions to generate a CSR classification with only a genus and species names as inputs
spp.csr.convert=function(genus,species)
{
	mfg=nico.phyto.convert(genus,species)
	csr=mfg.csr.convert(mfg)
	return(csr)
}

##CSR column for whole data.frame, based on the presence of columns named genus and species
spp.csr.convert.df=function(phyto.df)
{
	phyto.df=phyto.convert.df(phyto.df)
	phyto.df=mfg.csr.convert.df(phyto.df,'MFG')
	return(phyto.df)
}