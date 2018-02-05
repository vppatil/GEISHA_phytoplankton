##not clear how function this is as of sept 19, 2017
##presumably need to load mfg csr library

load('MFG_CSR.rda')
#contains the mfg_csr correspondence library developed by orlane, nico, et al at workshop 2.

mfg.csr.convert<-function(mfg.num)
{
  #requires loading MFG_CSR.rda
  #want to pull out the mfg number in an intelligent way.
  #if this is run with something returned from your library, no problem.
  
  if(mfg.num %in% mfg.csr$MFG.number==F){csr=NA}
  else
  {
	csr=mfg.csr$CSR[mfg.csr$MFG.number==mfg.num]
	return(csr)
  }

}

mfg.csr.convert.df=function(phyto.df,mfg.num)
{
  csrs<-vector(length=dim(phyto.df)[1])
  for(i in 1:dim(phyto.df)[1])
  {
    csrs[i]=mfg.csr.convert(phyto.df[[mfg.num]][i])
  }
  phyto.df$CSR=csrs
  return(phyto.df)
}

spp.csr.convert=function(genus,species)
{
	mfg=nico.phyto.convert(genus,species)
	mfg.num=strsplit(mfg,split='-')[[1]][1]
	csr=mfg.csr.convert(mfg.num)
	return(csr)
}

spp.csr.convert.df=function(phyto.df)
{
	phyto.df=phyto.convert.df(phyto.df)
	phyto.df$MFG.num=sapply(orlane$MFG.from.Nico,function(x) strsplit(x,split='-')[[1]][1])
	phyto.df=mfg.csr.convert.df(phyto.df,'MFG.num')
	return(phyto.df)
}