traits.to.kruk=function(flagella=NA,silica=NA,MLD=NA,mucilage=NA,aerotopes=NA,SV=NA,V=NA)
{
	kruk.mfg=''
	if(flagella==1 & silica==1) kruk.mfg='II'
	if(flagella==1 & silica==0 & MLD < 2) kruk.mfg='I'
	if(flagella==1 & silica==0 & MLD >= 2) kruk.mfg='V'
	if(flagella==0 & silica==1) kruk.mfg='VI'
	if(flagella==0 & silica==0 & mucilage==1 & aerotopes==1 & SV >=0.6) kruk.mfg='III'
	if(flagella==0 & silica==0 & mucilage==1 & aerotopes==1 & SV < 0.6) kruk.mfg='VII'
	if(flagella==0 & silica==0 & mucilage==1 & aerotopes==0 & SV < 10) kruk.mfg='I'
	if(flagella==0 & silica==0 & mucilage==1 & aerotopes==0 & SV >= 10) kruk.mfg='VII'
	if(flagella==0 & silica==0 & mucilage==0 & V < 30 & MLD <20) kruk.mfg='I'
	if(flagella==0 & silica==0 & mucilage==0 & V < 30 & MLD >= 20) kruk.mfg='IV'
	if(flagella==0 & silica==0 & mucilage==0 & V >= 30 & aerotopes==1 ) kruk.mfg='III'
	if(flagella==0 & silica==0 & mucilage==0 & V >= 30 & aerotopes==0 ) kruk.mfg='IV'

	return(kruk.mfg)
}

##need to add database wrapper and inverse function to this.
