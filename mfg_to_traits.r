##code to output a trait matrix based on salmaso MFG classification
##matrix is currently incomplete, and is only the inverse of information contained in salmaso et al. 2015 supplemental table 2
## could be expanded by indicating the expected trait values for individual classes/orders, based on the literature

#extract a trait matrix for a single character string MFG name. The matrix will contain NA for any traits that are not required to identify a particular MFG.
mfg.to.traits<-function(MFG)
{
  MFG.num=ifelse(is.na(MFG),NA,strsplit(MFG,split='-')[[1]][1])
	trait.df<-switch(MFG.num,
		'1a' = data.frame(flagella = 1,
		                  autotroph = 0,
		                  size = 'Large',
		                  colonial = NA,
		                  centric = NA,
		                  gelatinous = NA,
		                  aerotopes = NA,
		                  filamentous = NA),
		'1b' = data.frame(flagella = 1, autotroph = 0, size = 'Large', colonial = NA, centric = NA, gelatinous = NA, aerotopes = NA, filamentous = NA),
		'1c' = data.frame(flagella = 1, autotroph = 0, size = 'Large', colonial = NA, centric = NA, gelatinous = NA, aerotopes = NA, filamentous = NA),
		'2a' = data.frame(flagella = 1, autotroph = 0, size = 'Small', colonial = 0, centric = NA, gelatinous = NA, aerotopes = NA, filamentous = NA),
		'2b' = data.frame(flagella = 1, autotroph = 0, size = 'Small', colonial = 0, centric = NA, gelatinous = NA, aerotopes = NA, filamentous = NA),
		'2c' = data.frame(flagella = 1, autotroph = 0, size = 'Small', colonial = 0, centric = NA, gelatinous = NA, aerotopes = NA, filamentous = NA),
		'2d' = data.frame(flagella = 1, autotroph = 0, size = 'Small', colonial = 0, centric = NA, gelatinous = NA, aerotopes = NA, filamentous = NA),
		'3a' = data.frame(flagella = 1, autotroph = 1, size = NA, colonial = 0, centric = NA, gelatinous = NA, aerotopes = NA, filamentous = NA),
		'3b' = data.frame(flagella = 1, autotroph = 1, size = NA, colonial = 1, centric = NA, gelatinous = NA, aerotopes = NA, filamentous = NA),
		'4' = data.frame(flagella = 0, autotroph = 1, size = NA, colonial = 0, centric = NA, gelatinous = NA, aerotopes = NA, filamentous = NA),
		'5a' = data.frame(flagella = 0, autotroph = 1, size = NA, colonial = 1, centric = NA, gelatinous = NA, aerotopes = NA, filamentous = NA),
		'5b' = data.frame(flagella = 0, autotroph = 1, size = 'Large', colonial = 1, centric = NA, gelatinous = NA, aerotopes = 1, filamentous = NA),
		'5c' = data.frame(flagella = 0, autotroph = 1, size = 'Large', colonial = 1, centric = NA, gelatinous = NA, aerotopes = 0, filamentous = NA),
		'5d' = data.frame(flagella = 0, autotroph = 1, size = 'Small', colonial = 1, centric = NA, gelatinous = NA, aerotopes = NA, filamentous = NA),
		'5e' = data.frame(flagella = 0, autotroph = 1, size = 'Small', colonial = 1, centric = NA, gelatinous = NA, aerotopes = NA, filamentous = NA),
		'6a1' = data.frame(flagella = 0, autotroph = 1, size = 'Large', colonial = 1, centric = 1, gelatinous = NA, aerotopes = NA, filamentous = NA),
		'6a2' = data.frame(flagella = 0, autotroph = 1, size = 'Large', colonial = 0, centric = 1, gelatinous = NA, aerotopes = NA, filamentous = NA),
		'6b1' = data.frame(flagella = 0, autotroph = 1, size = 'Large', colonial = 1, centric = 0, gelatinous = NA, aerotopes = NA, filamentous = NA),
		'6b2' = data.frame(flagella = 0, autotroph = 1, size = 'Large', colonial = 0, centric = 0, gelatinous = NA, aerotopes = NA, filamentous = NA),
		'7a' = data.frame(flagella = 0, autotroph = 1, size = 'Small', colonial = NA, centric = 1, gelatinous = NA, aerotopes = NA, filamentous = NA),
		'7b' = data.frame(flagella = 0, autotroph = 1, size = 'Small', colonial = NA, centric = 0, gelatinous = NA, aerotopes = NA, filamentous = NA),
		'8a' = data.frame(flagella = 0, autotroph = 1, size = 'Large', colonial = 0, centric = NA, gelatinous = NA, aerotopes = NA, filamentous = NA),
		'8b' = data.frame(flagella = 0, autotroph = 1, size = 'Large', colonial = 0, centric = NA, gelatinous = NA, aerotopes = NA, filamentous = NA),
		'9a' = data.frame(flagella = 0, autotroph = 1, size = 'Small', colonial = 0, centric = NA, gelatinous = NA, aerotopes = NA, filamentous = NA),
		'9b' = data.frame(flagella = 0, autotroph = 1, size = 'Small', colonial = 0, centric = NA, gelatinous = NA, aerotopes = NA, filamentous = NA),
		'9c' = data.frame(flagella = 0, autotroph = 1, size = 'Small', colonial = 0, centric = NA, gelatinous = NA, aerotopes = NA, filamentous = NA),
		'9d' = data.frame(flagella = 0, autotroph = 1, size = 'Small', colonial = 0, centric = NA, gelatinous = NA, aerotopes = NA, filamentous = NA),
		'10a'= data.frame(flagella = 0, autotroph = 1, size = NA, colonial = 1, centric = NA, gelatinous = NA, aerotopes = NA, filamentous = 1),
		'10b'= data.frame(flagella = 0, autotroph = 1, size = NA, colonial = 1, centric = NA, gelatinous = NA, aerotopes = NA, filamentous = 1),
		'10c'= data.frame(flagella = 0, autotroph = 1, size = NA, colonial = 1, centric = NA, gelatinous = NA, aerotopes = NA, filamentous = 1),
		'11a'= data.frame(flagella = 0, autotroph = 1, size = NA, colonial = 1, centric = NA, gelatinous = 0, aerotopes = NA, filamentous = 0),
		'11b'= data.frame(flagella = 0, autotroph = 1, size = NA, colonial = 1, centric = NA, gelatinous = 1, aerotopes = NA, filamentous = 0),
		'11c'= data.frame(flagella = 0, autotroph = 1, size = NA, colonial = 1, centric = NA, gelatinous = NA, aerotopes = NA, filamentous = 0),
		stop("MFG not included")
		)

	if(is.na(MFG.num)){
	  trait.df=data.frame(flagella = NA, autotroph = NA, size = NA, colonial = NA, centric = NA, gelatinous = NA, aerotopes = NA, filamentous = NA)
	}

	
		trait.df=cbind(MFG,trait.df)
		return(trait.df)
}

#this function applies the mfg.to.traits function to an entire dataframe
mfg.to.traits.df<-function(df) 
{
	trait.df<-vector()
	for(i in 1:dim(df)[1])
	{
	  print(i)
		trait.df=rbind(trait.df,mfg.to.traits(df$MFG[i]))
	}
	trait.df=trait.df[!duplicated(trait.df),]
	trait.df=merge(df,trait.df)
	return(trait.df)
}

mfg.to.traits.df1 <- function(mfg){ #mfg = vector with MFG according to Salmaso et al., 2007
    mfg.list <- list()
    for(i in 1:length(mfg)){
       mfg.list[[i]] <- mfg.to.traits(mfg[i])
      }
    mfg.df <- do.call(rbind, mfg.list)
    return(mfg.df)
}

