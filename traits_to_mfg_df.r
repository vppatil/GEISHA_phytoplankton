#wrapper function to apply traits.to.mfg to an entire trait matrix. the second argument is a character vector of column names containing the trait values.
#The order of the column names should be: flagella, size, colonial, filament,centric,gelatinous, aerotopes,class, order
traits_to_mfg_df<-function(dframe,arg.names)
{
  dframe$mfg.from.traits=''
  for(i in 1:dim(dframe)[1])
  {
    dframe$mfg.from.traits[i]=traits.to.mfg(dframe[[arg.names[1]]][i],dframe[[arg.names[2]]][i],dframe[[arg.names[3]]][i],dframe[[arg.names[4]]][i],dframe[[arg.names[5]]][i],dframe[[arg.names[6]]][i],dframe[[arg.names[7]]][i],dframe[[arg.names[8]]][i],dframe[[arg.names[9]]][i])
  }
  
  return(dframe)
}
