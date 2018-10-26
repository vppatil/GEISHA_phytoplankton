#' Transform a phytoplankton timeseries into a matrix of abundances for ordination
#'
#' @param phyto.df Name of data.frame object
#' @param abundance.var Character string: field containing abundance data. NA for presence/absence
#' @param taxa.name Character string: field containing taxonomic identifiers
#' @param date.name Character string: field containing date.
#' @param format Character string: POSIX format string for formatting date column
#' @param timeagg Character string: time interval for aggregating abundance. default is day.
#' 
#' @export date_mat
#' 
#' @return A matrix of phytoplankton abundance, with taxa in rows and time in columns.
#'         If time.agg = 'monthday', returns a 3dimensional matrix (taxa,month,year).
#'         If abundance.var = NA, matrix cells will be 1 for present, 0 for absent
#' 
#' @examples
#' data(lakegeneva)
#' #example dataset with 50 rows
#' 
#' geneva.mat<-date_mat(lakegeneva,abundance.var=NA)
#' 
#' geneva.mat

date_mat<-function(phyto.df,abundance.var='biovol_um3_ml',taxa.name='phyto_name',
                   date.name='date_dd_mm_yy',format='%d-%m-%y',
                   time.agg=c('day','month','year','monthyear'))
{
  library(lubridate)
  time.agg=time.agg[1]
  
  if(is.na(abundance.var)) #if only a species list, create a presence/absence matrix
  {
    phyto.df$presence=1
    abundance.var='presence'
  }
  
  mean.nona=function(x) mean(x[!is.na(x)])
  
  phyto.df$date_dd_mm_yy=as.POSIXct(phyto.df$date_dd_mm_yy,format=format)
  if(time.agg=='month')
  {
    phyto.df$month=month(phyto.df$date_dd_mm_yy)
    phyto.agg=aggregate(formula(paste(abundance.var,'~',taxa.name,'+ month')),data=phyto.df,FUN=mean.nona)
    
    phyto.mat=tapply(phyto.agg[[abundance.var]],list(phyto.agg$month,phyto.agg[[taxa.name]]),mean.nona)
    phyto.mat[is.na(phyto.mat)]=0
    
    return(phyto.mat)  
  }else if(time.agg=='year')
  {
    phyto.df$year=year(phyto.df$date_dd_mm_yy)
    phyto.agg=aggregate(formula(paste(abundance.var,'~',taxa.name,'+ year')),data=phyto.df,FUN=mean.nona)
    
    phyto.mat=tapply(phyto.agg[[abundance.var]],list(phyto.agg$year,phyto.agg[[taxa.name]]),mean.nona)
    phyto.mat[is.na(phyto.mat)]=0
    
    return(phyto.mat)  
  }else if(time.agg == 'yearmonth')
  {
    phyto.df$year=year(phyto.df$date_dd_mm_yy)
    phyto.df$month=month(phyto.df$date_dd_mm_yy)
    
    phyto.agg=aggregate(formula(paste(abundance.var,'~',taxa.name,'+ year +month')),data=phyto.df,FUN=mean.nona)
    
    phyto.mat=tapply(phyto.agg[[abundance.var]],list(phyto.agg$month,phyto.agg[[taxa.name]],phyto.agg$year),mean.nona)
    phyto.mat[is.na(phyto.mat)]=0
    
    return(phyto.mat)
  }
  
  mean.nona=function(x) mean(x[!is.na(x)])
  
  phyto.agg=aggregate(formula(paste(abundance.var,'~',taxa.name,'+',date.name)),data=phyto.df,FUN=mean.nona)
  
  phyto.mat=tapply(phyto.agg[[abundance.var]],list(phyto.agg[[date.name]],phyto.agg[[taxa.name]]),mean.nona)
  phyto.mat[is.na(phyto.mat)]=0
  
  return(phyto.mat)
  
}


accum = function(b_data, column, n,save.pdf=F,lakename=NA) {
  b_data = subset(b_data, b_data[, column] > 0)  
  
  ntaxa0 = as.data.frame(table(b_data$date_dd_mm_yy, b_data$phyto_name))
  
  ntaxa0 = subset(ntaxa0, Freq > 0)
  
  
  
  
  ntaxa = as.data.frame(table(ntaxa0$Var1))
  ntaxa$date_dd_mm_yy = as.POSIXct(as.character(ntaxa$Var1),format='%d-%m-%y')
  
  if(save.pdf)
  {
    pdf(paste('Richness',lakename,'.pdf',sep=''))
    plot(Freq ~ date_dd_mm_yy, data = ntaxa, pch = 19, ylab = 'number of taxa', xlab = '')
    dev.off()
    
  }else{
    plot(Freq ~ date_dd_mm_yy, data = ntaxa, pch = 19, ylab = 'number of taxa', xlab = '')
    
  }
  
  sbio = aggregate(b_data[, column] ~ phyto_name + date_dd_mm_yy, data = b_data, sum)  
  sbio$date_dd_mm_yy=as.POSIXct(sbio$date_dd_mm_yy,format='%d-%m-%y')
  
  rich = as.data.frame(table(sbio$date_dd_mm_yy),stringsAsFactors = F)  
  rich$Var1=as.POSIXct(rich$Var1,format='%Y-%m-%d')
  
  rich=rich[match(unique(sbio$date_dd_mm_yy),rich$Var1),]
  first = aggregate(date_dd_mm_yy ~ phyto_name, data = sbio, min)
  first$n = 0
  
  
  num.taxa=length(rich$Var1)
  freq=rich$Freq
  
  row.end=cumsum(rep(sum(freq),n))
  
  row.start=1
  for(i in 2:n)
  {
    row.start[i]=row.end[i-1]+1
  }
  
  simul.length=n * sum(freq)
  
  
  nruns = data.frame (phyto_name=vector(length=simul.length),date_dd_mm_yy = vector(length=simul.length),n=vector(length=simul.length))
  
  for (j in 1:n){
    tt = data.frame( phyto_name = NULL,date_dd_mm_yy = NULL) 
    for (i in 1:length(rich$Var1)) {
      x1 = subset(sbio, date_dd_mm_yy == unique(sbio$date_dd_mm_yy)[i])
      
      ttt =  data.frame(phyto_name = sample(first$phyto_name, as.numeric(table(x1$date_dd_mm_yy)), replace = F), date_dd_mm_yy = unique(sbio$date_dd_mm_yy)[i],stringsAsFactors = F)
      tt = rbind(tt, ttt)
      
    }
    tt$n=j
    nruns[row.start[j]:row.end[j],] = tt
  }
  nruns$date_dd_mm_yy = as.POSIXct(nruns$date_dd_mm_yy, origin = '1970-01-01')
  nruns$n=rep(1:n,each=sum(freq))
  out = rbind(first, nruns)
}
