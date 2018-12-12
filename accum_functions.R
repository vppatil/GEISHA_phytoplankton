####
##datemat.create
setwd('~/gleon/Geisha/datasets/phyto_data/')
load('cheney_accumtest.RData')

###note, that if you specify time.agg = month,year,or my, you could aggregate in different ways. 
##also, 
date_mat<-function(phyto.df,abundance.var='biovol_um3_ml',taxa.name='phyto_name',date.name='date_dd_mm_yy',format='%d-%m-%y',time.agg='day')
{
  library(lubridate)
  
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

plotaccum = function(mue, n,save.pdf=F,lakename=NA) {
  mue = mue[order(mue$date_dd_mm_yy),]
  
  if(save.pdf)
  {
    pdf(paste('cumTaxa',lakename,'.pdf',sep=''))
    plot(c(1:dim(subset(mue, n == 0))[1]) ~ date_dd_mm_yy, data = subset(mue, n == 0), ylab  = 'cum number of taxa', xlab = '')
    mue = subset(mue, date_dd_mm_yy > 0)
    
    for(i in 1:n){
      xx = subset(mue, mue$n == i)
      xx$date_dd_mm_yy=as.POSIXct(xx$date_dd_mm_yy,origin = '1970-01-01')
      
      xxx = aggregate(date_dd_mm_yy ~ phyto_name, data = xx, min)
      xxx = xxx[order(xxx$date_dd_mm_yy),]
      
      lines(c(1:dim(xxx)[1]) ~ date_dd_mm_yy, data = xxx, type = 'l', col = 'grey')
    }
    
  dev.off()
  }else{
    plot(c(1:dim(subset(mue, n == 0))[1]) ~ date_dd_mm_yy, data = subset(mue, n == 0), ylab  = 'cum number of taxa', xlab = '')
    mue = subset(mue, date_dd_mm_yy > 0)
    
    for(i in 1:n){
      xx = subset(mue, mue$n == i)
      xx$date_dd_mm_yy=as.POSIXct(xx$date_dd_mm_yy,origin = '1970-01-01')
      
      xxx = aggregate(date_dd_mm_yy ~ phyto_name, data = xx, min)
      xxx = xxx[order(xxx$date_dd_mm_yy),]
      
      lines(c(1:dim(xxx)[1]) ~ date_dd_mm_yy, data = xxx, type = 'l', col = 'grey')
    }
    
  }
}

library(algaeClassify)
abd.col=c(11,11,11,11,10,11,11,11,12,12,12,11,11,12,11,12,11,11,10,11)
for(i in 1:length(lakes))
{
  
  lakename=strsplit(lakes[i],split='_')[[1]][2]
  lake=read.csv(lakes[i],stringsAsFactors=F)
  lake<-genus_species_extract(lake,'phyto_name')
  lake$phyto_name=paste(lake$genus,lake$species)
  lake$phyto_name=trimws(lake$phyto_name,'both')
  lake.accum=accum(lake,column=abd.col[i],50,save.pdf=T,lakename=lakename)
  plotaccum(lake.accum,50,save.pdf=T,lakename=lakename)
  
}
  
sampeff = function(b_data,column) {
  biovol=b_data[[column]]
  b_data = subset(b_data, biovol > 0)  
  sbio = aggregate(biovol ~ date_dd_mm_yy, data = b_data, sum)
  names(sbio)[2] = 'sbiovol'
  sbio2 = merge(b_data, sbio, by = 'date_dd_mm_yy', all = T)
  sbio2$relbiovol = sbio2$biovol /sbio2$sbiovol
  minbio = aggregate(relbiovol ~ date_dd_mm_yy, data = sbio2, min)
  minbio$date_dd_mm_yy=as.POSIXct(minbio$date_dd_mm_yy,format='%d-%m-%y')
  
  plot(I(100 * relbiovol) ~ date_dd_mm_yy, data = minbio, log = 'y', xlab = '', ylab = 'Minimum contribution [%]')
  
  
}

ba = function(data) {
  data$date_dd_mm_yy=as.POSIXct(data$date_dd_mm_yy)
  data = subset(data, biovol > 0)
  data$year = as.numeric(format(data$date_dd_mm_yy, "%Y"))
  st = as.data.frame(table(data$stationid)  )
  st2 = as.data.frame(table(data$stationid, data$date_dd_mm_yy, data$year)  )
  st2 = subset(st2, Freq > 0)
  names(st2)[1:3] = c('stationid','date_dd_mm_yy','year')
  st3 =   as.data.frame(table(st2$stationid,  st2$year)  )
  mf = mean(st3$Freq)
  
}
