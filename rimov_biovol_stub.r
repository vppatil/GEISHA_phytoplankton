####rimov biovol stub
####add in mfg new scripts

setwd('~/gleon/Geisha/Datasets/rawphytodata/')
rimov<-read.csv('PhytoMFG_Rimov_QAQC14Nov2017.csv')

rimov$MFG=as.character(rimov$MFG)
rimov$CSR=as.character(rimov$CSR)

rimov$MFG[is.na(rimov$MFG)]='unknown'
rimov$CSR[is.na(rimov$CSR)]='unknown'

rimov.biovol.agg=aggregate(biovol_um3_ml~date_dd_mm_yy+stationid+CSR,data=rimov,FUN=function(x) sum(na.omit(x)))

rimov.biovol.agg$date=as.POSIXct(rimov.biovol.agg$date_dd_mm_yy,format='%d-%m-%y')
rimov.biovol.agg<-aggregate(biovol_um3_ml~date+CSR,data=rimov.biovol.agg,FUN=function(x) mean(na.omit(x)))
rimov.biovol.agg<-rimov.biovol.agg[order(rimov.biovol.agg$date),]

rimov.biovol.agg2<-aggregate(biovol_um3_ml~date_dd_mm_yy+stationid,data=rimov,FUN=function(x) sum(na.omit(x)))
rimov.biovol.agg2$date=as.POSIXct(rimov.biovol.agg2$date_dd_mm_yy,format='%d-%m-%y')
rimov.biovol.agg2<-aggregate(biovol_um3_ml~date,data=rimov.biovol.agg2,FUN=function(x) mean(na.omit(x)))
rimov.biovol.agg2<-rimov.biovol.agg2[order(rimov.biovol.agg2$date),]


rimov.biovol.agg3=aggregate(biovol_um3_ml~date_dd_mm_yy+stationid+MFG,data=rimov,FUN=function(x) sum(na.omit(x)))
rimov.biovol.agg3$date=as.POSIXct(as.character(rimov.biovol.agg3$date_dd_mm_yy),format='%d-%m-%y')
rimov.biovol.agg3<-aggregate(biovol_um3_ml~date+MFG,data=rimov.biovol.agg3,FUN=function(x) mean(na.omit(x)))
rimov.biovol.agg3<-rimov.biovol.agg3[order(rimov.biovol.agg3$date),]

library(ggplot2)
ggplot()+geom_bar(data=rimov.biovol.agg3,aes(x=date,y=biovol_um3_ml,fill=MFG),stat='identity')

ggplot()+geom_bar(data=rimov.biovol.agg,aes(x=date,y=biovol_um3_ml,fill=CSR),stat='identity')

setwd('~/gleon/Geisha/phyto_package/GEISHA_phytoplankton_github_shared/phytodiv_output/')
write.csv(rimov.biovol.agg2,'rimov_totalbiovolume_timeseries.csv')
write.csv(rimov.biovol.agg,'rimov_csr_biovolume_timeseries.csv')
write.csv(rimov.biovol.agg3,'rimov_mfg_biovolume_timeseries.csv')
