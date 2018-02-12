#------
# CSR validation 
#------
library(ggplot2)
library(scales)
library(vegan)

rm(list = ls())

dat.m = read.csv("madgwick_csr_data.csv",stringsAsFactors = F)

source('species_mfg_convert02042018.r')
source('mfg_csr.r')

dat.m$SV=dat.m$surface.area/dat.m$volume
dat.m$MSV=dat.m$mld*dat.m$SV

dat.m<-genus.species.extract(dat.m,'Species')
dat.m<-phyto.convert.df(dat.m)

dat.m$MFG.number=sapply(dat.m$MFG,function(x) strsplit(gsub('-','_',x),split='_')[[1]][1])
dat.m=mfg.csr.convert.df(dat.m,'MFG.number')

dat.m

s.group=dat.m[dat.m$CSR=='S',]

#final plot
ggplot(dat.m, aes(x = MSV, y = SV, color = CSR)) + 
  geom_point(size=3,shape=19,position=position_jitter(width=0.1,height=0.1))+
  theme(text=element_text(face="bold",size=20))+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),limits=c(2,1000)) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),limits=c(0.01,10))+
  xlab("msv-1")+ylab("sv-1")+
  annotate("text",x=10,y=10,label="C",fontface=2,size=10)+
  annotate("text",x=1000,y=10,label="R",fontface=2,size=10)+
  annotate("text",x=50,y=0.02,label="S",fontface=2,size=10) 
#geom_text(aes(label = sh.name), hjust = 0, vjust = 0) 

#C final plot
ggplot(out.df[out.df$CSR=='C',], aes(x = MSV, y = SV, color = CSR)) + 
  geom_point(size=3,shape=19,position=position_jitter(width=0.1,height=0.1))+
  theme(text=element_text(face="bold",size=20))+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),limits=c(2,1000)) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),limits=c(0.01,10))+
  xlab("msv-1")+ylab("sv-1")+
  annotate("text",x=10,y=10,label="C",fontface=2,size=10)+
  annotate("text",x=1000,y=10,label="R",fontface=2,size=10)+
  annotate("text",x=50,y=0.02,label="S",fontface=2,size=10) 
#geom_text(aes(label = sh.name), hjust = 0, vjust = 0) 

#C final plot
ggplot(out.df[out.df$CSR=='R',], aes(x = MSV, y = SV, color = 'blue')) + 
  geom_point(size=3,shape=19,position=position_jitter(width=0.1,height=0.1))+
  theme(text=element_text(face="bold",size=20))+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),limits=c(2,1000)) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),limits=c(0.01,10))+
  xlab("msv-1")+ylab("sv-1")+
  annotate("text",x=10,y=10,label="C",fontface=2,size=10)+
  annotate("text",x=1000,y=10,label="R",fontface=2,size=10)+
  annotate("text",x=50,y=0.02,label="S",fontface=2,size=10) 
#geom_text(aes(label = sh.name), hjust = 0, vjust = 0) 

#C final plot
ggplot(out.df[out.df$CSR=='CR',], aes(x = MSV, y = SV, color = 'blue')) + 
  geom_point(size=3,shape=19,position=position_jitter(width=0.1,height=0.1))+
  theme(text=element_text(face="bold",size=20))+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),limits=c(2,1000)) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),limits=c(0.01,10))+
  xlab("msv-1")+ylab("sv-1")+
  annotate("text",x=10,y=10,label="C",fontface=2,size=10)+
  annotate("text",x=1000,y=10,label="R",fontface=2,size=10)+
  annotate("text",x=50,y=0.02,label="S",fontface=2,size=10) 


#C final plot
ggplot(out.df[out.df$CSR=='S',], aes(x = MSV, y = SV)) + 
  geom_point(size=3,shape=19,position=position_jitter(width=0.1,height=0.1))+
  theme(text=element_text(face="bold",size=20))+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),limits=c(2,1000)) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),limits=c(0.01,10))+
  xlab("msv-1")+ylab("sv-1")+
  annotate("text",x=10,y=10,label="C",fontface=2,size=10)+
  annotate("text",x=1000,y=10,label="R",fontface=2,size=10)+
  annotate("text",x=50,y=0.02,label="S",fontface=2,size=10) 


#####
#code for ordination validation
library(vegan)
library(labdsv)

out.df=subset(out.df,!is.na(out.df$CSR))
out.df.measurements=subset(out.df,select=c('MSV','SV'))
group.df=subset(out.df,select=c('CSR','MFG','group'))

out.df.dist=dist(out.df.measurements)
csr.cca=cca(out.df.measurements~CSR+MFG+1,data=group.df)
anova(csr.cca)

csr.kmeans=kmeans(dist(out.df.measurements),3)
library(fpc)
plotcluster(out.df.measurements, csr.kmeans$cluster)

# vary parameters for most readable graph
library(cluster) 
clusplot(out.df.measurements, csr.kmeans$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

library(fpc)
csr.kmeans2=kmeans(dist(out.df.measurements),6)

cluster.stats(out.df.dist, csr.kmeans$cluster, csr.kmeans2$cluster)

library(randomForest)
csr.rf=randomForest(CSR~MSV+SV,data=out.df)

