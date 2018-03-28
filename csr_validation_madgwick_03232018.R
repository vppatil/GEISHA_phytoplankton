#------
# CSR validation 
#------
library(ggplot2)
library(scales)
library(vegan)

rm(list = ls())

setwd('~/gleon/Geisha/phyto_package/GEISHA_phytoplankton_github_shared/')
dat.m = read.csv("madgwick_csr_data.csv",stringsAsFactors = F)

source('species_mfg_convert02042018.r')
source('mfg_csr.r')

dat.m$SV=dat.m$surface.area/dat.m$volume
dat.m$MSV=dat.m$mld*dat.m$SV

dat.m<-genus.species.extract(dat.m,'Species')
dat.m<-phyto.convert.df(dat.m)

dat.m$MFG[dat.m$Species=='Osscilatoria']='5a-FilaCyano'
dat.m$MFG[dat.m$Species=='Elaktothrix']='11b-GelaChlor'

dat.m$MFG.number=sapply(dat.m$MFG,function(x) strsplit(gsub('-','_',x),split='_')[[1]][1])
dat.m=mfg.csr.convert.df(dat.m,'MFG.number')

dat.m$CSR[dat.m$Species=='Small non-flagellates'|dat.m$Species=='Small flagellates']='C'

dat.m

s.group=dat.m[dat.m$CSR=='S',]

#final plot
csr.plot=ggplot(dat.m, aes(x = MSV, y = SV, color = CSR)) + 
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

#final plot
mfg.plot=ggplot(dat.m, aes(x = MSV, y = SV, color = MFG)) + 
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


multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

multiplot(csr.plot,mfg.plot,cols=1)

#####
#code for hierarchical clustering validation
library(vegan)
library(labdsv)

out.df=dat.m

#optional phase out
out.df=out.df[out.df$CSR %in% c('C','CR','S'),]

out.df=subset(out.df,!is.na(out.df$CSR))
out.df.measurements=subset(out.df,select=c('MSV','SV'))
group.df=subset(out.df,select=c('CSR','MFG'))

#log transform
out.df.measurements=sapply(out.df.measurements[],log)

out.df.dist=dist(out.df.measurements)

hc=hclust(out.df.dist)
plot(hc,labels=out.df$CSR)

csr.ord=cmdscale(out.df.dist)
csr.ord=metaMDS(out.df.dist)

ord.env=envfit(csr.ord~CSR,data=group.df)
ord.env

plot(csr.ord)
plot(ord.env)

