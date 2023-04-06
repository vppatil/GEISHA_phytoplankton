library(curl)
library(lubridate)
library(rjsonlite)
library(algaeClassify)

file.edit("~/.Renviron")

phytodat<-read.csv('alllakes_algaebase_long.csv')
head(phytodat)

phytosub<-subset(phytodat,select=c('phyto_name'))
phytosub<-genus_species_extract(phytosub,'phyto_name')
phytosub<-phytosub[!duplicated(phytosub),]
phytosub<-phytosub[order(phytosub$phyto_name),]

n=10
test.dat<-phytosub[sample(1:nrow(phytosub),n,replace=FALSE),]

algaebase_search_df(test.dat,api_file = 'keyfile.txt')

nrows=n
pb <- txtProgressBar(min=1,max = nrows)

for(i in 1:n)
{
	hist(rnorm(10000,0,1))
	Sys.sleep(1)
	setTxtProgressBar(pb, i)
}
close(pb)