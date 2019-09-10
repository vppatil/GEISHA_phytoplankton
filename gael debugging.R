#reproducing gael's problems with algaeclassify
setwd('~/gleon/Geisha/datasets/phyto_data/')
library(algaeClassify)

kasu<-read.csv('RawPhytoData_Kasumigaura_QAQC_17Aug2017.csv')
names(kasu)
table(is.na(kasu$biomass_ug_l))

test=accum(b_data=kasu,column=11,n=10)
#column = NA only works if it is only a list of species that were present on a given date.

data("lakegeneva")
lakegeneva.common.genera=phyto_ts_aggregate(lakegeneva,AbundanceVar='biovol_um3_ml',SummaryType='presence.absence',GroupingVar1='genus')

lakegeneva.common.genera=phyto_ts_aggregate(lakegeneva,AbundanceVar='biovol_um3_ml',SummaryType='abundance',GroupingVar1='genus')
