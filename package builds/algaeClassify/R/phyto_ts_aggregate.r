#' Aggregate phytoplankton timeseries based on abundance. Up to 3 grouping variables can be given:
#' e.g. genus, species, stationid, depth range.
#' If no abundance var is given, will aggregate to presence/absence of grouping vars.
#'
#' @param phyto.data data.frame
#' @param AbundanceVar character string with field name containing abundance data
#' @param DateVar      character string: field name for date variable. character or POSIX data.
#' @param GroupingVar1 character string: field name for first grouping variable. defaults to spp.
#' @param GroupingVar2 character string: name of additional grouping var field
#' @param GroupingVar3 character string: name of additional grouping var field
#' @param remove.rare  TRUE/FALSE. If TRUE, removes all instances of GroupingVar1 that occur < 5%
#'                     of time periods.
#' @param fun          function used to aggregate abundance based on grouping variables
#' @param format       character string: format for DateVar POSIXct conversion
#'
#' @export phyto_ts_aggregate
#' 
#' @return a data.frame with grouping vars, date_dd_mm_yy, and abundance or presence/absence
#' 
#' @examples
#' data(lakegeneva)
#' lakegeneva<-genus_species_extract(lakegeneva,'phyto_name')
#' lakegeneva.common.genera=phyto_ts_aggregate(lakegeneva,AbundanceVar=NA,GroupingVar1='genus')
#' head(lakegeneva.common.genera)
#' 
#' @seealso \url{http://www.algaebase.org} for up-to-date phytoplankton taxonomy,
#'     \url{https://powellcenter.usgs.gov/geisha} for project information


phyto_ts_aggregate=function(phyto.data,DateVar='date_dd_mm_yy',AbundanceVar='biovol_um3_ml',GroupingVar1='phyto_name',GroupingVar2=NA,GroupingVar3=NA,
                            remove.rare=F,fun=sum,format='%d-%m-%y')
{
  
  phyto.data$date_dd_mm_yy=as.POSIXct(phyto.data[[DateVar]],format=format)
  
  if(remove.rare)
  {
    rare.tab=table(phyto.data[[GroupingVar1]],phyto.data$date_dd_mm_yy)
    rare.tab=ifelse(rare.tab>0,1,0)
    num.occur=dim(rare.tab)[2]
    freq.occur=apply(rare.tab,1,sum)
    rare.spp=row.names(rare.tab)[freq.occur<(num.occur*.05)]
    phyto.data=phyto.data[phyto.data[[GroupingVar1]] %in% rare.spp==FALSE,]
  }
  
  groupingvars=c(GroupingVar1,GroupingVar2,GroupingVar3)
  groupingvars=stats::na.omit(groupingvars)
  
  if(is.na(AbundanceVar))
  {
    phyto.data$presence=1;
    AbundanceVar='presence'
    fun=max; ##presence absence if no abundance var
  }
  
  for(i in 1:length(groupingvars))
  {
    phyto.data[[groupingvars[i]]][is.na(phyto.data[[groupingvars[i]]])]=-9999
  }

  groupingvars=paste(groupingvars,collapse=' + ')
  
  agg.func=stats::formula(paste(AbundanceVar,"~ date_dd_mm_yy + ",groupingvars))
  
  ts.agg=stats::aggregate(formula=agg.func,data=phyto.data,FUN=fun)
  
  return(ts.agg)

  
  
}
