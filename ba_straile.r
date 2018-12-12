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