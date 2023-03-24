#helper function to pull out json output
#may be a simpler way of doing this.
#sticks in an NA instead of NULL
algaebase_output_parse<-function(x,field.name) {
  
  res<-x[[field.name]]
  if(is.null(res)){res=NA}
  return(res)
}