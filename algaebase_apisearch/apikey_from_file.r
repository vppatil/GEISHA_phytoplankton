#set api key from file, so you don't have to leave it in your scripts
apikey_from_file<-function(keyfile)
{
  #keyfile should be a text file with the API key on the first line
  apikey<-readLines(con=keyfile,n=1,warn=FALSE)
  invisible(apikey) #will invisibly return result
  
  #e.g. apikey<-apikey.from.file(keyfile)
}
