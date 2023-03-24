setwd('~/Downloads/')
library(jsonlite)
library(httr)

abtest<-fromJSON(txt = "algaebaseoutput.txt")
sptest<-fromJSON(txt = "genspp.txt")

#curl string that works- genus search
# curl -g -X GET -H "abapikey:pL9oQFjncdidBjxUJqWR9k9TdpHkH975" "https://api.algaebase.org/v1.3/genus?genus=Haematococcus"

#r command sequence that works:
API_KEY = "pL9oQFjncdidBjxUJqWR9k9TdpHkH975"
url = "https://api.algaebase.org/v1.3/genus?genus=Haematococcus"

speciesurl="https://api.algaebase.org/v1.3/species?genus=[cn]Navicula&specificEpithet=[cn]acrosphaeria"

httpResponse <- GET(url, add_headers("abapikey" = API_KEY), accept_json())
speciesResponse<-GET(speciesurl,add_headers("abapikey"=API_KEY),accept_json())

results = fromJSON(content(httpResponse, "text"))
species.results<-fromJSON(content(speciesResponse,"text"))

head(results)
names(results)
results[[1]] #can extract total number of results and total number of pages.
str(results$result)

species.results
write.csv(species.results,'species_results_text.csv')

algae_search<-function(genus,species,search.level='species',keyname="abapikey",key,partial.search=FALSE)
{
  if(search.level %in% c('genus','species')==FALSE){
    stop("Error: search.level must be either 'genus' or 'species'")
  }
  url.base="https://api.algaebase.org/v1.3/genus"
  url=paste(url.base,search.level,sep='/')
  
  header=paste(keyname,key,sep=':')
  if(search.level=='species'){
    q=GET(url=url.base,add_headers("abapikey"=key),query=list(genus=genus,specificEpithet=species))
  }else{
    q=GET(url=url.base,add_headers("abapikey"=key),query=list(genus=genus))
  }
  q.pretty<-toJSON(content(q,as="parsed",type = "application/json"))
  return(q.pretty)
}

genus="Cyclotella"
species="cyclopunctata"
search.level="species"
uname="abapikey"
key="pL9oQFjncdidBjxUJqWR9k9TdpHkH975"

test<-algae_search(genus=genus,species=species,key=key)

httr::GET(url = "https://api.algaebase.org/v1.3/genus",query=list(genus="Navicula"),add_headers(abapikey = "pL9oQFjncdidBjxUJqWR9k9TdpHkH975"),params="globoff")


curl -g -X GET -H "abapikey:pL9oQFjncdidBjxUJqWR9k9TdpHkH975" "https://api.algaebase.org/v1.3/genus?genus=Haematococcus"


genus.only=0

querystring<-paste0("\" \"https://api.algaebase.org/v1.3/species?genus=",taxa,"\"")
headerstring=paste(uname,key,sep=":")
firstpart<-"curl -g -X GET -H \""

curlstring<-paste0(firstpart,headerstring,querystring)
curlstring

library(curlconverter)
straighten(curlstring)
