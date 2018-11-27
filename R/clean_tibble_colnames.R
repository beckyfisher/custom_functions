
clean.tibble.colnames <- function(x){
   name.vals = names(x)
   name.vals <- gsub(" " , ".",name.vals,fixed=T)
   name.vals <- gsub(",", "",name.vals,fixed=T)
   name.vals <- gsub("(" , "",name.vals,fixed=T)
   name.vals <- gsub(")" , "",name.vals,fixed=T)
   name.vals <- gsub("%" , "",name.vals,fixed=T)
   name.vals <- gsub("-" , "",name.vals,fixed=T)
   names(x)<-name.vals
   return(x)}


