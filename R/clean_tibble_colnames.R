

#' clean.tibble.colnames
#'
#' Function to remove illegal characters in tibbles imported through tidyverse methods.
#'
#' @param  x The tibble or data.frame to clean
#'
#' @details Removes illegal characters in column names allowed in tibbles but would not normally be allowed in base R. this is to make them usable with code that odes not support this functionality of tibbles. Currently replaces " " with "." and all other characters with "".
#'
#' @export
#' @return x with column names stripped of illegal characters.
#'
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


