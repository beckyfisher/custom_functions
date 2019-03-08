#' remove_nan
#'
#' Removes NaN and replaces with NA in a numeric vector
#'
#' @param  x A Numeric vector
#'
#' @export
#' @return A numeric vector with any NaN values replaced with NA
#'
remove_nan=function(x){
  x[which(is.nan(x))]=NA
  return(x)}

