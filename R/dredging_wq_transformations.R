
#' ntu.xform
#'
#' Applies a transformation
#'
#' @param  x A numeric vector of raw NTU values
#'
#' @export
#' @return A numeric vector of transformed values.
ntu.xform=function(x){return((10^x)-1)}
#' dli.xform
#'
#' Applies a transformation
#'
#' @param  x A numeric vector of raw DLI values
#'
#' @export
#' @return A numeric vector of transformed values.
dli.xform=function(x){return(((1-x)^3) * 30)}
#' assd.xform
#'
#' Applies a transformation
#'
#' @param  x A numeric vector of raw ssd values
#'
#' @export
#' @return A numeric vector of transformed values.
assd.xform=function(x){return(x^2)}
#' xform
#'
#' Applies a transformation
#'
#' @param  x A numeric vector of raw NTU values
#'
#' @param  var.name A character string indicating the variable being transformed
#'
#' @export
#' @return A numeric vector of transformed values.
xform=function(x,var.name){
       x.out=x
       if(length(grep("dli.i",var.name))>0){
         x.out=dli.xform(x.out)}
       if(length(grep("ntu.i",var.name))>0){
         x.out=ntu.xform(x.out)}
       if(length(grep("assd.i",var.name))>0){
         x.out=assd.xform(x.out)}
       return(x.out)}
