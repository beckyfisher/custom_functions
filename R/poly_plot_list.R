#' poly.plot.list
#'
#' Plot a list of values as polygons on the existing plot
#'
#' @param  x A list of x values for plotting - must contain two
#' elements representing the start and end values of the polygons
#'
#' @param  y.lim The y values over which to draw the polygons
#'
#' @export
#' @return
#'
 poly.plot.list <- function(x,y.lim,alpha.f=0.3,col.val="grey"){
   for(d in 1:length(x)){
      x.d=x[[d]]
      polygon(c(x.d[1],x.d[1],
                x.d[2],x.d[2]),
                c(y.lim, rev(y.lim)),
               col=adjustcolor(col.val, alpha.f = alpha.f), border=NA)}
}
