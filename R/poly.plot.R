#' poly.plot.list
#'
#' Plot a list of values as polygons on the existing plot
#'
#' @param  x A list of x values for plotting - must contain two
#' elements representing the start and end values of the polygons
#'
#' @param  y.lim The y values over which to draw the polygons
 poly.plot.list <- function(x,y.lim,alpha.f=0.3,col.val="grey"){
   for(d in 1:length(x)){
      x.d=x[[d]]
      polygon(c(x.d[1],x.d[1],
                x.d[2],x.d[2]),
                c(y.lim, rev(y.lim)),
               col=adjustcolor(col.val, alpha.f = alpha.f), border=NA)}
 }


#' poly.plot.mat
#'
#' Plot polygons on the existing plot from a supplied matrix of values
#'
#' @param  x The matrix/data.frame from which to obtain values for plotting
#'
#' @param  start.col The column in the matrix or data.frame that defines the start plotting value for each row
#'
#' @param  end.col The column in the matrix or data.frame that defines the end plotting value for each row
#'
#' @param  y.lim The y values over which to draw the polygons

 poly.plot.mat <- function(x,start.col,end.col,y.lim,alpha.f=1,col.val="yellow"){
  for(r in 1:nrow(x)){
   polygon(c(x[r,start.col], cyc.hist[r,start.col],
            x[r,end.col], x[r,end.col]),
        c(min(y.lim),max(y.lim),max(y.lim),min(y.lim)),
               col=adjustcolor(col.val,alpha=alpha.f),border=NA,xpd=F)}
}
