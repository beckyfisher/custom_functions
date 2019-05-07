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