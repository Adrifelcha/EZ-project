VerticalHist <- function(x, xscale = NULL, xwidth, hist,
                         fillCol = "gray80", lineCol = "gray40") {
  ## x - the x position of each histogram
  ## xscale - "height" of the tallest bar (horizontally),
  ## xwidth - horizontal spacing between histograms
  ## hist - an object of type "histogram" (i.e., with $breaks and $density)
  binWidth <- hist$breaks[2] - hist$breaks[1]
  if (is.null(xscale)) xscale <- xwidth * 0.90 / max(hist$density)
  n <- length(hist$density)
  
  right_x.l <- rep(x, n)
  right_x.r <- right_x.l + hist$density * xscale
  left_x.r <- rep(x, n)
  left_x.l <- left_x.r - hist$density * xscale
  x.l = c(left_x.l,right_x.l)
  x.r = c(left_x.r,right_x.r)
  
  y.b <- hist$breaks[1:n]
  y.b = rep(y.b,2)
  y.t <- hist$breaks[2:(n + 1)]
  y.t = rep(y.t,2)
  
  rect(xleft = x.l, ybottom = y.b, xright = x.r, ytop = y.t,
       col = fillCol, border = fillCol)
}