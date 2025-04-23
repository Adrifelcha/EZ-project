#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
# This is a helper function that creates a vertical histogram.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!

VerticalHist <- function(x, xscale = NULL, xwidth, hist,
                         fillCol = "gray80", lineCol = "gray40") {
  ## x - the x position of each histogram
  ## xscale - "height" of the tallest bar (horizontally),
  ## xwidth - horizontal spacing between histograms
  ## hist - an object of type "histogram" (i.e., with $breaks and $density)
  # Calculate the width of each bin in the histogram
  # This is the distance between consecutive break points
  binWidth <- hist$breaks[2] - hist$breaks[1]
  
  # If xscale is not provided, calculate it based on the maximum density
  # This ensures the widest bar will take up 90% of the allocated width
  if (is.null(xscale)) xscale <- xwidth * 0.90 / max(hist$density)
  
  # Count the number of bins in the histogram
  n <- length(hist$density)
  
  # Calculate the x-coordinates for the right side of the histogram
  # For a right-facing histogram:
  right_x.l <- rep(x, n)                # Left edge at the center position
  right_x.r <- right_x.l + hist$density * xscale  # Right edge extends based on density
  
  # Calculate the x-coordinates for the left side of the histogram
  # For a left-facing histogram:
  left_x.r <- rep(x, n)                 # Right edge at the center position
  left_x.l <- left_x.r - hist$density * xscale   # Left edge extends based on density
  
  # Combine the left and right coordinates for all bars
  # This creates a symmetric histogram extending in both directions
  x.l = c(left_x.l, right_x.l)          # All left edges
  x.r = c(left_x.r, right_x.r)          # All right edges
  
  # Calculate the y-coordinates for the bottom of each bar
  # These are the lower break points of each bin
  y.b <- hist$breaks[1:n]
  y.b = rep(y.b, 2)                     # Duplicate for left and right sides
  
  # Calculate the y-coordinates for the top of each bar
  # These are the upper break points of each bin
  y.t <- hist$breaks[2:(n + 1)]
  y.t = rep(y.t, 2)                     # Duplicate for left and right sides
  
  # Draw the rectangles representing the histogram bars
  # Each rectangle is defined by its left, right, bottom, and top coordinates
  rect(xleft = x.l, ybottom = y.b, xright = x.r, ytop = y.t,
       col = fillCol, border = fillCol) 
}