
OnLineSegment <- function(x0,y0, x1,y1, x2,y2){
  # Returns TRUE if (x0,y0) is on the line segment between (x1,y1) and
  # (x2,y2)
  if (x0 < min(x1,x2) || y0 < min(y1,y2) || x0 > max(x1,x2) || y0 > max(y1,y2)) return(F)
  else
    dist <- ifelse((x2 == x1 && y2 == y1), sqrt((x1-x0)^2 + (y1-y0)^2), 
            abs((x2-x1)*(y1-y0) - (x1-x0)*(y2-y1)) / sqrt((x2-x1)^2 + (y2-y1)^2))
  dist <= 0.1
}

FindTime <- function(x0,y0,index,dframe){
  # Based on (x0,y0) Linearly interpolates time between
  # location[index-1] and location[index]
  dist1 <- sqrt((dframe$Locale_E[index-1] - x0)^2 +
                  (dframe$Locale_N[index-1] - y0)^2)
  dist2 <- sqrt((dframe$Locale_E[index] - x0)^2 +
                  (dframe$Locale_N[index] - y0)^2)
  dist3 <- dist1 + dist2
  as.numeric(dframe$tspan[index]*dist1/dist3 + dframe$tspan[index-1]
             *dist2/dist3)
}