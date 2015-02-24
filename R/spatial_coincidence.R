
SpatLagOvl <- function(x0, y0, line, dframe, delta_s){
  pnt <- SpatialPoints(cbind(x0, y0), CRS("+init=epsg:2400"))
  buf <- gBuffer(pnt, width=delta_s)
  if(gDisjoint(buf,line))
    return(F)
  else{
    # find time point on line
    sgm <- gIntersection(buf, line)
    if (nrow(sgm@lines[[1]]@Lines[[1]]@coords)!=2){
      ip <- as.integer((0.5+nrow(sgm@lines[[1]]@Lines[[1]]@coords)/2))
      # use centre point
      xp <- sgm@lines[[1]]@Lines[[1]]@coords[ip,1]
      yp <- sgm@lines[[1]]@Lines[[1]]@coords[ip,2]
      tp <- dframe$tspan[which(abs(dframe$Locale_E - xp) < 0.01 &
                                 abs(dframe$Locale_N - yp) < 0.01)][1]
    }
    else{
      xp <- 0.5*(sgm@lines[[1]]@Lines[[1]]@coords[1,1]+
                   sgm@lines[[1]]@Lines[[1]]@coords[2,1])
      yp <- 0.5*(sgm@lines[[1]]@Lines[[1]]@coords[1,2]+
                   sgm@lines[[1]]@Lines[[1]]@coords[2,2])
      for (ind in 2:nrow(dframe))
        if (OnLineSegment(xp,yp,dframe$Locale_E[ind-1],
                          dframe$Locale_N[ind-1],dframe$Locale_E[ind],
                          dframe$Locale_N[ind]))
          tp <- FindTime(xp,yp,ind,dframe)
    }
    return(tp)
  }
}