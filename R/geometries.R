if (!require("sp")) install.packages("sp")
if (!require("rgeos")) install.packages("rgeos")
if (!require("rgdal")) install.packages("rgdal")

# coordinates of two points identiefied in Google Earth, for example
pnt1_xy <- cbind(5.6660, 51.9872) # enter your own coordinates
pnt2_xy <- cbind(5.121621, 52.1018) # enter your own coordinates

# combine coordinates in single matrix
coords <- rbind(pnt1_xy, pnt2_xy)

# make spatial points object
prj_string_WGS <- CRS("+proj=longlat +datum=WGS84")
mypoints <- SpatialPoints(coords, proj4string=prj_string_WGS)

# inspect object
class(mypoints)
str(mypoints)

# create and display some attribute data and store in a data frame
mydata <- data.frame(cbind(id = c(1,2),
                           Name = c("WUR - Gaia", "Home - Utrecht")))

# make spatial points data frame
mypointsdf <- SpatialPointsDataFrame(
  coords, data = mydata,
  proj4string=prj_string_WGS)
class(mypointsdf) # inspect and plot object
names(mypointsdf)
str(mypointsdf)
spplot(mypointsdf, zcol="Name", col.regions = c("red", "blue"),
       xlim = bbox(mypointsdf)[1, ]+c(-0.01,0.01),
       ylim = bbox(mypointsdf)[2, ]+c(-0.01,0.01),
       scales= list(draw = TRUE))


simple_line <- Line(coords)
lines_obj <- Lines(list(simple_line), "1") # function argument is a list
spatlines <- SpatialLines(list(lines_obj), proj4string=prj_string_WGS)
line_data <- data.frame(Name = "straight line", row.names="1")
mylinesdf <- SpatialLinesDataFrame(spatlines, line_data)
class(mylinesdf)
str(mylinesdf)
spplot(mylinesdf, col.regions = "blue",
       xlim = bbox(mypointsdf)[1, ]+c(-0.01,0.01),
       ylim = bbox(mypointsdf)[2, ]+c(-0.01,0.01),
       scales= list(draw = TRUE))

# write to kml ; below we assume a subdirectory GRS-33306\data on
# drive D. Create it if it does not exist yet or change name!
writeOGR(mypointsdf, "data/mypointsGE.kml",
         "mypointsGE", driver="KML", overwrite_layer=TRUE)
writeOGR(mylinesdf, "data/mylinesGE.kml",
         "mylinesGE", driver="KML", overwrite_layer=TRUE)

# route from Google Earth
myroute <- readOGR("data/route.kml", "route.kml")

# put both lines in single data frame
myroute@proj4string <- prj_string_WGS
names(myroute)
myroute$Description <- NULL # delete Description
mylinesdf <- rbind(mylinesdf, myroute)

# define CRS object for RD projection
prj_string_RD <- CRS("+proj=sterea +lat_0=52.15616055555555
+lon_0=5.38763888888889
+k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel
+towgs84=565.2369,50.0087,465.658,
-0.406857330322398,0.350732676542563,-1.8703473836068,4.0812 +units=m
+no_defs")
# perform the coordinate transformation from WGS84 to RD
mylinesRD <- spTransform(mylinesdf, prj_string_RD)

mylinesdf$length <- gLength(mylinesRD, byid=T)
mylinesdf@data # inspect the contents of the data slot

# make circles around points, with radius equal to distance between
# points
mypointsRD <- spTransform(mypointsdf, prj_string_RD)
pnt1_rd <- coordinates(mypointsRD)[1,]
pnt2_rd <- coordinates(mypointsRD)[2,]
# define a series of angles
ang <- pi*0:200/100
circle1x <- pnt1_rd[1] + cos(ang) * mylinesdf$length[1]
circle1y <- pnt1_rd[2] + sin(ang) * mylinesdf$length[1]
circle2x <- pnt2_rd[1] + cos(ang) * mylinesdf$length[1]
circle2y <- pnt2_rd[2] + sin(ang) * mylinesdf$length[1]
# Go through some steps to create SpatialPolygonsDataFrame object
circle1 <- Polygons(list(Polygon(cbind(circle1x, circle1y))),"1")
circle2 <- Polygons(list(Polygon(cbind(circle2x, circle2y))),"2")
spcircles <- SpatialPolygons(list(circle1, circle2),
                             proj4string=prj_string_RD)
circledat <- data.frame(mypointsRD@data, row.names=c("1", "2"))
circlesdf <- SpatialPolygonsDataFrame(spcircles, circledat)

buffpoint <- gBuffer(mypointsRD[1,], width=mylinesdf$length[1],
                     quadsegs=25)
mydiff <- gDifference(circlesdf[1,], buffpoint)
gArea(mydiff)
myintersection <- gIntersection(circlesdf[1,], buffpoint)
gArea(myintersection)
print(paste("The difference in area =", round(100 * gArea(mydiff) /
                                                gArea(myintersection),3), "%"))

MySymDifference <- gSymdifference(circlesdf[1,], circlesdf[2,])
MySymDifferencedf <- SpatialPolygonsDataFrame(MySymDifference,
                                              data.frame(id=1), F)
spplot(MySymDifferencedf, col.regions="gray60", colorkey=FALSE,
       sp.layout=list(list("sp.points", mypointsRD, col="red", pch=19,
                           cex=1.5), list("sp.lines", mylinesRD, lwd=1.5)),
       main="Symmetric difference, etc.")