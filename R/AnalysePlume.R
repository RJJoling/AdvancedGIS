# Clean workspace
rm(list=ls())

# Load required packages
library(sp)
library(gstat)
library(rgdal)
library(rgeos)

# Enter group number
group <- 5   # provide correct number

# Read the data of all groups
# CLEAR THE CACHE OF YOUR WEB BROWSER (F5 or Ctrl-Shift-R while using the web browser) !!!!
all_obs <- read.table(url("http://scomp5062.wur.nl/courses/grs33306/output/observations.txt"), header=T)
all_obs$datime <- as.POSIXct(all_obs$datime, format='%Y-%m-%d %H:%M:%S')

# Delete accidental adjacent duplicate records with identical co-ordinates and 
# up to 5 seconds temporal difference.
for (i in nrow(all_obs):2)
  if (all_obs$GNo[i]==all_obs$GNo[(i-1)] && all_obs$lon[i]==all_obs$lon[(i-1)] && all_obs$lat[i]==all_obs$lat[(i-1)])
    if (as.numeric(difftime(all_obs$datime[i], all_obs$datime[(i-1)], units="secs")) <= 5)
      all_obs <- all_obs[-i,]

# Set time boundaries & select observations within time frame
end_time <- as.POSIXct("2015-02-26 15:30:00", format='%Y-%m-%d %H:%M:%S')
start_time <- as.POSIXct("2015-02-26 13:00:00", format='%Y-%m-%d %H:%M:%S')
all_obs <- subset(all_obs, datime >= start_time & datime <= end_time)

# Prediction times (8 snapshots, starting 13:15)
pred_times <- start_time + 1:8*as.difftime(15,  units="mins")

# Make spatial & assign World Geodetic System (WGS84) coordinate system
coordinates(all_obs) <- ~lon+lat
all_obs@proj4string <- CRS("+proj=longlat +datum=WGS84")

# Project to Dutch (RD) grid (Note: this is an approximate transformation).
prj_string_RD <- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 
+k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +towgs84=565.2369,50.0087,465.658,
-0.406857330322398,0.350732676542563,-1.8703473836068,4.0812 +units=m +no_defs")
all_rd <- spTransform(all_obs, prj_string_RD)
dimnames(all_rd@coords)[[2]] <- c("x", "y")

# Retrieve extent study area from the server
con <- url("http://scomp5062.wur.nl/courses/grs33306/input/extents.Rdata")
load(con)
close(con)
rm(con)
# Define 2D prediction grid in RD coordinates, later time will be added
RDgrid <- expand.grid(x=seq(RD_minX, RD_maxX, length.out=100), 
                      y=seq(RD_minY, RD_maxY, length.out=100))

# Coerce spatial data to plain data frame
all_rd <- data.frame(all_rd)

# Define map function (CreateSnapshot), also for later use.
# Inverse distance weighting (idw) is used for interpolation
CreateSnapshot <- function(mapdata, predtime, starttime, grd){
  mapdata$t <- as.numeric(difftime(predtime, mapdata$datime, units="mins"))
  grd$t <- rep(as.numeric(difftime(predtime, starttime, units="mins")),10000)
  subdata <- subset(mapdata, t>=0 & t <= 45)
  subdata$t <- subdata$t*5.0  # anisotropy time dimension
  if (nrow(subdata) > 0){
    coordinates(subdata) <- ~x+y+t
    outmap <- idw(ppm~1, subdata, SpatialPoints(grd), idp=2.0, debug.level=0)$var1.pred
  } else{
    outmap <- rep(0, 10000)
  }
  outmap <-  SpatialPixelsDataFrame(SpatialPoints(grd[1:2]), data.frame(ppm=outmap))
  # where/when is the threshold exceeded?
  outmap$danger <- as.factor(outmap$ppm > 100)
  return(outmap)
}

# Create maps, compute misclassification costs.
mycost <- 0
for (i in 1:8){
  mymap <- CreateSnapshot(all_rd, pred_times[i], start_time, RDgrid)
  # Load reference map (timeslice)
  num <- as.numeric(difftime(pred_times[i], start_time, units="mins"))
  con <- url(paste("http://scomp5062.wur.nl/courses/grs33306/input/slice2015_",sprintf("%03d", num),".Rdata",sep=""))
  load(con)
  close(con)
  rm(con)
  # Is threshold exceeded in reference map?
  timeslice$danger <- as.factor(timeslice$plume > 100)
  # compute misclassification cost
  mycost <- mycost + sum(ifelse(mymap$danger == F & timeslice$danger == T, 5, 
                                ifelse(mymap$danger == T & timeslice$danger == F, 1, 0)))
}

# Show a map
i <- 4   # for example
mymap <- CreateSnapshot(all_rd, pred_times[i], start_time, RDgrid)
levels(mymap$danger) <- c("safe","hazard")
if (length(unique(mymap$danger))==1)
  if (mymap$ppm[1] > 100)
    mymap$danger <- factor("hazard", levels=c("safe","hazard"))
spplot(mymap, zcol="danger", col.regions=c("dark green", "red"), 
       main=as.character(pred_times[i]))

# Make two subsets of data: one excluding the data of group and one consisting of the data of group
groupExclude <- all_rd[-which(all_rd$GNo == group),]
groupExclude$GNo <- NULL
groupObserve <- all_rd[which(all_rd$GNo == group),] 


# Random walk procedure; groups cannot jump from one side of the campus to another.
# We need "more realistic" random paths, i.e. distances that an be walked and points 
# that can be visited. We will use a simple random walk procedure: let nobsgroup be
# the number measurements of the group; let perm be a permutation of the vector of 
# time intervals at which the group made measurements; let forbidden be a location
# that cannot be measured, within a pond, building, or dangerous road. (1) choose a
# random start location, (2) make measurement, (3) choose a random direction, (4)
# go to new valid point that can be reached within the time interval from perm, (5) 
# repeat untill nobsgroup measurements are made.

# Read a polygon difining areas that cannot be sampled // Substitute folder name
forbidden <- readOGR("F:/AdvancedGI_GIS/Campus/WaterBuildings.shp", "WaterBuildings")
# Read studyarea
studarea <- readOGR("F:/AdvancedGI_GIS/Campus/true_box.kml", "true")
studarea@proj4string <- forbidden@proj4string
# Project to Dutch grid (RD)
forbidden <- spTransform(forbidden, prj_string_RD)
studarea <- spTransform(studarea, prj_string_RD)

valid <- gDifference(studarea, forbidden)
plot(valid)

# DEFINE RANDOM WALK FUNCTION (will be called later)
RandomWalk <- function(groupdata,validpoly){
  nobsgroup <- nrow(groupdata)
  speed <- 50.0 # [3.0 km/h --> m/min]
  tinterval <- sample(groupdata$t[2:nobsgroup]-groupdata$t[1:(nobsgroup-1)])
  # choose startlocation
  notvalid <- T
  while(notvalid){
    x <- runif(1,validpoly@bbox[1,1],validpoly@bbox[1,2])
    y <- runif(1,validpoly@bbox[2,1],validpoly@bbox[2,2])
    xy <- SpatialPoints(cbind(x,y),proj4string=validpoly@proj4string)
    notvalid <- !gWithin(xy,validpoly)
  }
  locs <- cbind(x,y)
  for(i in 1:(nobsgroup-1)){
    notvalid <- T
    it <- 0
    distance <- speed * tinterval[i]
    while(notvalid){
      direction <- runif(1, 0, 2*pi)
      xnew <- x + distance * cos(direction)
      ynew <- y + distance * sin(direction)
      xy <- SpatialPoints(cbind(xnew,ynew),proj4string=validpoly@proj4string)
      notvalid <- !gWithin(xy,validpoly)
      it <- it + 1
      if (it==500){
        distance <- distance * 0.75 # get out of loop in case of long break
        it <- 0
      }
    }
    x <- xnew
    y <- ynew
    locs <- rbind(locs,cbind(x,y))
  }
  times <- min(groupdata$datime) + as.difftime(cumsum(c(0,tinterval)), unit="mins")
  locs <- SpatialPointsDataFrame(locs, data = data.frame(datime=times),
                                 proj4string=validpoly@proj4string)
  locs <- spTransform(locs, CRS("+proj=longlat +datum=WGS84"))
  locs@proj4string <- CRS(as.character(NA))
  return(locs)
}

# compare group with random walk sampling --- THIS TAKES A FEW MINUTES
groupObserve$t <- as.numeric(difftime(groupObserve$datime, start_time, unit="mins"))
set.seed(187187) # make reproduceable

randomcosts <- numeric(0)
for (i in 1:100){   # 100 realizations of a random walk (maybe you want more or fewer)
  print(paste("Run", i, "of 100"), quote = F)
  path <- RandomWalk(groupObserve, valid)
  minute <- as.integer(difftime(path$datime, start_time, unit="mins")+0.5)
  ppm <- numeric(0)
  for(j in 1:nrow(path)){
    if(minute[j] <= 120){
      con <- url(paste("http://scomp5062.wur.nl/courses/grs33306/input/slice2015_",sprintf("%03d", minute[j]),".Rdata",sep=""))
      load(con)
      close(con)
      tmp_ppm <- as.numeric(over(path[j,],timeslice))  # overlay to find concentration of toxine
      tmp_ppm <- replace(tmp_ppm, is.na(tmp_ppm), 0.0) # rare point on edge of study area
    }
    else tmp_ppm <- max(0,rnorm(1,2.0,1.5)) # random value
    ppm <- c(ppm, tmp_ppm)
  }
  path$ppm <- ppm
  path@proj4string <- CRS("+proj=longlat +datum=WGS84")
  path_rd <- spTransform(path, prj_string_RD)
  path_rd <- as.data.frame(path_rd)
  data_rd <- rbind(path_rd, groupExclude)
  currentcost <- 0
  for (k in 1:6){
    currentmap <- CreateSnapshot(data_rd, pred_times[k], start_time, RDgrid)
    # load reference map (timeslice)
    num <- as.numeric(difftime(pred_times[k], start_time, units="mins"))
    con <- url(paste("http://scomp5062.wur.nl/courses/grs33306/input/slice2015_",sprintf("%03d", num),".Rdata",sep=""))
    load(con)
    close(con)
    # is threshold exceeded?
    timeslice$danger <- as.factor(timeslice$plume > 100)
    # compute cost
    currentcost <- currentcost + sum(ifelse(currentmap$danger == F & timeslice$danger == T, 5, 
                                  ifelse(currentmap$danger == T & timeslice$danger == F, 1, 0)))
  }
  randomcosts <- c(randomcosts,currentcost)
}

# Plot of group performance compared to random behaviour
ext <- range(randomcosts)
ext <- (ext[2]-ext[1])/10.
hist(randomcosts, main="Group vs random path sampling", xlab="Misclassification costs", 
     xlim=c(min(randomcosts,mycost)-ext,max(randomcosts,mycost)+ext))
lines(rbind(c(mycost,0),c(mycost,50)),col="red", lwd=2)

# Plot last random walk (from the sequence of realizations) along with valid polygon
# Valid polygon has to be transformed into spatial dataframe object
validdf <- SpatialPolygonsDataFrame(valid, data.frame(id=1))
line_path_rd <- Line(cbind(path_rd$x, path_rd$y))
spplot(validdf, col.regions="grey70", colorkey=F, 
       sp.layout=list(list("sp.lines", line_path_rd, lty=2, col="red"),
                      list("sp.points", line_path_rd, pch=19, col="red")))
