# load libraries
if (!require("sp")) install.packages("sp")
if (!require("rgl")) install.packages("rgl")
if (!require("rgdal")) install.packages("rgdal")
if (!require("rgeos")) install.packages("rgeos")
if (!require("MASS")) install.packages("MASS")
if (!require("spacetime")) install.packages("spacetime")


# Visualise all available data --------------------------------------------

#load dataframe
dem <- readGDAL(fname="data/Bear_Tracks/DEM.tif")

# PLOTTING 2009 DATA MATING SEASON (May 1 - July 15, 2009) OVER DEM
load("data/Bear_Tracks/mating2009.Rdata")
summary(mating2009)
image(dem, col=grey(0:99/99), asp=1)
text(1380000,6897100,"Overlay DEM and bear trajectories", cex=0.7)
bearnames <- sort(unique(mating2009$Name)) # unique bear names
bear_clrs <- rainbow(length(bearnames)) # colour palette
mating2009$LMT_date <- as.POSIXct(mating2009$LMT_date, format='%d-%m-%Y %H:%M:%S')

# Direct plotting using spplot is extremely slow. Instead we will
# plot trajectories of individual bears, one by one (as lines)
for (i in 1:length(bearnames)){
  subs <- subset(mating2009, Name==bearnames[i]) # individual bear
  subs <- subs[order(subs$LMT_date),] # order GPS positions on time
  lines(subs$Locale_E, subs$Locale_N, col=bear_clrs[i])
}

legend("topleft", bearnames[1:29], lty = rep(1,29), col=bear_clrs[1:29], cex=0.6)
legend("topright", bearnames[30:length(bearnames)], lty = rep(1,(length(bearnames)-29)),
       col=bear_clrs[30:length(bearnames)], cex=0.6)


# Calculate average speed -------------------------------------------------

#calculate average speed for Keiko
keiko <- subset(mating2009, Name=="Keiko")
n <- nrow(keiko)
tdif <- difftime(keiko$LMT_date[2:n], keiko$LMT_date[1:(n-1)],units="hours")
tdif <- as.numeric(tdif)
speed <- 0.001 *sqrt((keiko$Locale_N[2:n] - keiko$Locale_N[1:(n-1)])^2 +
                       (keiko$Locale_E[2:n] - keiko$Locale_E[1:(n-1)])^2)/tdif

#average speed
mean(speed)

#row numbers where calculated speed > maximum brown bear speed
which(speed > 48)


# Load August 2007/2008/2009 data and remove outliers ---------------------


rm(list=ls()) # clean-up data we will not use for the moment
# Read August 2007 data
data2007 <- read.table("data/Bear_Tracks/August2007.txt", header = T, sep=",")
data2007$LMT_date <- as.POSIXct(data2007$LMT_date, format='%d-%m-%Y %H:%M:%S')
plot(data2007$Locale_E, data2007$Locale_N, asp=1)

# delete obvious outlier caused by GPS error
data2007 <- subset(data2007, Locale_E > 1400000)
plot(data2007$Locale_E, data2007$Locale_N, asp=1)

# Read August 2008 data
data2008 <- read.table("data/Bear_Tracks/August2008.txt", header = T, sep=",")
data2008$LMT_date <- as.POSIXct(data2008$LMT_date, format='%d-%m-%Y %H:%M:%S')
plot(data2008$Locale_E, data2008$Locale_N, asp=1)

# delete obvious outlier caused by GPS error
data2008 <- subset(data2008, Locale_N < 7000000)
plot(data2008$Locale_E, data2008$Locale_N, asp=1)

# Read August 2009 data
data2009 <- read.table("data/Bear_Tracks/August2009.txt", header = T, sep=",")
data2009$LMT_date <- as.POSIXct(data2009$LMT_date, format='%d-%m-%Y %H:%M:%S')
plot(data2009$Locale_E, data2009$Locale_N, asp=1)


# Plot bear locations for 2008 --------------------------------------------

# 2D Plot of locations 2008 per bear
plot(data2008$Locale_E, data2008$Locale_N, asp=1, col=c("red", "green", "blue", "purple",
           "black")[data2008$PubName], main = "Locations five bears, August 2008")
legend("topright", levels(data2008$PubName), pch = rep(1,5),
       col=c("red", "green", "blue", "purple", "black"))

# SUBSETS and ordering
Koski2008 <- subset(data2008, PubName == "Koski (5570)")
Grivla2008 <- subset(data2008, PubName == "Grivla (2911)")
Koski2008 <-Koski2008[order(Koski2008$LMT_date),] # order on data_time
Grivla2008 <- Grivla2008[order(Grivla2008$LMT_date),]

# 2D Plot trajectories
plot(Koski2008$Locale_E, Koski2008$Locale_N, type="l", main = "Track Koski, August 2008") # 2D single bear


# Create 3d view of Koski and Grivla SpaceTime Cube -----------------------

#set start date/time
tstart <- min(min(Koski2008$LMT_date),min(Grivla2008$LMT_date))
Koski2008$tspan <- difftime(Koski2008$LMT_date, tstart, units="hours")
Grivla2008$tspan <- difftime(Grivla2008$LMT_date, tstart, units="hours")

#set scales for 3d cube
xscale <- c(min(Grivla2008$Locale_E,Koski2008$Locale_E), max(Grivla2008$Locale_E,Koski2008$Locale_E))
yscale <- c(min(Grivla2008$Locale_N,Koski2008$Locale_N), max(Grivla2008$Locale_N,Koski2008$Locale_N))
zscale <- c(0,max(Grivla2008$tspan,Koski2008$tspan))

#create new window, plot 3d cube
open3d(windowRect=c(100,100,700,700))
plot3d(Koski2008$Locale_E, Koski2008$Locale_N, Koski2008$tspan, type="l", col="red", lwd=1.5,
       xlab="East", ylab="North", zlab="time", xlim=xscale, ylim=yscale,zlim=zscale)
lines3d(Grivla2008$Locale_E, Grivla2008$Locale_N, Grivla2008$tspan, col="blue", lwd= 1.5)


# EXTRA - Preferred locations ---------------------------------------------

density <- kde2d(Koski2008$Locale_E, Koski2008$Locale_N)
png(filename = "output/Track and density map Koski, August 2008.png",  width = 800, height = 600, units = "px")
image(density, main = "Track and density map Koski, August 2008")
lines(Koski2008$Locale_E, Koski2008$Locale_N)
dev.off()


# 2D intersection of trajectories -----------------------------------------

# using spatial objects and methods from sp & rgeos libraries)
lGrivla <- Lines(Line(cbind(Grivla2008$Locale_E, Grivla2008$Locale_N)),"1")
lGrivla <- SpatialLines(list(lGrivla), proj4string = CRS("+init=epsg:2400"))
lGrivla <- SpatialLinesDataFrame(lGrivla, data=data.frame(ID="1",name="Grivla"), match.ID = T)
lKoski <- Lines(Line(cbind(Koski2008$Locale_E, Koski2008$Locale_N)),"1")
lKoski <- SpatialLines(list(lKoski), proj4string = CRS("+init=epsg:2400"))
lKoski <- SpatialLinesDataFrame(lKoski, data=data.frame(ID="1",name="Koski"), match.ID = T)

# spatial intersection, no time yet
scoin <- gIntersection(lGrivla, lKoski)
spplot(lGrivla, zcol="name",col.regions="blue", lwd=1, colorkey=FALSE,
       xlim=xscale, ylim=yscale, sp.layout=list(list("sp.lines", lKoski,
       col="red"), list("sp.points", scoin, pch=19, col="black")))


# Temporal coincidence moments --------------------------------------------

source("R/temporal_coincidence.R")

#create time dataframe
tdataframe <- data.frame(tGriv=numeric(nrow(scoin@coords)), tKosk=numeric(nrow(scoin@coords)))

#fill dataframe with values
for (j in 1:nrow(scoin@coords)){
  for (ind in 2:nrow(Koski2008))
    if (OnLineSegment(scoin@coords[j,1],scoin@coords[j,2],
                      Koski2008$Locale_E[ind-1], Koski2008$Locale_N[ind-1],
                      Koski2008$Locale_E[ind], Koski2008$Locale_N[ind])){
      tdataframe$tKosk[j] <- FindTime(scoin@coords[j,1], scoin@coords[j,2],ind,Koski2008)
      break}
  for (ind in 2:nrow(Grivla2008))
    if (OnLineSegment(scoin@coords[j,1],scoin@coords[j,2],
                      Grivla2008$Locale_E[ind-1],Grivla2008$Locale_N[ind-1],
                      Grivla2008$Locale_E[ind], Grivla2008$Locale_N[ind])){
      tdataframe$tGriv[j] <- FindTime(scoin@coords[j,1], scoin@coords[j,2],ind,Grivla2008)
      break}
}

#create spatialpoints
delta_t <- 8.0 #set threshold
stClose <- scoin[which(abs(tdataframe$tGriv - tdataframe$tKosk) < delta_t),]
stTimes<-apply(tdataframe[which(abs(tdataframe$tGriv-tdataframe$tKosk) < delta_t),],1,mean)

#create 3d plot
open3d(windowRect=c(100,100,700,700))
plot3d(Koski2008$Locale_E, Koski2008$Locale_N, Koski2008$tspan,
       type="l", col="red", lwd=1.5, xlab="East", ylab="North",
       zlab="time", xlim=xscale, ylim=yscale,zlim=zscale)
lines3d(Grivla2008$Locale_E, Grivla2008$Locale_N, Grivla2008$tspan, col="blue", lwd= 1.5)
points3d(stClose@coords[,1], stClose@coords[,2], stTimes, col="green", size = 10)


# Spatial coincidence moments ---------------------------------------------

source("R/spatial_coincidence.R")

delta_s <- 25.0 #set threshold

#create buffer for Grivla
near_1 <- numeric()
for (ii in 1: nrow(Grivla2008)){
  t1 <- SpatLagOvl(Grivla2008$Locale_E[ii], Grivla2008$Locale_N[ii], lKoski, Koski2008, delta_s)
  if(t1!= F)
    near_1 <- rbind(near_1,c(Grivla2008$Locale_E[ii], Grivla2008$Locale_N[ii],Grivla2008$tspan[ii],t1))}

#create buffer for Koski
near_2 <- numeric()
for (ii in 1: nrow(Koski2008)){
  t2 <- SpatLagOvl(Koski2008$Locale_E[ii], Koski2008$Locale_N[ii], lGrivla, Grivla2008, delta_s)
  if(t2 != F)
    near_2 <- rbind(near_2,c(Koski2008$Locale_E[ii], Koski2008$Locale_N[ii],Koski2008$tspan[ii],t2))}

#define attribute names
colnames(near_1) <- c("x","y","tGriv","tKosk")
colnames(near_2) <- c("x","y","tKosk","tGriv")

#create dataframe
near_1 <- data.frame(near_1)
near_2 <- data.frame(near_2)

#subset above threshold
stnear1 <- subset(near_1, abs(tGriv -tKosk) < delta_t)
stnear2 <- subset(near_2, abs(tGriv -tKosk) < delta_t)

#create 3d plot
open3d(windowRect=c(100,100,700,700))
plot3d(Koski2008$Locale_E, Koski2008$Locale_N, Koski2008$tspan,
       type="l", col="red", lwd=1.5, xlab="East", ylab="North",
       zlab="time", xlim=xscale, ylim=yscale,zlim=zscale)
lines3d(Grivla2008$Locale_E, Grivla2008$Locale_N, Grivla2008$tspan,
        col="blue", lwd= 1.5)
points3d(stnear1[,1],stnear1[,2],stnear1[,3],col="green", size = 10)
points3d(stnear2[,1],stnear2[,2],stnear2[,3],col="yellow", size = 10)


# EXTRA 2 - Analysis of bear behaviour ------------------------------------

'not gonna happen'


# Plot GPS track in Google Earth ------------------------------------------

# unproject to WGS84; data assumed to be in Swedish grid RT90
# documentation available at: http://spatialreference.org/
prj_string_RT90 <- CRS("+init=epsg:2400")
prj_string_WGS <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
kos2d <- SpatialPointsDataFrame(cbind(Koski2008$Locale_E,
         Koski2008$Locale_N), data.frame(time=Koski2008$LMT_date))
kos2d@proj4string <- prj_string_RT90
koswgs84 <- spTransform(kos2d, prj_string_WGS)
writeOGR(koswgs84, "data/koskipoints2008.kml", "Koski", driver="KML", overwrite_layer=T)


# Infection plot ----------------------------------------------------------

# read centre point KML
sourceWGS <- readOGR("data/infection_centre.kml", "infection_centre.kml")
sourceRT90 <- spTransform(sourceWGS, prj_string_RT90)
sourceRT90 <- gBuffer(sourceRT90, width=2500)
plot(sourceRT90, xlim=bbox(kos2d)[1,], ylim=bbox(kos2d)[2,], col="yellow", lty=0)
points(kos2d)

# add time span of infection and make it a spacetime object
sourceSTI <- STI(sourceRT90, as.POSIXct("2008-08-10 00:00:00"), as.POSIXct("2008-08-15 23:59:59"))

# also make spacetime object for recorded locations Koski
kos <- STI(SpatialPoints(cbind(Koski2008$Locale_E, Koski2008$Locale_N)), Koski2008$LMT_date)

# find locations that were recorded during the time span of the infection
kos@sp@proj4string <- prj_string_RT90
infectKos <- over(kos, sourceSTI)
infectKos <- which(!is.na(infectKos))

# check
kos2d$time[infectKos] # Explain the response!

# plot
points(kos2d[infectKos,], pch=19, cex=0.5, col="red") # Explain!


# Overlay in SpaceTime ----------------------------------------------------

# information about method over
showMethods(over)
?sp::over

# make spacetime object for recorded locations Grivla
gri <- STI(SpatialPoints(cbind(Grivla2008$Locale_E, Grivla2008$Locale_N)), Grivla2008$LMT_date)
gri@sp@proj4string <- prj_string_RT90

# plot
stplot(kos, type = "l", number=12)
stplot(gri, type = "l", number=12)

# do points overlap?
test <- over(kos,gri)
summary(test) #no... (due to significant numbers for space and time)
