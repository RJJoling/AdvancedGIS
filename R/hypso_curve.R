# load libraries
if (!require("sp")) install.packages("sp")
if (!require("raster")) install.packages("raster")
if (!require("rgdal")) install.packages("rgdal")
if (!require("tcltk")) install.packages("tcltk")


#Set wd
setwd("M:/Studie/Advanced GIS/R Projects/AdvancedGISExercises/")

#load dataframe
dem <- readGDAL(fname="data/DEM_Preprocessed.tif")
#names(dem)
#summary(dem)
#str(dem)
#hist(dem$band1, breaks=500)

#Erase outliers
dem$band1 <- replace(dem$band1, dem$band1 > 2500, NA) 
#hist(dem$band1, breaks=500) #check changes

#Create rasterlayer
dem <- raster(dem)

#Change resolution (150x150)
dem <- aggregate(dem, fact=5, na.rm=T)
#dem

#Visualise
#hist(dem, breaks=500)
#plot(dem, col=grey(0:99 / 99), axes=F, legend=T, box=F)

#Create hypsometric curve
probset <- 1000:0/1000 # generate a descending set of probabilities
#str(probset)
quants <- quantile(dem, probs=probset,na.rm=T)
#plot(0:1000/10, quants, ty = "l", xlab="Area percentage", ylab="Elevation")

# define palette & breaks
grayset <- grey(1:99 / 100) # palette of grey shades
maxdem <- cellStats(dem, stat='max')

# define a set of breakpoints for assigning grey shades to DEM
breakset <- c(seq(cellStats(dem, stat='min')-1,maxdem,length.out=100))


#Create hypsometric function
drawhypso <- function(percentage){
  percentage <- round(percentage,1)
  percentage <- min(100,percentage)
  percentage <- max(0,percentage)
  invfrac=(100-percentage)/100
  plot(0:1000/10, quants, ty="l",xlab="Area percentage",
       ylab="elevation",
       lwd=2, xlim=c(0,106))
  threshold <- as.numeric(quants[min(which(probset<=invfrac))])
  lines(x=c(percentage,percentage,0,102),
        y=c(min(quants),threshold,threshold,threshold),col="golden rod",
        lty=3, lwd=1)
  locatindx <- which(breakset>threshold)
  if(length(locatindx)==0){
    rect(102, breakset[1:99],108,breakset[100],border=NA,col=grayset)
    plot(dem, col=grayset, breaks=breakset, axes=F, legend=F, box=F)}
  else{
    locatindx <- min(locatindx)-1
    rect(102, breakset[1:99],108,breakset[100],border=NA,
         col=c(grayset[1:locatindx],rep("golden rod",100-locatindx)))
    breaksubset <- c(breakset[1:locatindx],threshold,maxdem+1)
    colorsubset <- c(grayset[1:locatindx],"golden rod")
    plot(dem, col=colorsubset,breaks=breaksubset, axes=T, legend=F,
         box=F)
      }
   }

replot <- function()
  drawhypso(as.numeric(tclvalue(percentage)))
tt_close <- function() {
  tkdestroy(tt)
  dev.off()
}

