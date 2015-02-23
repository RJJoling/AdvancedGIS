source("R/hypso_curve.R")

#Create seperate window
windows(10,5)
par(mfcol=c(1,2),mar=c(4,2,1,1), xaxs="i", yaxs="i")
drawhypso(1)

#Create GUI slider
percentage <- 50 # some initial value
tclvalue(percentage) <- 50
replot()
tt <- tktoplevel()
tkwm.title(tt, "Hypso")
slider <- tkscale(tt, label = "Area percentage",
                  from=0, to=100, showvalue=1, variable=percentage,
                  resolution=1, orient="horiz", relief="groove",length=160)
butt1 <- tkbutton(tt, text="Replot", command=replot, width=10)
butt2 <- tkbutton(tt, text="Quit", command=tt_close, width=10)
tkpack(slider, butt1, butt2)

#Create PNG for each percentage
fnames <- character()
for (i in 0:100){
  fnam=paste("output/perc", sprintf("%03d", i), ".png", sep="")
  png(filename = paste(fnam, sep = ""))
  par(mfcol=c(1,2),mar=c(4,2,1,1), xaxs="i", yaxs="i")
  drawhypso(i)
  dev.off()
  fnames <- c(fnames,fnam)
}

#Create GIF
shell(paste(c("convert", fnames, "output/hypso.gif"), collapse=" "))

unlink(fnames) # delete .png files
