# load libraries
if (!require("rgl")) install.packages("rgl")

# load data
load("data/AlibiQuery.Rdata")

# define variables
t1=0.0; x1=-1.0; y1=-1.0; t2=1.0; x2=-1.5; y2=-1.0; v1=4.0; t3=0.0; x3=1.0; y3=1.0; t4=2.0; x4=1.0; y4=1.0; v2=2.26

# create spacetime grid
xyt <- expand.grid(x=seq(-5,5,length=100), y=seq(-5,5,length=100),
                   t=(seq(min(t1,t3), max(t2,t4),length=100)))

# select two beads
bead1 <- subset(xyt,(((xyt$x - x1)^2 + (xyt$y - y1)^2 <=
         v1^2*(xyt$t - t1)^2) & ((xyt$x - x2)^2 + (xyt$y - y2)^2 <=
         v1^2*(xyt$t - t2)^2) & (t1 <= xyt$t & xyt$t <= t2)))
bead2 <- subset(xyt,(((xyt$x - x3)^2 + (xyt$y - y3)^2 <=
         v2^2*(xyt$t - t3)^2) & ((xyt$x - x4)^2 + (xyt$y - y4)^2 <=
         v2^2*(xyt$t - t4)^2) & (t3 <= xyt$t & xyt$t <= t4)))

# create 3D plot
open3d(windowRect=c(50,50,600,600))
plot3d(bead1,type="p",col="red",xlim=c(-5,5),ylim=c(-5,5), zlim=c(min(t1,t3), max(t2,t4)))
points3d(bead2, col="blue")

# check if beads touch each other
alibi(t1, x1, y1, t2, x2, y2, v1, t3, x3, y3, t4, x4, y4,v2) # returns TRUE
