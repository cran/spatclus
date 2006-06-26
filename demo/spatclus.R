library(spatstat)

data(chemist)
data(grille)
data(irislist)

# plot of the chemist shop locations
for (i in 1:30){plot(irislist[[i]],xlim=c(-6,8),ylim=c(-7,7),main="Chemist shop locations",lty=3);par(new=TRUE)}
points(chemist$x,chemist$y,pch="+",xlim=c(-6,8),ylim=c(-7,7),asp=1)

# location and detection of spatial clusters of chemist shops adjusted for the inhomogeneous population density
RES <- clus(chemist,grille,limx=c(-6,8),limy=c(-7,7),eps=0.2)


# plot of the trajectory
cat("Select Enter to see the next graph \n")
readline()

for (i in 1:30){plot(irislist[[i]],xlim=c(-6,8),ylim=c(-7,7),main="Trajectory",lty=3);par(new=TRUE)}
points(chemist$x,chemist$y,pch="+",xlim=c(-6,8),ylim=c(-7,7),asp=1)
for (i in 1:(length(RES$res$x)-1)) segments(RES$res$x[i],RES$res$y[i],RES$res$x[i+1],RES$res$y[i+1]) #segment joignant les points (i) et (i+1)
points(RES$res$x[1],RES$res$y[1],pch=7,col=4) #entoure d'un carr�le premier point de la trajectoire

# plot of the regression
cat("Select Enter to see the regression plot \n")
readline()

plotreg(RES,RES$stat$kmax)

# plot of the cluster located
cat("Select Enter to visualize the cluster detected \n")
readline()

for (i in 1:30){plot(irislist[[i]],xlim=c(-6,8),ylim=c(-7,7),main=" ",lty=3);par(new=TRUE)}
plotclus(RES,m=2,limx=c(-6,8),limy=c(-7,7),rcex=11.5,pop=grille)
