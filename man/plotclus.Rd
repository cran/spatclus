\name{plotclus}
\alias{plotclus}
\title{Cluster plot}
\description{
  Plots the data points and a representation of the cluster located.
}
\usage{
plotclus(nomlst, m, limx = c(0, 100), limy = c(0, 100), col1 = 225, 
	rcex = 0.68, pop, k=floor((m+1)/2))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nomlst}{A list corresponding to the "clus" function's return value.}
  \item{m}{The number of breaks of the model to be plotted.}
  \item{limx}{2 element vector containing the study area bounds of the X-axis.}
  \item{limy}{2 element vector containing the study area bounds of the Y-axis.}
  \item{col1}{The color to be used for the cluster representation.}
  \item{rcex}{The size to be used for the disc surrounding the points localized in cluster.}
  \item{pop}{A 2 column matrix containing the underlying population coordinates.}
  \item{k}{The number of clusters to be displayed.}
}
\value{
  A R-graphic window containing the plot.
}
\details{
  A cluster groups together the points between two breaks with a low mean distance. The plotreg function allow to choose the number of clusters.
}
\examples{
  library(spatstat)

  data(chemist)
  data(grille)
  data(irislist)

  # location and detection of spatial clusters
  # adjusted for an inhomogeneous population density
  RES <- clus(chemist,grille,limx=c(-6,8),limy=c(-7,7))

  # plot of the cluster located
  for (i in 1:30){
  plot(irislist[[i]],xlim=c(-6,8),ylim=c(-7,7),main=" ",lty=3)
  par(new=TRUE)
  }
  plotclus(RES,m=2,limx=c(-6,8),limy=c(-7,7),rcex=11.5,pop=grille)
}
\author{Christophe Dematte{\"\i}  \email{demattei@iurc.montp.inserm.fr}
}

\keyword{spatial}
