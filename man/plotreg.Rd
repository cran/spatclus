\name{plotreg}
\alias{plotreg}
\title{Regression plot}
\description{
  Plots the ordered distance serie and the regression function.
}
\usage{
plotreg(nomlst, m)
}
\arguments{
  \item{nomlst}{A list corresponding to the "clus" function's return value.}
  \item{m}{The number of breaks of the model to be plotted.}
}
\value{
  A R-graphic window containing the plot.
}
\note{
  To plot the regression function with the number of breaks that maximizes the WD max F-statistic, one can use "RES$\$$stat$\$$kmax" as value for the parameter "m" in wich RES is the list returned by the "clus" function.
}
\examples{
  library(spatstat)

  data(chemist)
  data(grille)
  data(irislist)

  # location and detection of spatial clusters
  # adjusted for an inhomogeneous population density
  RES <- clus(chemist,grille,limx=c(-6,8),limy=c(-7,7))

  # plot of the regression
  x11()
  plotreg(RES,RES$stat$kmax)
}
\author{Christophe Dematte{\"\i}  \email{demattei@iurc.montp.inserm.fr}
}

\keyword{spatial}
