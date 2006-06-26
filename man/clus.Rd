\name{clus}
\alias{clus}
\title{Cluster location and detection}
\description{
  Locates and detects multiple spatial clusters in 2D and 3D and determines the Kulldorff's circular zone in 2D (without detection).
}
\usage{
 clus(data, pop, dataincyn = "n", rndm = NaN, m = 9, eps = 0.1, limx, limy, 
    limz, method = 1, methk = 3, start = 1, export = "n", repexport)
}
\arguments{
  \item{data}{Data frame with 2 or 3 colums (x and y, and z in 3D) giving coordinates of case data points.}
  \item{pop}{Matrix with 2 or 3 columns (depending on wether 2D or 3D data) giving coordinates of underlying population data points.}
  \item{dataincyn}{"y" means that cases are included in the grid, "n" that they are not.}
  \item{rndm}{Vector giving the number of the rows containing cases coordinates in the grid (only if datainc="y").}
  \item{m}{Maximum number of breaks.}
  \item{eps}{Minimum size of cluster (ratio of the total number of cases).}
  \item{limx}{2 element vector containing the study area bounds of the X-axis.}
  \item{limy}{2 element vector containing the study area bounds of the Y-axis.}
  \item{limz}{In 3D, 2 element vector containing the study area bounds of the Z-axis.}
  \item{method}{1 for multiple break clusters, 2 for Kulldorff localization, 3 for the 2 methods.}
  \item{methk}{In the Kulldorff localization, 1 for Bernoulli model, 2 for Poisson model.}
  \item{start}{Indicates the rank of the first trajectory point in term of distance from the area edges. 1 means that the first point of the trajectory is the nearest from the edge.}
  \item{export}{If method = 2 or method = 3, and if export = "y", the data will be exported in "repexport" directory in SatScan software format.}
  \item{repexport}{If export = "y", defines the directory in which data in SatScan software format will be exported.}
}
\value{
  A list of objects :

  \item{res}{A result matrix giving, for each point ordered by its rank in the trajectory, its distance to the nearest neighbourg, the expentancy of this distance, and its weighted distance.}

  \item{pop}{The matrix with 2 or 3 columns giving coordinates of underlying population data points without cases.}

  \item{bc}{A list of vectors. The kth element of the list gives the estimated breaks for the model with k breaks.}

  \item{stat}{A list of non corrected statistic values (F), corrected statistic value (wdm), threshold value for the WDM statistic (wdms) and significativity (signif).}

  \item{kulld.p}{A vector giving the results of the Kulldorff method with the Poisson model. lambda is the value of the spatial scan test statistic, loglambda is its logarithm, cx and cy are the coordinates of the circle center and rayon is its ray.}

  \item{kulld.b}{A vector giving the results of the Kulldorff method with the Bernouilli model. lambda is the value of the spatial scan test statistic, loglambda is its logarithm, cx and cy are the coordinates of the circle center and rayon is its ray.}


}
\details{
  The "clus" function is the main function. It uses all other functions described below, except "plot" functions. Thus, generally, only the clus function is necessary since others are implicitely called. However, they can be usefull for other purposes, such as when one wants to determine the breaks from a serie, not only in the spatial field.
  Its main arguments are "data" (case locations) and "pop" (underlying population locations). The function determines the trajectory giving a selection order to each point, computes the weighting of the distance, determines the potential clusters through the computation of the breaks by a regression of this weighted distance on the selection order, and finally tests the significativity of those potential clusters.
}
\note{
  Only arguments "data", "pop", "limx" and "limy" are essential (and "limz" in 3D) but the others have default values. So do not forget to adapt them at your special case.

}

\seealso{
  \code{\link{datainc}}
  \code{\link{regdist}}
  \code{\link{fstat}}
  \code{\link{kulld}}
  \code{\link{delai}}
}
\examples{
  library(spatstat)

  data(chemist)
  data(grille)
  data(irislist)

  # plot of the chemist shop locations
  for (i in 1:30){
  plot(irislist[[i]],xlim=c(-6,8),ylim=c(-7,7),main="Chemist shop locations",lty=3)
  par(new=TRUE)
  }
  points(chemist$x,chemist$y,pch="+",xlim=c(-6,8),ylim=c(-7,7),asp=1)

  # location and detection of spatial clusters
  # adjusted for an inhomogeneous population density
  RES <- clus(chemist,grille,limx=c(-6,8),limy=c(-7,7))

  # plot of the trajectory
  x11()
  for (i in 1:30){
  plot(irislist[[i]],xlim=c(-6,8),ylim=c(-7,7),main="Trajectory",lty=3)
  par(new=TRUE)
  }
  points(chemist$x,chemist$y,pch="+",xlim=c(-6,8),ylim=c(-7,7),asp=1)
  for (i in 1:(length(RES$res$x)-1)){
  segments(RES$res$x[i],RES$res$y[i],RES$res$x[i+1],RES$res$y[i+1])
  }
  #entoure d'un carré le premier point de la trajectoire
  points(RES$res$x[1],RES$res$y[1],pch=7,col=4)

  # plot of the regression
  x11()
  plotreg(RES,RES$stat$kmax)

  # plot of the cluster located
  x11()
  for (i in 1:30){
  plot(irislist[[i]],xlim=c(-6,8),ylim=c(-7,7),main=" ",lty=3)
  par(new=TRUE)
  }
  plotclus(RES,m=2,limx=c(-6,8),limy=c(-7,7),rcex=11.5,pop=grille)
}
\references{
Bai J. and Perron P. (1998), Estimating and testing linear models with multiple structural changes. \emph{Econometrica}, \bold{66}, 47--78.

Bai J. and Perron P. (2003), Critical values for multiple structural change tests. \emph{Econometrics Journal}, \bold{6}, 72--78.

Bai J. and Perron P. (2003), Computation and analysis of multiple structural change models. \emph{Journal of Applied Econometrics}, \bold{18}, 1--22.

Bickel P. and Breiman L. (1983), Sum of functions of nearest neighbourg distances, moment bounds, limit theorems and a goodness of fit test.  \emph{Annals of Probability}, \bold{11}, 185--214.

Dematte{\"\i} C., Molinari N. and Daur\`es J.P. (2006), Arbitrarily shaped multiple spatial cluster detection for case event data. \emph{Accepted in Computational Statistics and Data Analysis}. Corrected proof available online via the DOI link \url{http://dx.doi.org/10.1016/j.csda.2006.03.011}.

Kulldorff M. and Nagarwalla N. (1995), Spatial disease clusters : Detection and Inference.  \emph{Statistics in Medicine}, \bold{14}, 799--810.

Kulldorff M. (1997), A spatial scan statistic.  \emph{Communications in Statistics - Theory and Methods}, \bold{26}, 1481--1496.
}
\author{Christophe Dematte\"\i \email{demattei@iurc.montp.inserm.fr}
}
\keyword{spatial}
