\name{kulld}
\alias{kulld}
\title{Kulldorff circular zone determination}
\description{
  The spatial scan statistic of Kulldorff is computed using the Poisson or Bernoulli model for each point of the grid, and the circular zone maximizing this statistic is located.
}
\usage{
 kulld(pop,rndm,methk,export,repexport)
}
\arguments{
  \item{pop}{Matrix with 3 columns giving coordinates of underlying population data points in the first 2 colums. Last is a column of 1's. This population must include the cases of the "data" object.}
  \item{rndm}{A vector giving the row number of the cases.}
  \item{methk}{If 1, the Bernouilli model is used. If 2, the Poisson model is used. If 3, the two models are successively used.}
  \item{export}{If "y", the data will be exported in "repexport" directory in SatScan software format.}
  \item{repexport}{If export = "y", defines the directory in which data in SatScan software format will be exported.}
}
\value{
  A list of objects :

  \item{\$pois}{A vector giving the results of the Kulldorff method with the Poisson model. lambda is the value of the spatial scan test statistic, loglambda is its logarithm, cx and cy are the coordinates of the circle center and rayon is its ray.}

  \item{\$bern}{A  vector giving the results of the Kulldorff method with the Bernouilli model. lambda is the value of the spatial scan test statistic, loglambda is its logarithm, cx and cy are the coordinates of the circle center and rayon is its ray.}
}
\details{
  The theory used in this function is based on works of M. Kulldorff.
}
\seealso{
  \code{\link{dist2p}}
  \code{\link{delai}}
}
\references{
Kulldorff M. (1997), A spatial scan statistic.  \emph{Communications in Statistics - Theory and Methods}, \bold{26}, 1481--1496.

Kulldorff M. and Nagarwalla N. (1995), Spatial disease clusters : Detection and Inference.  \emph{Statistics in Medicine}, \bold{14}, 799--810.
}
\author{Christophe Dematte{\"\i}  \email{demattei@iurc.montp.inserm.fr}
}
\keyword{spatial}
