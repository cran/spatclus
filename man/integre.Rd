\name{integre}
\alias{integre}

\title{Distance expectation computation in 2D}
\description{
  Computes the expectation of the distance from a point to its nearest neighbourg.
}
\usage{
integre(pop, x, y, rmax, n, k, pas)
}

\arguments{
  \item{pop}{The grid with "1" if the point has not been yet included in the trajectory, "0" else.}
  \item{x}{X-coordinate of the point from wich we want to compute the expectancy of the distance.}
  \item{y}{Y-coordinate of the point from wich we want to compute the expectancy of the distance.}
  \item{rmax}{The maximal distance from the (x,y) point to the study area border.}
  \item{n}{The total number of points (cases).}
  \item{k}{The number of points already selected in the trajectory.}
  \item{pas}{Makes possible to choose the precision of the integral calculation.}
}
\value{
  Value of the computed expectation.
}
\seealso{
  \code{\link{airegrille}}
}
\author{Christophe Dematte{\"\i}  \email{demattei@iurc.montp.inserm.fr}
}
\keyword{spatial}
