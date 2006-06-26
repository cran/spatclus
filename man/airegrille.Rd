\name{airegrille}
\alias{airegrille}

\title{Surface computation in 2D}
\description{
  Computes the surface of the area with "1" on the grid and out of the circle with (x,y) as center coordinates and r as ray.
}
\usage{
airegrille(pop, x, y, r)
}

\arguments{
  \item{pop}{The underlying population with "1" if point has not yet been included in the trajectory, "0" else.}
  \item{x}{X-coordinate of the center of the circle.}
  \item{y}{Y-coordinate of the center of the circle.}
  \item{r}{Radius of the circle.}
}
\value{
  The computed surface.
}
\seealso{
  \code{\link{dist2p}}
}
\author{Christophe Dematte{\"\i}  \email{demattei@iurc.montp.inserm.fr}
}
\keyword{spatial}
