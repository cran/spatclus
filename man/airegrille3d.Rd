\name{airegrille3d}
\alias{airegrille3d}

\title{Volume computation in 3D}
\description{
  This function is the 3D version of the airegrille function which is described hereafter.
}
\usage{
airegrille3d(pop, x, y, z, r)
}

\arguments{
  \item{pop}{The underlying population with "1" if point has not yet been included in the trajectory, "0" else.}
  \item{x}{X-coordinate of the center of the sphere.}
  \item{y}{Y-coordinate of the center of the sphere.}
  \item{z}{Z-coordinate of the center of the sphere.}
  \item{r}{Radius of the sphere.}
}
\value{
  The computed volume.
}
\seealso{
  \code{\link{dist2p3d}}
  \code{\link{airegrille}}
}
\author{Christophe Dematte{\"\i}  \email{demattei@iurc.montp.inserm.fr}
}
\keyword{spatial}
