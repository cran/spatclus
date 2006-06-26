\name{espdist3d}
\alias{espdist3d}
\title{Distance weighting in 3D}
\description{
  This function is the 3D version of the espdist function which is described hereafter.
}
\usage{
 espdist3d(data, pop)
}
\arguments{
  \item{data}{Data frame with 3 colums (x, y and z) giving coordinates of case data points. The first row must be the first trajectory point.}
  \item{pop}{Matrix with 4 columns giving coordinates of underlying population data points in the first 3 colums. Last is a column of 1's. This grid must include the cases of the "data" object.}
}
\value{
  A list of objects :

  \item{res}{A result matrix giving, for each point ordered by its rank in the trajectory, its distance to the nearest neighbourg, the expentancy of this distance, and its weighted distance. The last point of the trajectory is not included in the res matrix.}

  \item{derpoint}{A vector containing the last trajectory point coordinates.}

}
\details{
}
\seealso{
  \code{\link{dist2p3d}}
  \code{\link{integre3d}}
  \code{\link{delai}}
}
\references{}
\author{Christophe Dematte{\"\i}  \email{demattei@iurc.montp.inserm.fr}
}
\keyword{spatial}
