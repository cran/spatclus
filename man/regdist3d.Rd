\name{regdist3d}
\alias{regdist3d}
\title{Distance weighting and breaks determination in 3D}
\description{
  This function is the 3D version of the regdist function which is described hereafter.
}
\usage{
 regdist3d(data, pop, start, m, h, T, limx, limy, limz)
}
\arguments{
  \item{data}{Data frame with 3 colums (x, y and z) giving coordinates of case data points.}
  \item{pop}{Matrix with 4 columns giving coordinates of underlying populations individuals in the first 3 colums. Last is a column of 1's.}
  \item{start}{Indicates the rank of the first trajectory point in term of distance from the area edges. 1 means that the first point of the trajectory is the nearest from the edge.}
  \item{m}{The maximal number of breaks.}
  \item{h}{The minimal cluster size.}
  \item{T}{The size of the weighted distance serie.}
  \item{limx}{2 element vector containing the study area bounds of the X-axis.}
  \item{limy}{2 element vector containing the study area bounds of the Y-axis.}
  \item{limz}{2 element vector containing the study area bounds of the Z-axis.}
}
\value{
  A list of objects :

  \item{res}{A result matrix giving, for each point ordered by its rank in the trajectory, its distance to the nearest neighbourg, the expentancy of this distance, and its weighted distance.}

  \item{bc}{A list of vectors. The $k^{th}$ element of the list gives the estimated breaks for the model with $k$ breaks.}

}
\seealso{
  \code{\link{nincdepart3d}}
  \code{\link{espdist3d}}
  \code{\link{multbreak}}
  \code{\link{regdist}}
}
\author{Christophe Dematte{\"\i}  \email{demattei@iurc.montp.inserm.fr}
}
\keyword{spatial}
