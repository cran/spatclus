\name{regdist}
\alias{regdist}
\title{Distance weighting and breaks determination in 2D}
\description{
  Determination of the trajectory, weighting of the distance and determination of the breaks by a regression of the distance on the selection order.
}
\usage{
 regdist(data, pop, start, m, h, T, limx, limy)
}
\arguments{
  \item{data}{Data frame with 2 colums (x and y) giving coordinates of case data points.}
  \item{pop}{Matrix with 3 columns giving coordinates of underlying populations individuals in the first 2 colums. Last is a column of 1's.}
  \item{start}{Indicates the rank of the first trajectory point in term of distance from the area edges. 1 means that the first point of the trajectory is the nearest from the edge.}
  \item{m}{The maximal number of breaks.}
  \item{h}{The minimal cluster size.}
  \item{T}{The size of the weighted distance serie.}
  \item{limx}{2 element vector containing the study area bounds of the X-axis.}
  \item{limy}{2 element vector containing the study area bounds of the Y-axis.}
}
\value{
  A list of objects :

  \item{res}{A result matrix giving, for each point ordered by its rank in the trajectory, its distance to the nearest neighbourg, the expentancy of this distance, and its weighted distance.}

  \item{bc}{A list of vectors. The $k^{th}$ element of the list gives the estimated breaks for the model with $k$ breaks.}

}
\seealso{
  \code{\link{nincdepart}}
  \code{\link{espdist}}
  \code{\link{multbreak}}
}
\author{Christophe Dematte{\"\i}  \email{demattei@iurc.montp.inserm.fr}
}
\keyword{spatial}
