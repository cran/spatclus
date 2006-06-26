\name{nincdepart}
\alias{nincdepart}
\title{First trajectory point determination in 2D}
\description{
  The choice of the first point of the trajectory is given by the rank of the point in term of distance from the edge of the study area. Generally, the point nearest from the edge is chosen (rank = 1).
}
\usage{
 nincdepart(data, ordre, limx, limy)
}
\arguments{
  \item{data}{Data frame with 2 colums (x and y) giving coordinates of case data points.}
  \item{ordre}{The rank of the first trajectory point in term of distance from the edge.}
  \item{limx}{2 element vector containing the study area bounds of the X-axis.}
  \item{limy}{2 element vector containing the study area bounds of the Y-axis.}
}
\value{
  The data frame "data" with the first trajectory point on the first row.
}
\author{Christophe Dematte{\"\i}  \email{demattei@iurc.montp.inserm.fr}
}
\keyword{spatial}
