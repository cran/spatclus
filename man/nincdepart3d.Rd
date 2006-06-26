\name{nincdepart3d}
\alias{nincdepart3d}
\title{First trajectory point determination in 3D}
\description{
  This function is the 3D version of the nincdepart function which is described hereafter.
}
\usage{
 nincdepart3d(data, ordre, limx, limy, limz)
}
\arguments{
  \item{data}{Data frame with 3 colums (x, y and z) giving coordinates of case data points.}
  \item{ordre}{The rank of the first trajectory point in term of distance from the edge.}
  \item{limx}{2 element vector containing the study area bounds of the X-axis.}
  \item{limy}{2 element vector containing the study area bounds of the Y-axis.}
  \item{limz}{2 element vector containing the study area bounds of the Z-axis.}
}
\value{
  The data frame "data" with the first trajectory point on the first row.
}
\seealso{
  \code{\link{nincdepart}}
}
\author{Christophe Dematte{\"\i}  \email{demattei@iurc.montp.inserm.fr}
}
\keyword{spatial}
