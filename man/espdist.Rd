\name{espdist}
\alias{espdist}
\title{Distance weighting in 2D}
\description{
  The distances to the nearest neighbourg are weighted by the expectancy of this distance in the uniform case. This allows first to avoid distance to be higher for the latest selected points, due to the elimination process of pre-selected points, and second to adjust for inhomogeneity in the underlying population density.
}
\usage{
 espdist(data, pop)
}
\arguments{
  \item{data}{Data frame with 2 colums (x and y) giving coordinates of case data points. The first row must be the first trajectory point.}
  \item{pop}{Matrix with 3 columns giving coordinates of underlying population data points in the first 2 colums. Last is a column of 1's. This grid must include the cases of the "data" object.}
}
\value{
  A list of objects :

  \item{res}{A result matrix giving, for each point ordered by its rank in the trajectory, its distance to the nearest neighbourg, the expentancy of this distance, and its weighted distance. The last point of the trajectory is not included in the res matrix.}

  \item{derpoint}{A vector containing the last trajectory point coordinates.}

}
\details{
 The theory used in this function is based on works of Bickel P. and Breiman L.
}
\seealso{
  \code{\link{dist2p}}
  \code{\link{integre}}
  \code{\link{delai}}
}
\references{
Bickel P. and Breiman L. (1983), Sum of functions of nearest neighbourg distances, moment bounds, limit theorems and a goodness of fit test.  \emph{Annals of Probability}, \bold{11}, 185--214.
}
\author{Christophe Dematte{\"\i}  \email{demattei@iurc.montp.inserm.fr}
}
\keyword{spatial}
