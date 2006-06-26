\name{datainc}
\alias{datainc}
\title{Inclusion of cases in the grid}
\description{
   If this is not yet done, the cases are included in the point grid.
}
\usage{
 datainc(data, pop)
}
\arguments{
  \item{data}{Data frame with 2 columns (x and y) or 3 colums (x, y and z) giving coordinates of case data points.}
  \item{pop}{Matrix with 3 or 4 columns giving coordinates of underlying population data points in the first 2 or 3 colums. Last is a column of 1's.}
}
\value{
  The underlying population coordinate matrix including cases.
}
\author{Christophe Dematte{\"\i}  \email{demattei@iurc.montp.inserm.fr}
}
\keyword{spatial}

