\name{multbreak}
\alias{multbreak}
\title{Multiple breaks determination}
\description{
  Determines the breaks by the resolution of the least square problem in the weighted distance regression on the selection order.
}
\usage{
 multbreak(res,m,h,T)
}
\arguments{
  \item{res}{a matrix giving, for each point ordered by its rank in the trajectory, its distance to the nearest neighbourg, the expentancy of this distance, and its weighted distance. The last point of the trajectory is not included in the res matrix.}
  \item{m}{The number of breaks.}
  \item{h}{The minimal cluster size.}
  \item{T}{The size of the weighted distance serie.}
}
\value{
  The m breaks vector.
}
\details{
  This function programming and the underlying method are based on works of J. Bai and P. Perron.
}
\references{
Bai J. and Perron P. (1998), Estimating and testing linear models with multiple structural changes. \emph{Econometrica}, \bold{66}, 47--78.

Bai J. and Perron P. (2003), Critical values for multiple structural change tests. \emph{Econometrics Journal}, \bold{6}, 72--78.

Bai J. and Perron P. (2003), Computation and analysis of multiple structural change models. \emph{Journal of Applied Econometrics}, \bold{18}, 1--22.
}
\author{Christophe Dematte{\"\i}  \email{demattei@iurc.montp.inserm.fr}
}
\keyword{spatial}
