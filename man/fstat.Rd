\name{fstat}
\alias{fstat}
\title{Test statistic}
\description{
  Computes the F-statistics for each model with $k$ breaks for $k \in 1,\ldots,m$, and the WD max F-statistic that allows to select the number of breaks and determines if the cluster(s) is significant.
}
\usage{
 fstat(reslst,bc,m,T,eps)
}
\arguments{
  \item{reslst}{A matrix giving, for each point ordered by its rank in the trajectory, its distance to the nearest neighbourg, the expentancy of this distance, and its weighted distance.}
  \item{bc}{A list. The element $k$ of the list is a vector containing the estimated breaks for the $k$-breaks model.}
  \item{m}{The maximum number of breaks.}
  \item{T}{The size of the weighted serie.}
  \item{eps}{Minimum size of cluster (ratio of the total number of cases).}
}
\value{
  A list of objects :

  \item{F}{A F-statistics matrix giving in the first row the F-statistic value for each of the $m$ models, and the corresponding weighted F-statistics in the second row.}

  \item{wdm}{The WD max F-statistic value.}

  \item{wdms}{The WD max F-statistic threshold at $5\%$.}

  \item{signif}{A boolean. If True, the cluster(s) located is significant.}

  \item{kmax}{The number of breaks of the model maximizing the WD max F-statistic.}
}
\details{
  The theory used in this function is based on works of J. Bai and P. Perron.
}
\seealso{
  \code{\link{supf}}
  \code{\link{critval}}
  \code{\link{critvalwdm}}
}
\references{
Bai J. and Perron P. (1998), Estimating and testing linear models with multiple structural changes. \emph{Econometrica}, \bold{66}, 47--78.

Bai J. and Perron P. (2003), Critical values for multiple structural change tests. \emph{Econometrics Journal}, \bold{6}, 72--78.
}
\author{Christophe Dematte{\"\i}  \email{demattei@iurc.montp.inserm.fr}
}
\keyword{spatial}
