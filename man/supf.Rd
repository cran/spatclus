\name{supf}
\alias{supf}

\title{F-statistic computation}
\description{
  Computes the F-statistic.
}
\usage{
supf(reslst, bc, k, T)
}

\arguments{
  \item{reslst}{A matrix giving, for each point ordered by its rank in the trajectory, its distance to the nearest neighbourg, the expentancy of this distance, and its weighted distance.}
  \item{bc}{A list. The element $k$ of the list is a vector containing the estimated breaks for the $k$-break model.}
  \item{k}{The number of breaks.}
  \item{T}{The size of the weighted distance serie.}}
\value{
  \item{Fdiff}{The F-statistic value}
}
\author{Christophe Dematte{\"\i}  \email{demattei@iurc.montp.inserm.fr}
}
\keyword{spatial}

