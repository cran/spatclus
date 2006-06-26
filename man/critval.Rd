\name{critval}
\alias{critval}

\title{Threshold sup F-statistic computation}
\description{
  Computes the sup F-statistic threshold.
}
\usage{
critval(q = 1, k = 2, e = eps)
}

\arguments{
  \item{q}{An unused argument fixed to 1.}
  \item{k}{The number of breaks.}
  \item{e}{The $\epsilon$ parameter value.}
}
\value{
  The threshold value.
}
\references{
Bai J. and Perron P. (2003), Critical values for multiple structural change tests. \emph{Econometrics Journal}, \bold{6}, 72--78.
}
\author{Christophe Dematte{\"\i}  \email{demattei@iurc.montp.inserm.fr}
}

\keyword{spatial}
