\name{cercle}
\alias{cercle}

\title{Circle plot}
\description{
  Plots a circle in 2D from its center coordinates and its radius.
}
\usage{
cercle(cx, cy, r, pas)
}

\arguments{
  \item{cx}{X-coordinate of the center}
  \item{cy}{Y-coordinate of the center}
  \item{r}{Radius}
  \item{pas}{Makes possible to choose the precision of the circle plot. $\frac{2 \pi}{pas}$ is the number of points from wich the circle is drawn.}
}
\details{
  This function allows to draw the Kulldorff circular zone.
}
\value{
  The circle plot.
}
\author{Christophe Dematte{\"\i}  \email{demattei@iurc.montp.inserm.fr}
}
\keyword{spatial}
