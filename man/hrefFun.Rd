\name{hrefFun}
\alias{hrefFun}
\title{Get estate}
\usage{
  hrefFun(x)
}
\arguments{
  \item{type}{choose to scrape offers for rent or sale.}

  \item{pages}{select the number of pages, each of them
  containing 40 items.}
}
\description{
  Get real estate data
}
\details{
  This function scrapes data from pap.fr using the
  \code{\link{XML}} and the \code{\link{stringr}} package.
}
\examples{
data <- getEstate(type = "vente", pages = 50)
}
\author{
  Bo Werth
}
\seealso{
  \code{\link{XML}}
}
\keyword{scrape}

