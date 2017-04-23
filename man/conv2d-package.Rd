\name{2d convolution}
\alias{conv2d}
\docType{package}

\title{
  2D convolution
}

\description{
  More about what it does (maybe more than one line)
  ~~ A concise (1-5 lines) description of the package ~~
}

\details{
  \tabular{ll}{
    Package: \tab conv2d\cr
    Type: \tab Package\cr
    Version: \tab 2.0.1\cr
    Date: \tab 2017-04-18\cr
    License: \tab wtfpl\cr
  }
  ~~ An overview of how to use the package, including the most important ~~
  ~~ functions ~~
}

\author{
  Jani Anttila

  Maintainer: Jani Anttila <jani.v.anttil@gmail.com>
}

\references{
  ~~ Literature or other references for background information ~~
}

\keyword{ package }

\seealso{
  ~~ Optional links to other man pages, e.g. ~~
  ~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
}

\examples{
  data <- matrix(rnorm(50*50),50,50)
  kk <- matrix(c(0.0,0.2,0.0,0.2,0.4,0.2,0.0,0.2,0.0),3,3)
  sdata <- csm(data,kk)
}
