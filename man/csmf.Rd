\name{csmf}
\alias{csmf}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
  convolution smoothing with Fortran
}

\description{
  %%  ~~ A concise (1-5 lines) description of what the function does. ~~
}

\usage{
  csmf(data, ker)
}
%- maybe also 'usage' for other objects documented here.

\arguments{
  \item{data}{
    %%     ~~Describe \code{data} here~~
  }
  \item{ker}{
    %%     ~~Describe \code{ker} here~~
  }
}

\details{
  %%  ~~ If necessary, more details than the description above ~~
}

\value{
  %%  ~Describe the value returned
  %%  If it is a LIST, use
  %%  \item{comp1 }{Description of 'comp1'}
  %%  \item{comp2 }{Description of 'comp2'}
  %% ...
}

\references{
  %% ~put references to the literature/web site here ~
}

\author{
  Jani Anttila
}

\note{
  %%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  %% ~~objects to See Also as \code{\link{help}}, ~~~
}

\examples{
  ##---- Should be DIRECTLY executable !! ----
  ##-- ==>  Define data, use random,
  ##--	or do  help(data=index)  for the standard data sets.
  data <- matrix(rnorm(25,0,1),5,5)
  kk <- matrix(c(0.0,0.2,0.0,0.2,0.4,0.2,0.0,0.2,0.0),3,3)
  sdata <- csmf(data,kk)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
