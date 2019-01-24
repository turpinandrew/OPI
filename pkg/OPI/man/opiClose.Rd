\name{opiClose}
\alias{opiClose}
\title{
Close using OPI.
}
\description{
Generic function for closing the chosen OPI implementation that is set with
\code{chooseOpi()}.
}
\usage{
opiClose(...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{\dots}{Implementation specific parameters. See details.}
}
%\details{
%}

\value{
Returns NULL if close succeeded, 
otherwise an implementation dependant error.

\subsection{Compass}{
    Returns a list of \code{err}, which is an error code, and \code{fixations}, which is a matrix with three columns:
time, x, y, and one row per fixation. 
}
}
\references{
Please cite:
A. Turpin, P.H. Artes and A.M. McKendrick
"The Open Perimetry Interface: An enabling tool for clinical visual psychophysics",
Journal of Vision 12(11) 2012.

http://perimetry.org/OPI
}
\author{
Andrew Tuprin <aturpin@unimelb.edu.au>
}

\seealso{
\code{\link{chooseOpi}}
}

\examples{
chooseOpi("SimGaussian")
if (!is.null(opiInitialize(sd=2)))
    stop("opiInitialize failed")
if (!is.null(opiClose()))
    stop("opiClose failed, which is very surprising!")
}

\keyword{ misc }
