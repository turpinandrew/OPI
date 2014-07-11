\name{cdTodb}
\alias{cdTodb}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert cd/\eqn{\mbox{m}^2}{m^2} to dB
}
\description{
Given a value in cd/\eqn{\mbox{m}^2}{m^2}, return the dB equivalent.
The default is HFA dB scale (maxumum stimulus 10000 apostilbs).
}
\usage{
cdTodb(cd, maxStim=10000/pi)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cd}{
Value to convert in cd/\eqn{\mbox{m}^2}{m^2}
}
  \item{maxStim}{
    Stimulus value for 0dB in cd/\eqn{\mbox{m}^2}{m^2}
    }
}
%\details{
%}
\value{
Returns dB value.
}
\references{
Please cite:
A. Turpin, P.H. Artes and A.M. McKendrick
"The Open Perimetry Interface: An enabling tool for clinical visual psychophysics",
Journal of Vision 12(11) 2012.

http://perimetry.org/OPI
}
\author{
Andrew Turpin <aturpin@unimelb.edu.au>
}
%\note{
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{dbTocd}}
}
\examples{
    dB <- cdTodb(10000/pi)  # 0 dB
    dB <- cdTodb(1000/pi)   # 10 dB
    dB <- cdTodb(100/pi)    # 20 dB
    dB <- cdTodb(10/pi)     # 30 dB
    dB <- cdTodb(1/pi)      # 40 dB
    dB <- cdTodb(0.1/pi)    # 50 dB
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ misc }
