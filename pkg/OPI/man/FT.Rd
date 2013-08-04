\name{FT}
\alias{FT}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Full Threshold 
}
\description{
FT begins with a 4-2dB staircase at level \code{est}. If the final estimate 
(last seen) is more than 4dB away from \code{est}, a second 4-2 staircase is
completed beginning at the estimate returned from the first.
}
\usage{
FT(est = 25, instRange = c(0, 40), verbose = FALSE, makeStim, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{est}{
Starting estimate in dB}
  \item{instRange}{
Dynamic range of the instrument c(min,max) in dB
}
  \item{verbose}{
True if you want each presentation printed
}
  \item{makeStim}{
A function that takes a dB value and numPresentations 
and returns an OPI datatype ready for passing to opiPresent
}
  \item{\dots}{
Extra parameters to pass to the opiPresent function
}
}
\details{
    Note this function will repeatedly call \code{opiPresent} for a stimulus 
    until \code{opiPresent} returns \code{NULL} (ie no error occured).
}
\value{
Returns a list containing
\item{npres}{Total number of presentations}
\item{respSeq}{Response sequence stored as a list of (seen,dB) pairs}
\item{first}{First staircase estimate in dB}
\item{final}{Final threshold estimate in dB}
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

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{dbTocd}}, \code{\link{opiPresent}}
}
\examples{
    # Stimulus is Size III white-on-white as in the HFA
makeStim <- function(db, n) { 
    s <- list(x=9, y=9, level=dbTocd(db), size=0.43, color="white",
             duration=200, responseWindow=1500)
    class(s) <- "opiStaticStimulus"

    return(s)
}
chooseOpi("SimHenson")
if (!is.null(opiInitialize(type="C", cap=6)))
    stop("opiInitialize failed")

result <- FT(makeStim=makeStim, tt=30, fpr=0.15, fnr=0.01)

if (!is.null(opiClose()))
    warning("opiClose() failed")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ misc }
