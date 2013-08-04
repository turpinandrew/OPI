\name{RtSigmaUnits}
\alias{RtSigmaUnits}
\docType{data}
\title{
    Response times to white-on-white Goldmann Size III targets for 12 subjects.
}
\description{
    Response times to white-on-white Goldmann Size III targets for 12 subjects.
    The second column is the distance of the stimuli from measured threshold 
    in 'sigma' units.
    The threshold was determined by post-hoc fit of a cummulative gaussian
    FoS curve to the data for each location and subject.
    Sigma is the standard deviation of the fitted FoS.
}
\usage{data(RtSigmaUnits)}
\format{
  A data frame with 30620 observations on the following 3 variables.
  \describe{
    \item{\code{Rt}}{Reaction time in ms.}
    \item{\code{Dist}}{Distance of stimuli from threshold in sigma units.}
    \item{\code{Person}}{Identifier of each subject.}
  }
}
\source{
A.M. McKendrick, J. Denniss and A. Turpin.
"Response times across the visual field: empirical 
observations and application to threshold determination"
In preparation, Aug 2013.
}
\references{
A.M. McKendrick, J. Denniss and A. Turpin.
"Response times across the visual field: empirical 
observations and application to threshold determination"
In preperation, Aug 2013.
}
\examples{
data(RtSigmaUnits)
}
\keyword{datasets}
