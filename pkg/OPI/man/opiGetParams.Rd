\name{opiGetParams}
\alias{opiGetParams}
\title{
Get OPI method parameters.
}
\description{
Get parameters of OPI functions which depends on the implementation set with \code{chooseOPI()}.
}
\usage{
opiGetParams(method, ...)
}
\arguments{
    \item{method}{Method for which to get parameters and defaults.}
    \item{\dots}{Implementation specific parameters. See details.}
}

\value{
Returns a list of parameters and their default vlues of the method \code{method} depending on
the OPI implementation selected with \code{chooseOPI()}.
}

\examples{
chooseOpi("SimHenson")
opiGetParams("opiInitialize")
opiGetParams("opiPresent")
}