\name{chooseOpi}
\alias{chooseOpi}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Choose an implementation of the OPI.
}
\description{
Chooses an implementation of the OPI to use.
}
\usage{
chooseOpi(opiImplementation)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{opiImplementation}{A character string that is one of the following.
    \itemize{
    \item\code{"SimNo"} for a simulator that always doesn't see.
    \item\code{"SimYes"} for a simulator that always does see.
    \item\code{"SimHenson"} for a simulator that uses a cummulative gaussian 
                psychometric function with standard deviation 
                according to Henson et al (2000) where variability 
                increases as true threshold (Humphrey dB) value 
                decreases. 
    \item\code{"SimHensonRT"} as for SimHenson, but response times
                in ms are sampled from a supplied response time data set 
                for each true positive response.
    \item\code{"SimGaussian"} for a simulator that uses a cummulative gaussian 
                psychometric function with 
                standard deviation supplied in opiInitialize(). 
    \item\code{"Octopus900"} for interfacing with the Octopus 900. 
    \item\code{"Octopus900F310"} for interfacing with the Octopus 900 using Logitech F310 controller.
    \item\code{"Octopus600"} for interfacing with the Octopus 600. 
    \item\code{"HEP"}        not working so well in new HEPs.
    \item\code{"KowaAP7000"} for interfacing with Kowa AP-7000.
    \item\code{NULL}         print a list of available OPI implementations.
    }
}
}
\value{ Returns TRUE if successful, FALSE otherwise.}

\references{
David B. Henson, Shaila Chaudry, Paul H. Artes, E. Brian Faragher, and
Alec Ansons.  
Response Variability in the Visual Field: Comparison of Optic Neuritis,
Glaucoma, Ocular Hypertension, and Normal Eyes.
Investigative Ophthalmology & Visual Science, February 2000, Vol. 41(2).

A.M. McKendrick, J. Denniss and A. Turpin 
"Response times across the visual field: empirical observations and application to threshold
determination".  In submission, August 2013.

Please cite:
A. Turpin, P.H. Artes and A.M. McKendrick
"The Open Perimetry Interface: An enabling tool for clinical visual psychophysics",
Journal of Vision 12(11) 2012.

http://perimetry.org/OPI

}

\author{
Andrew Turpin <aturpin@unimelb.edu.au>
}

%\note{ }

\examples{
    if(!chooseOpi("SimHenson"))
        warnings()
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ misc }
