\name{opiInitialize}
\alias{opiInitialize}
\alias{opiInitialise}
\title{
Initialize OPI.
}
\description{
Generic function for initialization of the chosen OPI implementation that is set with
\code{chooseOpi()}.
}
\usage{
opiInitialize(...)
opiInitialise(...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{\dots}{Implementation specific parameters. See details.}
}
\details{
\subsection{SimHenson}{
    \code{opiInitialize(type="C", cap=6, display=NULL)}

    If the chosen OPI implementation is \code{SimHenson}, then
  \code{type} can be one of:
    \code{"N"}, for normal patients; 
    \code{"G"}, for POAG patients; and
    \code{"C"}, for a combination. See Table 1 in Henson et al (2000).
    \code{cap} is the maximum
    standard deviation value that the simulator will use for
    the slope/spread of the psychometric function.

    If \code{display} is a vector of four numbers \code{c(xlow, xhi, ylow, yhi)}, then 
    a plot area is created of dimension \code{xlim=range(xlow, xhi)} and 
    \code{ylim=range(ylow, yhi)} and each call to \code{opiPresent} will
    display a point on the area.
    The color of the plot area can be set with \code{opiSetBackground}, and the 
    color of the displayed point is determined by the stimulus passed to \code{opiPresent}.
}
\subsection{SimHensonRT}{
    \code{opiInitialize(type="C", cap=6, display=NULL, rtData)}

    If the chosen OPI implementation is \code{SimHensonRT}, then
    the first three parameters are as in \code{SimHenson}, and 
  \code{rtData} is a data frame with 3 columns:
    \code{"Rt"}, reponse time; 
    \code{"Dist"}, signifying that distance between assumed threshold and stimulus value is your units; and
    \code{"Person"}, which identifies the subject from which the \code{Rt} and \code{Dist} was taken. 

    This package contains \code{RtSigmaUnits} or \code{RtDbUnits} 
    that can be loaded with the commands
    \code{data(RtSigmaUnits)} or \code{data(RtDbUnits)}.
}
\subsection{SimGaussian}{
    \code{opiInitialize(sd, display=NULL)}

    If the chosen OPI implementation is \code{SimGaussian}, then \code{sd}
    is the standard deviation value that the simulator will use for 
    the slope/spread of the psychometric function.

    \code{display} is as for SimHenson.
}
\subsection{Octopus900}{
    \code{opiInitialize(eyeSuiteJarLocation, eyeSuiteSettingsLocation, eye, gazeFeed=0)}

    If the chosen OPI implementation is \code{Octopus900}, then you must 
    specify two directory/folder names and the eye to be tested.

    \code{eyeSuiteJarLocation} is the folder name containing the EyeSuite jar files,
    and should include the trailing slash.

    \code{eyeSuiteSettingsLocation} is the folder name containing the EyeSuite setting files,
    and should include the trailing slash.

    \code{eye} must be either "left" or "right".

    \code{gazeFeed} is 0 for no gaze tracking infromation. 
    If \code{gazeFeed} is 1, then 
    a single frame is returned as part of the value from \code{\link{opiPresent}}
    which is the most recent frame captured. 
    If \code{gazeFeed} is 2, then 
    a all frames containing the asterix are returned as part of the value from \code{\link{opiPresent}},
    which are the frames captured while a static stimulus is displayed. 
    If \code{gazeFeed} is greater than zero, 
    a Java driven window appears containing the live feed from the Octopus 900 gaze camera.
}
}

\value{
Returns NULL if initialization succeeded, 
otherwise an implementation dependant error.

\subsection{Octopus900}{
Returns NULL if successful, 1 if Octopus900 is already initialised by a previous call to 
\code{opiInitialize}, and 2 if some error occured that prevented ininitialisation.

The default background and stimulus setup is to white-on-white perimetry. Use \code{opiSetBackground}
to change the background and stimulus colors.
}

}
\references{
Please cite:
A. Turpin, P.H. Artes and A.M. McKendrick
"The Open Perimetry Interface: An enabling tool for clinical visual psychophysics",
Journal of Vision 12(11) 2012.

http://perimetry.org/OPI

David B. Henson, Shaila Chaudry, Paul H. Artes, E. Brian Faragher, and
Alec Ansons.  
Response Variability in the Visual Field: Comparison of Optic Neuritis,
Glaucoma, Ocular Hypertension, and Normal Eyes.
Investigative Ophthalmology & Visual Science, February 2000, Vol. 41(2).
}
\author{
Andrew Tuprin <aturpin@unimelb.edu.au>
}

\seealso{
\code{\link{chooseOpi}},
\code{\link{opiSetBackground}},
\code{\link{opiClose}},
\code{\link{opiPresent}}
}

\examples{
    # Set up a simple simulation for white-on-white perimetry
chooseOpi("SimHenson")
if (!is.null(opiInitialize(type="C", cap=6)))
    stop("opiInitialize failed")

    # Set up a simple simulation for white-on-white perimetry
    # and display the stimuli in a plot region
chooseOpi("SimHenson")
if (!is.null(opiInitialize(type="C", cap=6, display=c(-30,30,-30,30))))
    stop("opiInitialize failed")

    # Set up a simple simulation for white-on-white perimetry
    # and display the stimuli in a plot region
chooseOpi("SimHensonRT")
data(RtSigmaUnits)
if (!is.null(opiInitialize(type="C", cap=6, display=c(-30,30,-30,30), rtData=RtSigmaUnits, rtFP=1:100)))
    stop("opiInitialize failed")

    # Set up a simulation using a psychometric function that is
    # a cumulative gaussian of standard deviation 2
chooseOpi("SimGaussian")
if (!is.null(opiInitialize(sd=2)))
    stop("opiInitialize failed")

\dontrun{
    # Set up the Octopus 900
chooseOpi("Octopus900")
if (!is.null(opiInitialize(
        eyeSuiteJarLocation="C:/EyeSuite/", 
        eyeSuiteSettingsLocation="C:/Documents and Settings/All Users/HAAG-STREIT/EyeSuite/",
        eye="left")))
    stop("opiInitialize failed")
}
}

\keyword{ misc }
