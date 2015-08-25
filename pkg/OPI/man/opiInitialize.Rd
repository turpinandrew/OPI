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
    \code{opiInitialize(type="C", A=NA, B=NA, cap=6, display=NULL, maxStim=10000/pi)}

    If the chosen OPI implementation is \code{SimHenson}, then
  \code{type} can be one of:
    \code{"N"}, for normal patients; \code{"G"}, for POAG patients;
    and \code{"C"}, for a combination. See Table 1 in Henson et al
    (2000).  If \code{type} is \code{"X"} then \code{A} and \code{B}
    should be specified and are used in place of one of the three
    A/B combinations as in Henson et al (2000).
    \code{cap} is the maximum
    standard deviation value that the simulator will use for
    the slope/spread of the psychometric function.

    If \code{display} is a vector of four numbers \code{c(xlow, xhi, ylow, yhi)}, then 
    a plot area is created of dimension \code{xlim=range(xlow, xhi)} and 
    \code{ylim=range(ylow, yhi)} and each call to \code{opiPresent} will
    display a point on the area.
    The color of the plot area can be set with \code{opiSetBackground}, and the 
    color of the displayed point is determined by the stimulus passed to \code{opiPresent}.

    \code{maxStim} is the maximum stimuls value in cd/\eqn{\mbox{m}^2}{m^2}. This is 
    used in converting cd/\eqn{\mbox{m}^2}{m^2} to dB values, and vice versa.
}
\subsection{SimHensonRT}{
    \code{opiInitialize(type="C", A=NA, B=NA, cap=6, display=NULL, maxStim=10000/pi, rtData, rtFP=1:1600)}

    If the chosen OPI implementation is \code{SimHensonRT}, then
    the first six parameters are as in \code{SimHenson}, and 
  \code{rtData} is a data frame with at least 2 columns:
    \code{"Rt"}, reponse time; and
    \code{"Dist"}, signifying that distance between assumed threshold and stimulus value in
        your units.

    This package contains \code{RtSigmaUnits} or \code{RtDbUnits} 
    that can be loaded with the commands
    \code{data(RtSigmaUnits)} or \code{data(RtDbUnits)}, and are suitable to pass as values
    for \code{rtData}.

    \code{rtFp} gives the vector of values in milliseconds from
    which a response time for a false positive response is randomly
    sampled.
}
\subsection{SimGaussian}{
    \code{opiInitialize(sd, display=NULL, maxStim=10000/pi)}

    If the chosen OPI implementation is \code{SimGaussian}, then \code{sd}
    is the standard deviation value that the simulator will use for 
    the slope/spread of the psychometric function.

    \code{display} and \code{maxStim} is as for SimHenson.
}
\subsection{Octopus900}{
    \code{opiInitialize(eyeSuiteSettingsLocation, eye, gazeFeed=0, bigWheel=FALSE),buzzer=0}

    If the chosen OPI implementation is \code{Octopus900}, then you must 
    specify a directory and the eye to be tested.

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

    \code{bigWheel} is \code{FALSE} for a standard Octopus 900 machine. Some research machines
    are fitted with an alternate aperture wheel that has 24 sizes, which are accessed
    with \code{bigWheel} is \code{TRUE}. 
    The mapping from size to 'hole on wheel' is hard coded; see code for
    details.

    \code{buzzer} is the buzzer volume which can be one 
     of 0 (no buzzer), 1, 2, or 3 (max volume). The buzzer sounds before each presentation.
}
\subsection{Octopus600}{
    \code{opiInitialize(ipAddress, eye, pupilTracking=FALSE, pulsar=FALSE, eyeControl=0)}

    If the chosen OPI implementation is \code{Octopus600}, then you must 
    specify the IP address of the Octopus 600 and the eye to test.

    \code{ipAddress} is the IP address of the Octopus 600 as a string.

    \code{eye} must be either "left" or "right".
    
    \code{pupilTracking} is TRUE to turn on IR illumination and set pupil black level (which happens
    at the first stimulus presentation).
    
    \code{pulsar} is TRUE for pulsar stimulus, FALSE for size III white-on-white.

    \code{eyeControl}
    \itemize{ 
      \item 0 is off
      \item 1 is eye blink
      \item 2 is eye blink, forehead rest, fixation control
      \item 3 is eye blink, forehead rest, fixation control, fast eye movements
    }
}
\subsection{KowaAP7000}{
    \code{opiInitialize(ip, port)}

    If the chosen OPI implementation is \code{KowaAP7000}, then you must 
    specify the IP address and port of the AP-7000 server.

    \code{ipAddress} is the IP address of the AP-7000 server as a string.

    \code{port} is the TCP/IP port of the AP-7000 server as a number.
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

\subsection{Octopus600}{
Returns NULL if successful, or an Octopus 600 error code

The default background and stimulus setup is to white-on-white perimetry.
}

\subsection{Kowa AP-7000}{
Always returns NULL. 
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
    # and display the stimuli in a plot region and simulate response times
chooseOpi("SimHensonRT")
data(RtSigmaUnits)
oi <- opiInitialize(type="C", cap=6, 
        display=c(-30,30,-30,30), rtData=RtSigmaUnits, rtFP=1:100)
if (!is.null(oi))
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
        eyeSuiteJarLocation="C:/Program Files (x86)/Haag-Streit/EyeSuite/", 
        eyeSuiteSettingsLocation="C:/ProgramData/Haag-Streit/EyeSuite/",
        eye="left")))
    stop("opiInitialize failed")
}

\dontrun{
    # Set up the Kowa AP-7000
chooseOpi("KowaAP7000")
if (!is.null(opiInitialize(ip="192.168.1.7", port=44965)))
    stop("opiInitialize failed")
}
}

\keyword{ misc }
