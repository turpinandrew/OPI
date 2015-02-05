\name{opiPresent}
\alias{opiPresent}
\title{
Use OPI to present stimulus.
}
\description{
Generic function for presentation of stimulus stim. 
Depending on your choice of OPI implementation set using 
\code{chooseOpi()}, 
different parameters are available for \code{opiPresent}.
}
\usage{
opiPresent(stim, nextStim=NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{stim}{
    A list of class \code{\link{opiStaticStimulus}}, \code{\link{opiKineticStimulus}}, or
        \code{\link{opiTemporalStimulus}}. 
  }
  \item{nextStim}{
    As for \code{stim}, but the next presentation to be made. 
    This might be useful
    on some machines, particularly projector based systems, where preparations
    for the next presentation can be made while waiting for a response to the
    current.
  }
  \item{\dots}{
    Parameters specific to your chosen opi implementation.
  }
}
\details{
\code{\link{opiPresent}}
is blocking in that it will not return until either a response
is obtained, or at least the responseWindow milliseconds has expired. (Note
that more time might have expired.) Specifying \code{nextStim} 
allows the implementing
machine to use the time waiting for a 
response to \code{stim} to make preparations
for the next stimuli. (For example retargeting the projector or moving
aperture and/or filter wheels.) 
There is no guarantee that the next call to
\code{\link{opiPresent}} will have \code{nextStim} as the first argument; this 
could be checked by the machine specific implementations (but currently is not, I think).

Also note that to allow for different parameters depending on the 
implementation chosen with \code{chooseOpi}, every parameter MUST
be named in a call to \code{\link{opiPresent}}.

\subsection{SimHenson}{

\code{opiPresent(stim, nextStim=NULL, fpr=0.03, fnr=0.01, tt=30)}

If the chosen OPI implementation is \code{SimHenson}, then
the response to a stimuli is determined by sampling from a
Frequency-of-Seeing (FoS) curve (also known as the psychometric
function) with formula \deqn{\mbox{fpr}+(1-\mbox{fpr}-\mbox{fnr})(1-\mbox{pnorm}(x, \mbox{tt},
\mbox{pxVar})),}{\code{fpr+(1-fpr-fnr)*(1-pnorm(x, tt, pxVar))},}
where \eqn{x}{\code{x}} is the stimulus value in Humphrey dB, and pxVar is
\deqn{\min\left(\mbox{simH.global.cap}, e^{A\times\mbox{tt}+B}\right).}{\code{min(simH.cap, exp(A*tt + B))}.} 
The ceiling \code{simH.global.cap}
is set with the call to \code{opiInitialize}, and \code{A} and \code{B}
are from Table 1 in Henson et al (2000). Which values are used is
determined by \code{simH.type} which is also set in the call to
\code{opiInitialize}.

Note that if the stimulus value is less than zero, then the Henson
formula is not used. The probability of seeing is \code{fpr}.
}

\subsection{SimHensonRT}{

\code{opiPresent(stim, nextStim=NULL, fpr=0.03, fnr=0.01, tt=30, dist=stim$level - tt)}

This presentation is the same as for \code{SimHenson}, but reaction
times are determined by sampling from \code{rtData} as passed to
\code{opiInitialize}.  The \code{dist} parameter is the distance
of the stimulus level from the true threshold, and should be in the
same units as the \code{Dist} column of \code{rtData}. The default
is just the straight difference between the stimulus level and the
true threshold, but you might want it scaled somehow to match
\code{rtData}.

}


\subsection{SimGaussian}{

\code{opiPresent(stim, nextStim=NULL, fpr=0.03, fnr=0.01, tt=30)}

If the chosen OPI implementation is \code{SimGaussian}, then
the response to a stimuli is determined by sampling from a
Frequency-of-Seeing (FoS) curve (also known as the psychometric
function) with formula \code{fpr+(1-fpr-fnr)*(1-pnorm(x, tt, simG.global.sd))},
where \code{x} is the stimulus value in Humphrey dB, and \code{simG.global.sd} is
set with \code{opiInitialize}.
}

\subsection{SimYes}{

\code{opiPresent(stim, nextStim=NULL)}

If the chosen OPI implementation is \code{SimYes}, then
the response to a stimuli is always yes, hence \code{\link{opiPresent}} always returns 
\code{err=NULL}, \code{seen=TRUE}, and \code{time=0}.
}

\subsection{SimNo}{

\code{opiPresent(stim, nextStim=NULL)}

If the chosen OPI implementation is \code{SimNo}, then
the response to a stimuli is always no, hence \code{\link{opiPresent}} always returns 
\code{err=NULL}, \code{seen=FALSE}, and \code{time=0}.
}

\subsection{Octopus600}{

\code{opiPresent(stim, nextStim=NULL)}

If the chosen OPI implementation is \code{Octopus600}, then nextStim is ignored. If \code{eyeControl} is non-zero, 
as set in \code{opiInitialize}, answer codes describing patient state may arise (see \code{answer} field in the Value section).
}

\subsection{KowaAP7000}{

\code{opiPresent(stim, nextStim=NULL)}
If the chosen OPI implementation is \code{KowaAP7000}, then nextStim is ignored. 
}


}
\value{
A list containing
\item{err }{\code{NULL} if no error occurred, otherwise a machine specific error
message.
This should include errors when the specified size cannot be achieved by
the device (for example, in a projection system with an aperture wheel of
predefined sizes.) If \code{stim} is \code{NULL}, then \code{err} contains the status of the
machine.}
\item{seen }{\code{TRUE} if a response was detected in the allowed
\code{responseWindow}, \code{FALSE} otherwise.}
\item{time}{The time in milliseconds from the onset (or offset, machine
specific) of the presentation
until the response from the subject if \code{seen} is \code{TRUE}. 
If \code{seen} is \code{FALSE}, this
value is undefined.
}
\item{frames}{Only returned for Octopus 900. An array of integer 0..255 for gaze image. 
In version 1.5, only a single frame is returned.
See the parameter \code{gazeFeed} for \code{\link{opiInitialize}} to control what frames are returned.
}
\item{numFrames}{Only returned for Octopus 900. The number of frames in \code{frames}.
}
\item{width}{Only returned for Octopus 900. Width of frame in \code{frames}.
}
\item{height}{Only returned for Octopus 900. Height of frame in \code{frames}.
}
\item{answer}{Only returned for \code{Octopus600}. Can be the following values:
  \itemize{
    \item 0 = stimulus not seen;
    \item 1 = stimulus seen;
    \item 132 = Response button was pressed before stimulus presentation (Patient needs a break - hold on examination);
    \item 36 = Eye is closed before stimulus presentation;
    \item 68 = Fixation lost before stimulus presentation (pupil center is out of green window in video image);
    \item 260 = Forehead rest lost before stimulus presentation;
    \item 516 = Fast Eye movements before stimulus presentation;
    \item 258 = Forehead rest lost during stimulus presentation;
    \item 66 = Fixation lost during stimulus presentation (pupil center is out of green window in video image);
    \item 34 = Eye was closed during stimulus presentation;
    \item 18 = Patient answer was too early (<=100ms after stimulus presentation) - lucky punch;
    \item 514 = Fast Eye movements during stimulus presentation
  }
}
\item{pupilX}{Only returned for KowaAP7000 and an opiStaticStimulus. 
          x-coordinate of pupil in pixels during presentation.}
\item{pupilY}{Only returned for KowaAP7000 and an opiStaticStimulus. 
          y-coordinate of pupil in pixels during presentation.}
\item{x}{Only returned for KowaAP7000 or Octopus900 and an opiKineticStimulus. 
          x coordinate of stimuli when button is pressed.}
\item{y}{Only returned for KowaAP7000 or Octopus900 and an opiKineticStimulus. 
          y coordinate of stimuli when button is pressed.}

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
\code{\link{opiStaticStimulus}}, 
\code{\link{opiKineticStimulus}}, 
\code{\link{opiTemporalStimulus}}, 
\code{\link{chooseOpi}}
}

\examples{
    # Stimulus is Size III white-on-white as in the HFA
makeStim <- function(db, n) { 
    s <- list(x=9, y=9, level=dbTocd(db, 10000/pi), size=0.43, color="white",
             duration=200, responseWindow=1500)
    class(s) <- "opiStaticStimulus"

    return(s)
}
chooseOpi("SimHenson")
if (!is.null(opiInitialize(type="C", cap=6)))
    stop("opiInitialize failed")

result <- opiPresent(stim=makeStim(10,0), tt=30, fpr=0.15, fnr=0.01)

    # Will not work as 'stim' is not named
#result <- opiPresent(makeStim(10,0), tt=30, fpr=0.15, fnr=0.01)

if (!is.null(opiClose()))
    warning("opiClose() failed")

    # Same but with simulated reaction times
chooseOpi("SimHensonRT")
data(RtSigmaUnits)
if (!is.null(opiInitialize(type="C", cap=6, rtData=RtSigmaUnits)))
    stop("opiInitialize failed")

dist <- (10 - 30)/min(exp(-0.098 * 30 + 3.62), 6)
result <- opiPresent(stim=makeStim(10,0), tt=30, fpr=0.15, fnr=0.01, dist=dist)

if (!is.null(opiClose()))
    warning("opiClose() failed")
}

\keyword{ misc }
