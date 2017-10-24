\name{opiSetBackground}
\alias{opiSetBackground}
\title{
Set background using OPI.
}
\description{
Generic function for setting background of the chosen OPI implementation that is set with
\code{chooseOpi()}.
}
\usage{
opiSetBackground(...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{\dots}{Implementation specific parameters. See details.}
}
\details{
\subsection{Octopus900}{ 
    \code{opiSetBackground(lum=NA, color=NA, fixation=NA, fixIntensity=NA)} 

    Allowable \code{lum} and \code{color} are defined in the .Octopus900Env environment.

    \itemize{
    \item\code{lum} is intensity of the background and can be one of
      \itemize{
        \item \code{.Octopus900Env$BG_OFF}, which turns background off.
        \item \code{.Octopus900Env$BG_1}, background of 1.27 cd/\eqn{\mbox{m}^2}{m^2}.
        \item \code{.Octopus900Env$BG_10}, background of 10 cd/\eqn{\mbox{m}^2}{m^2}.
        \item \code{.Octopus900Env$BG_100}, background of 100 cd/\eqn{\mbox{m}^2}{m^2}.
      }

    \item\code{color} can be one of the following choices.
        \itemize{
            \item\code{.Octopus900Env$MET_COL_WW} for white-on-white
            \item\code{.Octopus900Env$MET_COL_RW} for red-on-white
            \item\code{.Octopus900Env$MET_COL_BW} for blue-on-white
            \item\code{.Octopus900Env$MET_COL_WY} for white-on-yellow
            \item\code{.Octopus900Env$MET_COL_RY} for red-on-yellow
            \item\code{.Octopus900Env$MET_COL_BY} for blue-on-yellow
        }


    \item\code{fixation} is one of 
        \itemize{
            \item\code{.Octopus900Env$FIX_CENTRE} or \code{.Octopus900Env$FIX_CENTER}
            \item\code{.Octopus900Env$FIX_CROSS}
            \item\code{.Octopus900Env$FIX_RING}
        }

    \item\code{fixIntensity} is a percentage between 0 and 100. 0 is off, 100 the brightest.
  }
}
\subsection{SimHenson and SimGaussian}{ 
    \code{opiSetBackground(col, gridCol)} 

    \code{col} is the background color of the
    plot area used for displaying stimuli, and \code{gridCol} the color of the gridlines.  
    Note the plot area will
    only be displayed if \code{opiInitialize} is called with a valid
    display argument.
}
\subsection{Octopus600}{ 
This function has no effect.
}
\subsection{KowaAP7000}{ 
    \code{opiSetBackground(lum, color, fixation,)} 

    \code{lum} and \code{color} are dependant for the Kowa AP-7000. 
    A white background must be 10 cd/\eqn{\mbox{m}^2}{m^2}, and a yellow background must be.
    100 cd/\eqn{\mbox{m}^2}{m^2}.
    If \code{lum} is 10 and \code{color} is not set, then \code{.KowaAP7000Env$BACKGROUND_WHITE}
    is assumed.
    If \code{lum} is 100 and \code{color} is not set, then \code{.KowaAP7000Env$BACKGROUND_YELLOW}
    is assumed.
    If both \code{lum} and \code{color} is set, then \code{lum} is ignored (a warning will be generated
    if \code{lum} is incompatible with \code{color}).
    
    \code{fixation} is one of
    \itemize{
        \item \code{.KowaAP7000Env$FIX_CENTER}, fixation marker in the centre.
        \item \code{.KowaAP7000Env$FIX_CENTRE}, fixation marker in the centre.
        \item \code{.KowaAP7000Env$FIX_AUX},    fixation marker is ???.
        \item \code{.KowaAP7000Env$FIX_MACULA}, fixation marker is a circle(?).
        \item \code{.KowaAP7000Env$FIX_AUX_LEFT}, fixation marker is as for AUX but only lower left.
    }
}
\subsection{Compass}{
    \code{opiSetBackground(fixation=NA, tracking_on=NA)}
    \itemize{
    \item{\code{fixation}=c(x,y,t)} where
        \itemize{
        \item{\code{x}} is one of -20, -6, -3, 0, 3, 6, 20 degrees.
        \item{\code{y}} is 0 degrees.
        \item{\code{t}} is 0 for a spot fixation marker at \code{c(x,y)}, or 1 for a square 
            centred on one of \code{(-3,0)}, \code{(0,0)}, \code{(+3,0)}.
        }
    \item{\code{tracking_on}} is either 0 (tracking off) or 1 (tracking on).
    }

Note: tracking will be relative to the PRL established with the fixation marker used at
setup (call to OPI-OPEN), so when tracking is on you should use the 
same fixation location as in the setup.
}

}

\value{
Returns NULL if succeeded, 
otherwise an implementation dependant error as follows.

\subsection{Octopus900}{ 
    -1 indicates \code{opiInitialize} has not been called.

    -2 indicates could not set the background color.

    -3 indicates could not set the fixation marker.

    -4 indicates that all input parameters were NA.
}
}
\references{
Please cite:
A. Turpin, P.H. Artes and A.M. McKendrick
"The Open Perimetry Interface: An enabling tool for clinical visual psychophysics",
Journal of Vision 12(11) 2012.

http://people.eng.unimelb.edu.au/aturpin/opi/index.html
}
\author{
Andrew Tuprin <aturpin@unimelb.edu.au>
}

\seealso{
\code{\link{chooseOpi}}
}

\examples{
chooseOpi("SimGaussian")
if (!is.null(opiInitialize(sd=2, display=c(-30,30,-30,30))))
    stop("opiInitialize failed")
if (!is.null(opiSetBackground(col="white",gridCol="grey")))
    stop("opiSetBackground failed, which is very surprising!")

\dontrun{
chooseOpi("Octopus900")
oi <- opiInitialize(eyeSuiteJarLocation="c:/EyeSuite/",
        eyeSuiteSettingsLocation="c:/Documents and Settings/All Users/Haag-Streit/",
        eye="left")
if (!is.null(oi))
    stop("opiInitialize failed")
if (!is.null(opiSetBackground(fixation=.Octopus900Env$FIX_CENTRE)))
    stop("opiSetBackground failed")
if (!is.null(opiSetBackground(fixation=.Octopus900Env$FIX_RING, fixIntensity=0)))
    stop("opiSetBackground failed")
if (!is.null(opiSetBackground(color=.Octopus900Env$MET_COL_BY)))
    stop("opiSetBackground failed")
if (!is.null(opiSetBackground(lum=.Octopus900Env$BG_100, color=.Octopus900Env$MET_COL_RW)))
    stop("opiSetBackground failed")
opiClose()
}
}

\keyword{ misc }
