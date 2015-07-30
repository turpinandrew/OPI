\name{opiStaticStimulus}
\alias{opiStaticStimulus}
\title{
Stimulus parameter list.
}
\description{
 List containing stimulus parameters
 with an S3 class attribute of \code{opiStaticStimulus}.
}
\details{ 
The list must contain the following elements.

\code{x} coordinate of the center of stimulus in degrees 
         relative to fixation 

\code{y} coordinate of the center of stimulus in degrees 
         relative to fixation 

\code{image} an image to display in a machine specific format 

\code{level} stimulus level in cd/\eqn{\mbox{m}^2}{m^2} (ignored if !is.na(image))

\code{size}  diameter of target in degrees, or scaling factor for 
               image if specified 

\code{color} machine specific stimulus color settings 
                (ignored if !is.na(image)) 

\code{duration} total stimulus duration in milliseconds maximum 

\code{responseWindow} time (>= 0) in milliseconds to wait for a response from 
          the onset of the stimulus presentation

\code{...} machine specific parameters

\subsection{SimHenson and SimGaussian}{
    Only \code{level} is used. 
    Duration and location are ignored, \code{color} is assumed "white" 
    and \code{size} is assumed to be 26/60 (Goldmann III).
}

\subsection{Octopus 900}{
    \code{x} and \code{y} are in degrees, with precision to one decimal place recognised.

    \code{image} is not possible on an Octopus 900.

    \code{level} is in cd/\eqn{\mbox{m}^2}{m^2}, and is rounded to the 
                nearest one tenth of a dB for display.

    \code{color} is ignored. Use \code{opiSetBackground()} to alter stimulus color.
}

\subsection{Octopus 900 F310 Controller}{
    As for the Octopus 900, but a \code{responseWindow} of -1
    means that the Octopus 900 server will wait until 
    either the L and R button is pressed in the controller until
    returning.
}

\subsection{Kowa AP 7000}{
    \code{x} and \code{y} are in degrees. (precision?)

    \code{image} is not possible on an Kowa AP 7000.

    \code{level} are in cd/\eqn{\mbox{m}^2}{m^2} in the range 0.03 to 3183,
                nearest one tenth of a dB for display.

    \code{size} is in degrees, but is rounded to the nearest Goldmann Size I..V for display.

    \code{color} one of 
        \code{.KowaAP7000Env$COLOR_WHITE},
        \code{.KowaAP7000Env$COLOR_GREEN},
        \code{.KowaAP7000Env$COLOR_BLUE}, and 
        \code{.KowaAP7000Env$COLOR_RED}.
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
Andrew Turpin <aturpin@unimelb.edu.au>
}

\seealso{
\code{\link{opiSetBackground}}
}

\examples{
    stim <- list(x=9, y=9, image=NA, 314, size=0.43, color="white", 
                duration=200, responseWindow=1500)
    class(stim) <- "opiStaticStimulus"
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ misc }
