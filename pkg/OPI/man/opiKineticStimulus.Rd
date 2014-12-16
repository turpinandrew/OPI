\name{opiKineticStimulus}
\alias{opiKineticStimulus}
\title{
Stimulus parameter list.
}
\description{
 List containing stimulus parameters
 with an S3 class attribute of \code{opiKineticStimulus}.
}
\details{ 
The list should contain the following elements.
\itemize{
\item\code{path}    list of (x,y) coordinates in degrees that is usable by
             \code{xy.coords()}
\item\code{image}   \code{image[i]} is the image to display (in a machine specific 
             format) in the section of the path specified by 
             \code{path[i]..path[i+1]}. 

\item\code{levels}  if \code{is.na(image)} then \code{levels[i]} is the stimulus level in
             cd/\eqn{\mbox{m}^2}{m^2} in the section of the path specified by
             \code{path[i]..path[i+1]}

\item\code{sizes}   \code{sizes[i]} is the size of stimulus (diameter in degrees) to use 
             for the section of path specified by \code{path[i]..path[i+1]}, or a 
             scaling factor for \code{images[i]}.
\item\code{colors}  \code{colors[i]} is the color to use for the stimulus in the section 
             of path specified by \code{path[i]..path[i+1]}. 
             Ignored if !is.na(image). 

\item\code{speeds}  \code{speeds[i]} is the speed (degrees per second) for the stimulus 
             to traverse the path specified by \code{path[i]..path[i+1]}.

\item\code{...} machine specific parameters 
}

\subsection{Octopus 900}{
    \code{x} and \code{y} are in degrees, with precision to three decimal places recognised.

    \code{image} is not possible on an Octopus 900.

    \code{levels} are in cd/\eqn{\mbox{m}^2}{m^2}, and are rounded to the 
                nearest one tenth of a dB for display.

    \code{colors} are ignored. Use \code{opiSetBackground()} to alter stimulus color.

    \code{sizes} are in degrees, but are round to the nearest Goldmann Size I..V for display.
}

\subsection{Kowa AP 7000}{
    \code{x} and \code{y} are in degrees (precision?).

    \code{image} is not possible on an Kowa AP 7000.

    \code{levels} are in cd/\eqn{\mbox{m}^2}{m^2}, and are rounded to the 
                nearest one tenth of a dB for display.

    \code{colors} are ignored. Use \code{opiInitialise()} to alter stimulus color.

    \code{sizes} are in degrees, but are round to the nearest Goldmann Size I..V for display.
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

\examples{
    # A Size III white kinetic stimuli on
    # a bilinear path {(27,27), (15,20), (0,0)} 
stim <- list(path=list(x=c(27,15,0), y=c(27,20,0)),
                sizes=rep(0.43,2), 
                colors=rep("white",2), 
                levels=rep(318,2), 
                speeds=c(4,3))
class(stim) <- "opiKineticStimulus"
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ misc }
