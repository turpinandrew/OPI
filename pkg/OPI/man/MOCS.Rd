\name{MOCS}
\alias{MOCS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Method of Constant Stimuli (MOCS)
}
\description{
MOCS performs either a yes/no or n-interval-forced-choice Method of 
Constant Stimuli test.
}
\usage{
    MOCS(params=NA, order="random", 
        responseWindowMeth="constant", responseFloor=1500, responseHistory=5, 
        keyHandler=function(correct,ret) return(list(TRUE, 0, NULL)), 
        interStimMin=200, interStimMax=500, 
        beep_function, 
        makeStim, 
        stim_print, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{params}{A matrix where each row is \code{x y n correct_n ll1 ll2 ... llm}
        where
        \itemize{
            \item{\code{x} is X coordinate of location}
            \item{\code{y} is Y coordinate of location}
            \item{\code{n} is Number of times this location/luminance(s) pair should be repeated}

            \item{\code{correct_n} is the index i of the luminance level (\code{lli}) that
            should be treated as a ``correct'' response (the correct interval).
            For a standard MOCS, this will be 1; for a 2AFC, this will
            be 1 or 2. This number will be in the range [1,m].} 

            \item{\code{lli} is the i'th luminance level to be used at
            this location for interval i of the presentation in cd/\eqn{\mbox{m}^2}{m^2}.
            For a standard MOCS, i=1, and the \code{params} matrix will have 5 columns.
            For a 2AFC, there will be two lli's, and \code{params} will have 6 columns.
            } 
        }
    }

    \item{order}{Control the order in which the stimuli are presented.
        \itemize{
               \item{\code{"random"} Randomise the order of trials/locations.}
               \item{\code{"fixed"} Present each row of \code{params} in order 
                of \code{1:nrow(params)}, ignoring the \code{n} (3rd) column in \code{params}.
                }
        }
    }

    \item{responseWindowMeth}{Control time perimeter waits for response. 
        \itemize{
              \item{\code{"speed"} After an average of the last \code{speedHistory}
                         response times, with a minimum of \code{responseFloor}.
                         Initially \code{responseFloor}.}
              \item{\code{"constant"} Always use \code{responseFloor}.}
              \item{\code{"forceKey"} Wait for a keyboard input.}
        }
    }

    \item{responseHistory}{Number of past yeses to average to get response window
                      (only used if \code{responseWindowMeth} is \code{"speed"}).}

    \item{responseFloor}{Minimum response window (for any \code{responseWindowMeth} 
            except \code{"forceKey"}).}
                   
    \item{keyHandler}{Function to get a keyboard input and returns as for \code{opiPresent}:
                list(seen={TRUE|FALSE}, response time (in ms), error code)
                The parameters passed to the function are the correct interval number 
                (column 4 of \code{params}), and 
                the result of \code{opiPresent}. See Examples.
    }
                   
   \item{interStimMin}{Regardless of response, wait \code{runif(interStimMin, interStimMax)} ms.}
   \item{interStimMax}{Regardless of response, wait \code{runif(interStimMin, interStimMax)} ms.}

   \item{beep_function}{A function that takes the string
   \code{'correct'}, the string \code{'incorrect'}, 
   or a stimulus number and plays an appropriate sound.  See examples.} 

  \item{makeStim}{
            A helper function to take a row of \code{params} and a response window 
             length in ms, and create a list of OPI stimuli types for 
             passing to opiPresent. This may include a \code{checkFixationOK} function.
             See Example.
    }

  \item{stim_print}{A function that takes an \code{opiStaticStimulus} and return list from 
        \code{opiPresent} and returns a string to print for each presentation. It is called
        immediately after each \code{opiPresent}, and the string is prepended with the 
        (x,y) coordinates of the presentation and ends with a newline.}

  \item{\dots}{Extra parameters to pass to the opiPresent function.}
}
\details{
    Whether the test is yes/no or forced-choice is determined by the 
    number of columns in \code{params}. The code simply presents all 
    columns from 5 onwards and collects a response at the end. So if
    there is only 5 columns, it is a yes/no task. If there are 6 columns
    it is a 2-interval-forced-choice. Generally, an nIFC experiment
    has 4+n columns in \code{params}.

    Note that when the \code{order} is \code{"random"}, the number of trials in 
    the test will be the sum of the 3rd column of \code{params}. When
    the \code{order} is \code{"fixed"}, there is only one presentation 
    per row, regardless of the value in the 3rd column of \code{params}.

    If a response is received before the final trial in a nIFC experiment,
    it is ignored.

    If the \code{checkFixationOK} funciton is present in a stimulus, then it
    is called after each presentation, and the result is ``anded'' with each stimulus 
    in a trial to get a TRUE/FALSE for fixating on all stimuli in a trial.

}
\value{
 Returns a data.frame with one row per stimulus copied from params with extra columns that
 are the return values from \code{opiPresent()} and a record of fixation (if \code{checkFixationOK} present in stim
objects returned from \code{makeStim}: see example). These last values will differ depending
 on which machine/simulation you are running (as chosen with \code{chooseOpi()}.
 \itemize{
        \item{column 1: x}
        \item{column 2: y}
        \item{column 3: correct stimulus index}
        \item{column 4: TRUE/FALSE was fixating for all presentations in this trial according to \code{checkFixationOK}}
        \item{column 5...: columns from params}
        \item{...: columns from opiPresent return}
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

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{dbTocd}}, \code{\link{opiPresent}}
}
\examples{
    # For the Octopus 900
    # Check if pupil centre is within 10 pixels of (160,140)
checkFixationOK <- function(ret) return(sqrt((ret$pupilX - 160)^2 + (ret$pupilY - 140)^2) < 10)

    # Return a list of opi stim objects (list of class opiStaticStimulus)
    # for each level (dB) in p[5:length(p)]. Each stim has responseWindow
    # BETWEEN_FLASH_TIME, except the last which has rwin.
    # This one assumes p is on old Octopus 900 dB scale (0dB == 4000 cd/m^2).
makeStim <- function(p, rwin) {
    BETWEEN_FLASH_TIME <- 750   # ms

    res <- NULL
    for(i in 5:length(p)) {

        s <- list(x=p[1], y=p[2], level=dbTocd(p[i],4000/pi), size=0.43, duration=200,
                  responseWindow=ifelse(i < length(p), BETWEEN_FLASH_TIME, rwin),
                  checkFixationOK=NULL
                )
        class(s) <- "opiStaticStimulus"
        res <- c(res, list(s))
    }
    return(res)
}

################################################################
# Read in a key press 'z' is correct==1, 'm' otherwise
#    correct is either 1 or 2, whichever is the correct interval
#   
# Return list(seen={TRUE|FALSE}, time=time, err=NULL))
#        seen is TRUE if correct key pressed
################################################################
\dontrun{
if (length(dir(".", "getKeyPress.py")) < 1)
   stop('Python script getKeyPress.py missing?')
}

keyHandler <- function(correct, ret) { 
    return(list(seen=TRUE, time=0, err=NULL))
    ONE <- "b'z'"
    TWO <- "b'm'"
    time <- Sys.time()
    key <- 'q'
    while (key != ONE && key != TWO) {
        a <- system('python getKeyPress.py', intern=TRUE)
        key <- a # substr(a, nchar(a), nchar(a))
        print(paste('Key pressed: ',key,'from',a))

        if (key == "b'8'")
            stop('Key 8 pressed')
    }
    time <- Sys.time() - time

    if ((key == ONE && correct == 1) || (key == TWO && correct == 2))
        return(list(seen=TRUE, time=time, err=NULL))
    else
        return(list(seen=FALSE, time=time, err=NULL))
}


################################################################
# Read in return value from opipresent with F310 controller.
# First param is correct, next is 1 for left button, 2 for right button
# Left button (LB) is correct for interval 1, RB for interval 2
#    correct is either 1 or 2, whichever is the correct interval
#   
# Return list(seen={TRUE|FALSE}, time=time, err=NULL))
#        seen is TRUE if correct key pressed
################################################################
F310Handler <- function(correct, opiResult) {
  z <- opiResult$seen == correct
  opiResult$seen <- z
  
  return(opiResult)
}

################################################################
# 2 example beep_function
################################################################
\dontrun{
require(beepr)
myBeep <- function(type='None') {
  if (type == 'correct') {
    beepr::beep(2)  # coin noise
    Sys.sleep(0.5)
  }
  if (type == 'incorrect') {
    beepr::beep(1) # system("rundll32 user32.dll,MessageBeep -1") # system beep
    #Sys.sleep(0.0)
  }
}

require(audio)
myBeep <- function(type="None") {
    if (type == 'correct') {
      wait(audio::play(sin(1:10000/10)))  
    }
    if (type == 'incorrect') {
      wait(audio::play(sin(1:10000/20)))
    }
}
}

################################################################
# An example stim_print function
################################################################
\dontrun{
stim_print <- function(s, ret) {
    sprintf("%4.1f %2.0f",cdTodb(s$level,10000/pi), ret$seen)
}
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ methods }
