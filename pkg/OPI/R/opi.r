#
# Choose which OPI implementation to run and distribute calls accordingly.
#
# This would all have been nicer in an OO style, with each implementation
# being a subclass of an opi class, but I don't think it can be in R.
# The OPI standard doesn't want users employing exactly the same function 
# no matter what the underlying implementation, and so there cannot be 
# extra parameters to create method signatures for different classes.
# Similarly, some implementations use exactly the same method signatures,
# again which will confuse R, I think. Anyway, if I am wrong, sorry about that.
# What I've done (use a list of implementations and then use a global
# integer to index them) works and makes sense to the non-OO person.
#
# Author: Andrew Turpin    (aturpin@unimelb.edu.au)
# Date: June 2012
# Modified:    Sep 2014 - added Octopus 600
#           16 Dec 2014 - added Kowa AP 7000
#           25 Feb 2016 - added imo
#           22 Jul 2016 - added Compass
#
# Copyright 2012 Andrew Turpin and Jonathan Denniss
# This program is part of the OPI (http://perimetry.org/OPI).
# OPI is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

packageStartupMessage("OPI version 2.9.1")

#' @export
.OpiEnv <- new.env(size=20)

################################################################################
# A list of available OPI implementations for chooseOpi to choose from, and 
# the opi* functions to index using .OpiEnv$chooser.
################################################################################
opi.implementations <- list(
    list(
        name="Compass",
        opiInitialize    = "compass.opiInitialize",
        opiClose         = "compass.opiClose",
        opiSetBackground = "compass.opiSetBackground",
        opiQueryDevice   = "compass.opiQueryDevice",
        opiPresent       = "compass.opiPresent"
    ),
    list(
        name="imo",
        opiInitialize    = "imo.opiInitialize",
        opiClose         = "imo.opiClose",
        opiSetBackground = "imo.opiSetBackground",
        opiQueryDevice   = "imo.opiQueryDevice",
        opiPresent       = "imo.opiPresent"
    ),
    list(
        name="KowaAP7000",
        opiInitialize    = "kowaAP7000.opiInitialize",
        opiClose         = "kowaAP7000.opiClose",
        opiSetBackground = "kowaAP7000.opiSetBackground",
        opiQueryDevice   = "kowaAP7000.opiQueryDevice",
        opiPresent       = "kowaAP7000.opiPresent"
    ),
    list(
        name="Octopus900",
        opiInitialize    = "octo900.opiInitialize",
        opiClose         = "octo900.opiClose",
        opiSetBackground = "octo900.opiSetBackground",
        opiQueryDevice   = "octo900.opiQueryDevice",
        opiPresent       = "octo900.opiPresent"
    ),
    list(
        name="Octopus900F310",
        opiInitialize    = "octo900.opiInitialize",
        opiClose         = "octo900.opiClose",
        opiSetBackground = "octo900.opiSetBackground",
        opiQueryDevice   = "octo900.opiQueryDevice",
        opiPresent       = "octo900.opiPresentF310"
    ),
    list(
        name="Octopus600",
        opiInitialize    = "octo600.opiInitialize",
        opiClose         = "octo600.opiClose",
        opiSetBackground = "octo600.opiSetBackground",
        opiQueryDevice   = "octo600.opiQueryDevice",
        opiPresent       = "octo600.opiPresent"
    ),
    list(
        name="SimNo",
        opiInitialize    = "simNo.opiInitialize",
        opiClose         = "simNo.opiClose",
        opiSetBackground = "simNo.opiSetBackground",
        opiQueryDevice   = "simNo.opiQueryDevice",
        opiPresent       = "simNo.opiPresent"
    ),
    list(
        name="SimYes",
        opiInitialize    = "simYes.opiInitialize",
        opiClose         = "simYes.opiClose",
        opiSetBackground = "simYes.opiSetBackground",
        opiQueryDevice   = "simYes.opiQueryDevice",
        opiPresent       = "simYes.opiPresent"
    ),
    list(
        name="SimHenson",
        opiInitialize    = "simH.opiInitialize",
        opiClose         = "simH.opiClose",
        opiSetBackground = "simH.opiSetBackground",
        opiQueryDevice   = "simH.opiQueryDevice",
        opiPresent       = "simH.opiPresent"
    ),
    list(
        name="SimGaussian",
        opiInitialize    = "simG.opiInitialize",
        opiClose         = "simG.opiClose",
        opiSetBackground = "simG.opiSetBackground",
        opiQueryDevice   = "simG.opiQueryDevice",
        opiPresent       = "simG.opiPresent"
    ),
    list(
        name="SimHensonRT",
        opiInitialize    = "simH_RT.opiInitialize",
        opiClose         = "simH_RT.opiClose",
        opiSetBackground = "simH_RT.opiSetBackground",
        opiQueryDevice   = "simH_RT.opiQueryDevice",
        opiPresent       = "simH_RT.opiPresent"
    ),
    list(
        name="Daydream",
        opiInitialize    = "daydream.opiInitialize",
        opiClose         = "daydream.opiClose",
        opiSetBackground = "daydream.opiSetBackground",
        opiQueryDevice   = "daydream.opiQueryDevice",
        opiPresent       = "daydream.opiPresent"
    )
)

################################################################################
# Input parameters
#   opiImplementation  Either "Octopus900", "HEP", "SimHenson", "SimGaussian"
#                      If NULL, prints a list of possible values. Returns TRUE.
# Side effect
#   Sets .OpiEnv$chooser
#
# Returns
#   TRUE     If successful
#   FALSE    Otherwise
################################################################################
#' @rdname chooseOpi
#' @title Choose an implementation of the OPI
#' @description Chooses an implementation of the OPI to use
#' @param opiImplementation A character string that is one of the following.
#' \itemize{
#'   \item\code{"SimNo"} for a simulator that always doesn't see.
#'   \item\code{"SimYes"} for a simulator that always does see.
#'   \item\code{"SimHenson"} for a simulator that uses a cummulative gaussian
#'   psychometric function with standard deviation according to Henson et al (2000)
#'   where variability increases as true threshold (Humphrey dB) value decreases.
#'   \item\code{"SimHensonRT"} as for SimHenson, but response times in ms are sampled
#'   from a supplied response time data set for each true positive response.
#'   \item\code{"SimGaussian"} for a simulator that uses a cummulative gaussian
#'   psychometric function with standard deviation supplied in opiInitialize().
#'   \item\code{"Octopus900"} for interfacing with the Octopus 900.
#'   \item\code{"Octopus900F310"} for interfacing with the Octopus 900 using Logitech
#'   F310 controller.
#'   \item\code{"Octopus600"} for interfacing with the Octopus 600.
#'   \item\code{"HEP"}        not working so well in new HEPs.
#'   \item\code{"KowaAP7000"} for interfacing with Kowa AP-7000.
#'   \item\code{NULL}         print a list of available OPI implementations.
#' }
#' @return Returns TRUE if successful, FALSE otherwise.
#' @examples
#' if(!chooseOpi("SimHenson"))
#'   warnings()
#' @references
#' David B. Henson, Shaila Chaudry, Paul H. Artes, E. Brian Faragher, and Alec Ansons.
#' Response Variability in the Visual Field: Comparison of Optic Neuritis, Glaucoma,
#' Ocular Hypertension, and Normal Eyes. Investigative Ophthalmology & Visual Science,
#' February 2000, Vol. 41(2).
#' 
#' A.M. McKendrick, J. Denniss and A. Turpin. "Response times across the visual field:
#' empirical observations and application to threshold determination". Vision Research,
#' 101, 2014.
#' 
#' A. Turpin, P.H. Artes and A.M. McKendrick. "The Open Perimetry Interface: An
#' enabling tool for clinical visual psychophysics", Journal of Vision 12(11) 2012.
#' @export
chooseOpi <- function(opiImplementation) { 
    possible <- unlist(lapply(opi.implementations, "[", "name"), use.names = FALSE)

        #
        # If NULL, print the list of possible
        #
    if (missing(opiImplementation))
        opiImplementation <- NULL
    if (is.null(opiImplementation)) {
        return(possible)
    }

        #
        # Warn about the one unimplemented one
        #
    if (opiImplementation == "HEP") {
        # require(rHEP)
        warning("Have not implemented chooseOPI(HEP)")
        return(FALSE)
    } 

        #
        # Find the index in opi.implementations
        #
    i <- which(opiImplementation == possible)
    if (length(i) == 0) {
        assign("chooser", NA, envir=.OpiEnv)
        warning(paste("chooseOpi() cannot find opiImplementation",opiImplementation))
        return(FALSE)
    } else {
        assign("chooser", i, envir=.OpiEnv)
        return(TRUE)
    }
}

#' @rdname chooseOpi
#' @export
chooseOPI <- chooseOpi

####################################################################################
# Simply send the opi*() call to the right implementation
####################################################################################
opiDistributor <- function(method, ...) {
    if (!exists("chooser", where=.OpiEnv) || is.na(.OpiEnv$chooser)) {
        msg <- "You have not chosen a valid OPI implementation. Use chooseOpi()"
        warning(msg)
        return(msg)
    }
    toCall <- opi.implementations[[.OpiEnv$chooser]][[method]]
    allowedArgs <- names(formals(toCall))
    haveArgs    <- names(list(...))
#print(paste("Allowed args: ", allowedArgs))
#print(paste("Have args: ", haveArgs))
    argsToPass  <- intersect(allowedArgs, haveArgs)
    argsNotPassed  <- setdiff(haveArgs, argsToPass)

    if (length(argsNotPassed) > 0)
        warning(paste(method, "Ignored argument ", argsNotPassed, "\n"))
#print(paste("Passing args: ", argsToPass))
    result <- do.call(toCall, list(...)[argsToPass])

    return(result)
}

#' @rdname opiPresent
#' @title Use OPI to present stimulus
#' @description Generic function for presentation of stimulus stim. Depending on
#' your choice of OPI implementation set using \code{chooseOpi()}, different
#' parameters are available for \code{opiPresent}
#' @param stim A list of class \code{\link{opiStaticStimulus}},
#' \code{\link{opiKineticStimulus}}, or \code{\link{opiTemporalStimulus}}
#' @param nextStim As for \code{stim}, but the next presentation to be made. This
#' might be useful on some machines, particularly projector based systems,
#' where preparations  for the next presentation can be made while waiting for a
#' response to the current
#' @param ... Parameters specific to your chosen opi implementation
#' @details \code{\link{opiPresent}} is blocking in that it will not return
#' until either a response is obtained, or at least the responseWindow
#' milliseconds has expired. (Note that more time might have expired.)
#' Specifying \code{nextStim} allows the implementing machine to use the time
#' waiting for a response to \code{stim} to make preparations for the next
#' stimuli. (For example retargeting the projector or moving aperture and/or
#' filter wheels.) There is no guarantee that the next call to
#' \code{\link{opiPresent}} will have \code{nextStim} as the first argument;
#' this could be checked by the machine specific implementations (but currently
#' is not, I think).
#' 
#' Also note that to allow for different parameters depending on the
#' implementation chosen with \code{chooseOpi}, every parameter MUST be named in
#' a call to \code{\link{opiPresent}}.
#' \subsection{SimHenson}{
#'   \code{opiPresent(stim, nextStim=NULL, fpr=0.03, fnr=0.01, tt=30)}
#'   
#'   If the chosen OPI implementation is \code{SimHenson}, then the response to a
#'   stimuli is determined by sampling from a Frequency-of-Seeing (FoS) curve
#'   (also known as the psychometric function) with formula
#'   \deqn{\mbox{fpr}+(1-\mbox{fpr}-\mbox{fnr})(1-\mbox{pnorm}(x, \mbox{tt}},
#'   where \eqn{x}{\code{x}} is the stimulus value in Humphrey dB, and pxVar is
#'   \deqn{\min\left(\mbox{simH.global.cap}, e^{A\times\mbox{tt}+B}\right).}{\code{min(simH.cap, exp(A*tt + B))}.}
#'   The ceiling \code{simH.global.cap} is set with the call to
#'   \code{opiInitialize}, and \code{A} and \code{B} are from Table 1 in Henson
#'   et al (2000). Which values are used is determined by \code{simH.type} which
#'   is also set in the call to \code{opiInitialize}.
#'   
#'   Note that if the stimulus value is less than zero, then the Henson formula
#'   is not used. The probability of seeing is \code{fpr}.
#'   
#'   \code{opiPresent(stim, nextStim=NULL, fpr=0.03, fnr=0.01, tt=NULL,
#'                    criteria=0.95, rt_shape=5.3, rt_rate=1.4, rt_scale=0.1)}
#'
#'   For determinng seen/not-seen for kinetic, the first location (to a fidelity
#'   of 0.01 degrees) on the path (it only works for single paths now) where the
#'   probability of seeing is equal to \code{criteria} is found. If no such
#'   location exists, then the stimuli is not seen. The probability of seeing at
#'   each location is determined using a frequency-of-seeing curve defined as a
#'   cumulative Gaussian with parameters controlled by \code{tt} and
#'   \code{opiInitialize}. At each location along the path, the mean of the FoS
#'   is taken from the \code{tt} function, which takes a distance-along-path
#'   (in degrees) as an argument, and returns a dB value which is the static
#'   threshold at that distance along the path.
#'   
#'   Function \code{tt} can return NA for not thresholds that are always not
#'   seen. At each location along the path, the standard deviation of the FoS
#'   is sampled from a Gaussion with mean taken from the formula of Henson et
#'   al (2000), as parametrised by \code{opiInitialize}, and standard deviation
#'   0.25.
#'   
#'   The location of a false positive response (for the total kinetic path) is
#'   sampled uniformly from the start of the path to the 'seeing' location, or
#'   the entire path if the stimuli is not seen.
#'   
#'   Note that the false positive rate \code{fpr} and the false negative rate
#'   \code{fnr} are specified for the whole path, and not for the individual
#'   static responses along the way.
#'   
#'   The actual location returned for a seen response is the location where the
#'   probability of seeing equals \code{criteria}, plus a response time sampled
#'   from a Gamma distribution parameterised by \code{rt_shape} and \code{rt_rate}
#'   and multiplied by \code{rt_scale}.That is:
#'   \code{rgamma(1, shape=rt_shape, rate=rt_rate) / rt_scale}.
#' }
#' \subsection{SimHensonRT}{
#'   \code{opiPresent(stim, nextStim=NULL, fpr=0.03, fnr=0.01, tt=30, dist=stim$level - tt)}
#' 
#'   For static stimuli, this function is the same as for \code{SimHenson}, but
#'   reaction times are determined by sampling from \code{rtData} as passed to
#'   \code{opiInitialize}.  The \code{dist} parameter is the distance of the
#'   stimulus level from the true threshold, and should be in the same units as
#'   the \code{Dist} column of \code{rtData}. The default is just the straight
#'   difference between the stimulus level and the true threshold, but you might
#'   want it scaled somehow to match \code{rtData}.
#' }
#' \subsection{SimGaussian}{
#'   \code{opiPresent(stim, nextStim=NULL, fpr=0.03, fnr=0.01, tt=30)}
#'   
#'   If the chosen OPI implementation is \code{SimGaussian}, then the response
#'   to a stimuli is determined by sampling from a Frequency-of-Seeing (FoS)
#'   curve (also known as the psychometric function) with formula
#'   \code{fpr+(1-fpr-fnr)*(1-pnorm(x, tt, simG.global.sd))}, where \code{x}
#'   is the stimulus value in Humphrey dB, and \code{simG.global.sd} is
#'   set with \code{opiInitialize}.
#' }
#' \subsection{SimYes}{
#'   \code{opiPresent(stim, nextStim=NULL)}
#'   
#'   If the chosen OPI implementation is \code{SimYes}, then the response to a
#'   stimuli is always yes, hence \code{\link{opiPresent}} always returns
#'   \code{err=NULL}, \code{seen=TRUE}, and \code{time=0}.
#' }
#' \subsection{SimNo}{
#' 
#'   \code{opiPresent(stim, nextStim=NULL)}
#' 
#'   If the chosen OPI implementation is \code{SimNo}, then the response to a
#'   stimuli is always no, hence \code{\link{opiPresent}} always returns
#'   \code{err=NULL}, \code{seen=FALSE}, and \code{time=0}.
#' }
#' \subsection{Octopus900F310}{
#'   \code{opiPresent(stim, nextStim=NULL)}
#'   
#'   This functions as for the Octopus900, but responses are taken from the F310
#'   Controller.
#'   
#'   If the L button is pressed, \code{seen} is set to 1.
#'   
#'   If the R button is pressed, \code{seen} is set to 2.
#'   
#'   If no button is pressed within \code{responseWindow}, then \code{seen} is set to 0.
#' }
#' \subsection{Octopus600}{
#'   \code{opiPresent(stim, nextStim=NULL)}
#'   
#'   If the chosen OPI implementation is \code{Octopus600}, then nextStim is
#'   ignored. If \code{eyeControl} is non-zero, as set in \code{opiInitialize},
#'   answer codes describing patient state may arise (see \code{answer} field
#'   in the Value section).
#' }
#' \subsection{KowaAP7000}{
#'   \code{opiPresent(stim, nextStim=NULL)}
#' 
#'   If the chosen OPI implementation is \code{KowaAP7000}, then \code{nextStim}
#'   is ignored. 
#' }
#' \subsection{Compass}{
#'   \code{opiPresent(stim, nextStim=NULL)}
#'   
#'   If the chosen OPI implementation is \code{Compass}, then \code{nextStim}
#'   is ignored. Note that the dB level is rounded to the nearest integer.
#'   
#'   If tracking is on, then this will block until the tracking is obtained,
#'   and the stimulus presented.
#' }
#' \subsection{DayDream}{
#'   If the chosen OPI implementation is \code{DayDream}, then \code{nextStim}
#'   is ignored.
#'   
#'   Note that the dB level is rounded to the nearest cd/\eqn{\mbox{m}^2}{m^2}
#'   that is in the \code{lut} specified in \code{opiInitialise}.
#'   
#'   Currently uses the most simple algorithm for drawing a 'circle'
#'   (ie not Bresenham's).
#'   
#'   Currently only implemented for \code{opiStaticStimulus}.
#' }
#' @return A list containing
#' \item{err }{\code{NULL} if no error occurred, otherwise a
#'   machine-specific error message.
#'   
#'   This should include errors when the specified size cannot be achieved by
#'   the device (for example, in a projection system with an aperture wheel of
#'   predefined sizes.) If \code{stim} is \code{NULL}, then \code{err} contains
#'   the status of the machine.}
#' \item{seen }{\code{TRUE} if a response was detected in the allowed
#'   \code{responseWindow}, \code{FALSE} otherwise. (Note, see
#'   Octopus900F310 above).}
#' \item{time}{The time in milliseconds from the onset (or offset,
#' machine-specific) of the presentation until the response from the subject
#' if \code{seen} is \code{TRUE}.
#' 
#' If \code{seen} is \code{FALSE}, this value is undefined.
#' 
#' For kinetic perimetry on the O900, this value is unknown...}
#' \item{answer}{Only returned for \code{Octopus600}. Can be the following values:
#' \itemize{
#'   \item 0 = stimulus not seen;
#'   \item 1 = stimulus seen;
#'   \item 132 = Response button was pressed before stimulus presentation
#'     (Patient needs a break - hold on examination);
#'   \item 36 = Eye is closed before stimulus presentation;
#'   \item 68 = Fixation lost before stimulus presentation (pupil center is
#'     out of green window in video image);
#'   \item 260 = Forehead rest lost before stimulus presentation;
#'   \item 516 = Fast Eye movements before stimulus presentation;
#'   \item 258 = Forehead rest lost during stimulus presentation;
#'   \item 66 = Fixation lost during stimulus presentation (pupil center is
#'     out of green window in video image);
#'   \item 34 = Eye was closed during stimulus presentation;
#'   \item 18 = Patient answer was too early (<=100ms after stimulus
#'     presentation) - lucky punch;
#'   \item 514 = Fast Eye movements during stimulus presentation
#' }}
#' \item{pupilX}{Only returned for KowaAP7000 (in pixels) and an
#'   opiStaticStimulus or O900 (in degrees) and staic/kinetic if gazeFeed==1.
#'   x-coordinate of centre of pupil during presentation.}
#' \item{pupilY}{Only returned for KowaAP7000 (in pixels) and an
#'   opiStaticStimulus or O900 (in degrees) and static/kinetic if gazeFeed==1.
#'   y-coordinate of centre of pupil during presentation.}
#' \item{purkinjeX}{Only returned for KowaAP7000 and an opiStaticStimulus.
#'   x-coordinate of centre of Purkinje Image in pixels during presentation.}
#' \item{purkinjeY}{Only returned for KowaAP7000 and an opiStaticStimulus.
#'   y-coordinate of centre of Purkinje Image in pixels during presentation.}
#' \item{x}{Only returned for KowaAP7000 or Octopus900 and an
#'   opiKineticStimulus. x coordinate of stimuli when button is pressed.}
#' \item{y}{Only returned for KowaAP7000 or Octopus900 and an
#'   opiKineticStimulus. y coordinate of stimuli when button is pressed.}
#' \item{time_rec}{Only returned for Compass. Time since epoch that the opiPresent command was received by the Compass in ms.}
#' \item{time_hw}{Only returned for Compass. Hardware time of button press or
#'   response window expired (integer ms).
#' 
#'   To get the hardware time that a presentation began, subtract
#'   responseWindow from \code{th} (for aligning with fixation data returned
#'   by \code{opiClose()}.}
#' \item{time_resp}{Only returned for Compass. Time since epoch that the
#'   response was received or response window expired (in ms).}
#' \item{num_track_events}{Only returned for Compass. The number of tracking
#'   events associated with this presentation.}
#' \item{num_motor_fails}{Only returned for Compass. The number of time the
#'   motor could not keep pace with eye movements.}
#' \item{pupil_diam}{Only returned for Compass. The diameter of the pupil on
#'   milimetres on presentation.}
#' \item{loc_x}{Only returned for Compass. The x location in pixels of the
#'   presentation on the retinal image returned by \code{opiInitialize}.}
#' \item{loc_y}{Only returned for Compass. The y location in pixels of the
#'   presentation on the retinal image returned by \code{opiInitialize}.}
#' @references
#' David B. Henson, Shaila Chaudry, Paul H. Artes, E. Brian Faragher, and
#' Alec Ansons. Response Variability in the Visual Field: Comparison of Optic
#' Neuritis, Glaucoma, Ocular Hypertension, and Normal Eyes. Investigative
#' Ophthalmology & Visual Science, February 2000, Vol. 41(2).
#' @seealso \code{\link{opiStaticStimulus}}, \code{\link{opiKineticStimulus}},
#' \code{\link{opiTemporalStimulus}}, \code{\link{chooseOpi}},
#' \code{\link{opiInitialize}}
#' @examples
#' # Stimulus is Size III white-on-white as in the HFA
#' makeStim <- function(db, n) {
#'   s <- list(x=9, y=9, level=dbTocd(db, 10000/pi), size=0.43, color="white",
#'             duration=200, responseWindow=1500)
#'   class(s) <- "opiStaticStimulus"
#'   return(s)
#' }
#' 
#' chooseOpi("SimHenson")
#' if (!is.null(opiInitialize(type="C", cap=6)))
#'   stop("opiInitialize failed")
#' 
#' result <- opiPresent(stim=makeStim(10,0), tt=30, fpr=0.15, fnr=0.01)
#' 
#' # Will not work as 'stim' is not named
#' #result <- opiPresent(makeStim(10,0), tt=30, fpr=0.15, fnr=0.01)
#' 
#' if (!is.null(opiClose()))
#'   warning("opiClose() failed")
#'   
#' # Same but with simulated reaction times
#' chooseOpi("SimHensonRT")
#' data(RtSigmaUnits)
#' if (!is.null(opiInitialize(type="C", cap=6, rtData=RtSigmaUnits)))
#'   stop("opiInitialize failed")
#'
#' dist <- (10 - 30)/min(exp(-0.098 * 30 + 3.62), 6)
#' result <- opiPresent(stim=makeStim(10,0), tt=30, fpr=0.15, fnr=0.01, dist=dist)
#' 
#' if (!is.null(opiClose()))
#'   warning("opiClose() failed")
#' @export
opiPresent        <- function(stim,nextStim=NULL,...) { 
    opiDistributor("opiPresent", stim=stim, nextStim=nextStim, ...) 
}

#' @rdname opiInitialize
#' @title Initialize OPI
#' @description Generic function for initialization of the chosen OPI
#' implementation that is set with \code{chooseOpi()}
#' @param ... Implementation specific parameters. See details.
#' @details
#' \subsection{SimHenson}{
#'   \code{opiInitialize(type="C", A=NA, B=NA, cap=6, display=NULL, maxStim=10000/pi)}
#'   
#'   If the chosen OPI implementation is \code{SimHenson}, then \code{type}
#'   can be one of: \code{"N"}, for normal patients; \code{"G"}, for POAG
#'   patients; and \code{"C"}, for a combination. See Table 1 in Henson et al
#'   (2000).
#'   
#'   If \code{type} is \code{"X"} then \code{A} and \code{B} should be
#'   specified and are used in place of one of the three A/B combinations as in
#'   Henson et al (2000). \code{cap} is the maximum standard deviation value that
#'   the simulator will use for the slope/spread of the psychometric function.
#'   
#'   If \code{display} is a vector of four numbers \code{c(xlow, xhi, ylow, yhi)},
#'   then a plot area is created of dimension \code{xlim=range(xlow, xhi)} and
#'   \code{ylim=range(ylow, yhi)} and each call to \code{opiPresent} will display
#'   a point on the area. The color of the plot area can be set with
#'   \code{opiSetBackground}, and the color of the displayed point is determined
#'   by the stimulus passed to \code{opiPresent}.
#'   
#'   \code{maxStim} is the maximum stimuls value in cd/\eqn{\mbox{m}^2}{m^2}.
#'   This is used in converting cd/\eqn{\mbox{m}^2}{m^2} to dB values, and
#'   vice versa.
#' }
#' \subsection{SimHensonRT}{
#'   \code{opiInitialize(type="C", A=NA, B=NA, cap=6, display=NULL, maxStim=10000/pi, rtData, rtFP=1:1600)}
#'   
#'   If the chosen OPI implementation is \code{SimHensonRT}, then the first six
#'   parameters are as in \code{SimHenson}, and \code{rtData} is a data frame
#'   with at least 2 columns: \code{"Rt"}, reponse time; and \code{"Dist"},
#'   signifying that distance between assumed threshold and stimulus value in
#'   your units.
#'   
#'   This package contains \code{RtSigmaUnits} or \code{RtDbUnits} that can be
#'   loaded with the commands \code{data(RtSigmaUnits)} or \code{data(RtDbUnits)},
#'   and are suitable to pass as values for \code{rtData}.
#'   
#'   \code{rtFp} gives the vector of values in milliseconds from which a response
#'   time for a false positive response is randomly sampled.
#' }
#' \subsection{SimGaussian}{
#'   \code{opiInitialize(sd, display=NULL, maxStim=10000/pi)}
#'   
#'   If the chosen OPI implementation is \code{SimGaussian}, then \code{sd} is the
#'   standard deviation value that the simulator will use for the slope/spread of
#'   the psychometric function.
#'   
#'   \code{display} and \code{maxStim} is as for SimHenson.
#' }
#' \subsection{Octopus900}{
#'   \code{opiInitialize(serverPort=50001,eyeSuiteSettingsLocation, eye, gazeFeed=NA, bigWheel=FALSE,pres_buzzer=0, resp_buzzer=0, zero_dB_is_10000_asb=TRUE)}
#'   
#'   If the chosen OPI implementation is \code{Octopus900}, then you must specify
#'   a directory and the eye to be tested.
#'   
#'   \code{serverPort} is the TCP/IP port on which the server is listening (on
#'   localhost).
#'   
#'   \code{eyeSuiteSettingsLocation} is the folder name containing the EyeSuite
#'   setting files, and should include the trailing slash.
#'   
#'   \code{eye} must be either "left" or "right".
#'   
#'   \code{gazeFeed} is the name of an existing folder into which the video frames
#'   of eye tracker are recorded. Set to \code{NA} for no recording.
#'   
#'   \code{bigWheel} is \code{FALSE} for a standard Octopus 900 machine. Some
#'   research machines are fitted with an alternate aperture wheel that has 24
#'   sizes, which are accessed with \code{bigWheel} is \code{TRUE}. The mapping
#'   from size to 'hole on wheel' is hard coded; see code for details.
#'   
#'   If \code{pres_buzzer} is greater than zero, a buzzer will sound with each
#'   stimuli presented.
#'   
#'   If \code{resp_buzzer} is greater than zero, a buzzer will sound with each
#'   button press (resposne). The volume can be one of 0 (no buzzer), 1, 2, or 3
#'   (max volume). If both buzzers are more than zero, the maximum of the two will
#'   be used as the volume.
#'   
#'   If \code{zero_dB_is_10000_asb} is \code{TRUE} then 0 dB is taken as 10000
#'   apostilbs, otherwise 0 dB is taken as 4000 apostilbs.
#' }
#' \subsection{Octopus600}{
#'   \code{opiInitialize(ipAddress, eye, pupilTracking=FALSE, pulsar=FALSE, eyeControl=0)}
#'   
#'   If the chosen OPI implementation is \code{Octopus600}, then you must specify
#'   the IP address of the Octopus 600 and the eye to test.
#'   
#'   \code{ipAddress} is the IP address of the Octopus 600 as a string.
#'   
#'   \code{eye} must be either "left" or "right".
#'   
#'   \code{pupilTracking} is TRUE to turn on IR illumination and set pupil
#'   black level (which happens at the first stimulus presentation).
#'   
#'   \code{pulsar} is TRUE for pulsar stimulus, FALSE for size III white-on-white.
#'   
#'   \code{eyeControl}
#'   \itemize{ 
#'     \item 0 is off
#'     \item 1 is eye blink
#'     \item 2 is eye blink, forehead rest, fixation control
#'     \item 3 is eye blink, forehead rest, fixation control, fast eye movements
#'   }
#' }
#' \subsection{KowaAP7000}{
#'   \code{opiInitialize(ip, port)}
#'   
#'   If the chosen OPI implementation is \code{KowaAP7000}, then you must specify
#'   the IP address and port of the AP-7000 server.
#'   
#'   \itemize{
#'     \item\code{ipAddress} is the IP address of the AP-7000 server as a string.
#'     \item\code{port} is the TCP/IP port of the AP-7000 server as a number.
#'   }
#' }
#' \subsection{imo}{
#'   \code{opiInitialize(ip, port)}
#'   
#'   If the chosen OPI implementation is \code{imo}, then you must specify the IP
#'   address and port of the imo server.
#'   
#'   \itemize{
#'     \item \code{ip} is the IP address of the imo server as a string.
#'     \item \code{port} is the TCP/IP port of the imo server as a number.
#'   }
#' }
#' \subsection{Compass}{
#'   \code{opiInitialize(ip, port)}
#'   
#'   If the chosen OPI implementation is \code{Compass}, then you must specify
#'   the IP address and port of the Compass server.
#'   
#'   \itemize{
#'     \item\code{ip} is the IP address of the Compass server as a string.
#'     \item\code{port} is the TCP/IP port of the Compass server as a number.
#'   }
#'   Warning: this returns a list, not a single error code.
#' }
#' \subsection{DayDream}{
#'   \code{opiInitialize(ip="127.0.0.1", port=50008, lut= seq(0, 400, length.out = 255), ppd = 11)}
#'   
#'   If the chosen OPI implementation is \code{Daydream}, then you must specify
#'   the IP address of the Android phone that is in the Daydream, and the port on
#'   which the server running on the phone is listening.
#'   
#'   \itemize{
#'     \item\code{lut} is a vector of 256 luminance values, with \code{lut[i]} being the cd/\eqn{\mbox{m}^2}{m^2} value for grey level i. Default is \code{seq(0, 4000, length.out = 255)}
#'     \item\code{ppd} points per degree. It is a scalar to multiply to x and y coordinates to convert from degrees to pixels. This assumes the viewing distance (z-coordinate) is 30cm.
#'   }
#' }
#' @return Returns NULL if initialization succeeded, otherwise an
#' implementation-dependent error.
#'
#' \subsection{Octopus900}{
#'   Returns NULL if successful, 1 if Octopus900 is already initialised by a
#'   previous call to \code{opiInitialize}, and 2 if some error occured that
#'   prevented ininitialisation. The default background and stimulus setup is
#'   to white-on-white perimetry. Use \code{opiSetBackground} to change the
#'   background and stimulus colors.
#' }
#'
#' \subsection{Octopus600}{
#'   Returns NULL if successful, or an Octopus 600 error code. The default
#'   background and stimulus setup is to white-on-white perimetry.
#' }
#' 
#' \subsection{Kowa AP-7000}{
#'   Always returns NULL.
#' }
#'
#' \subsection{imo}{
#'   Always returns NULL. Will \code{stop} if there is an error.
#' }
#'
#' \subsection{Compass}{
#'   Returns a list with elements:
#'   \itemize{
#'     \item{err} NULL if successful, not otherwise.
#'     \item{prl} a pair giving the (x,y) in degrees of the Preferred Retinal
#'       Locus detected in the initial alignment.
#'     \item{onh} a pair giving the (x,y) in degrees of the ONH as selected by
#'     the user.
#'     \item{image} raw bytes being the JPEG compressed infra-red image acquired
#'     during alignment.
#'   }
#' }
#' @references
#' David B. Henson, Shaila Chaudry, Paul H. Artes, E. Brian Faragher, and Alec
#' Ansons. Response Variability in the Visual Field: Comparison of Optic Neuritis,
#' Glaucoma, Ocular Hypertension, and Normal Eyes. Investigative Ophthalmology &
#' Visual Science, February 2000, Vol. 41(2).
#' @seealso \code{\link{chooseOpi}}, \code{\link{opiSetBackground}},
#' \code{\link{opiClose}}, \code{\link{opiPresent}}
#' @examples
#' # Set up a simple simulation for white-on-white perimetry
#' chooseOpi("SimHenson")
#' if (!is.null(opiInitialize(type="C", cap=6)))
#'   stop("opiInitialize failed")
#'
#' # Set up a simple simulation for white-on-white perimetry
#' # and display the stimuli in a plot region
#' chooseOpi("SimHenson")
#' if (!is.null(opiInitialize(type="C", cap=6, display=c(-30,30,-30,30))))
#'   stop("opiInitialize failed")
#'
#' # Set up a simple simulation for white-on-white perimetry
#' # and display the stimuli in a plot region and simulate response times
#' chooseOpi("SimHensonRT")
#' data(RtSigmaUnits)
#' oi <- opiInitialize(type="C", cap=6,
#'                     display=c(-30,30,-30,30), rtData=RtSigmaUnits, rtFP=1:100)
#' if (!is.null(oi))
#'   stop("opiInitialize failed")
#'
#' # Set up a simulation using a psychometric function that is
#' # a cumulative gaussian of standard deviation 2
#' chooseOpi("SimGaussian")
#' if (!is.null(opiInitialize(sd=2)))
#'   stop("opiInitialize failed")
#'
#' \dontrun{
#'   # Set up the Octopus 900
#'   chooseOpi("Octopus900")
#'   if (!is.null(opiInitialize(
#'        eyeSuiteSettingsLocation="C:/ProgramData/Haag-Streit/EyeSuite/",
#'        eye="left")))
#'     stop("opiInitialize failed")
#' }
#' \dontrun{
#'   # Set up the Kowa AP-7000
#'   chooseOpi("KowaAP7000")
#'   opiInitialize(ip="192.168.1.7", port=44965)
#' }
#' 
#' \dontrun{
#'   # Set up the imo
#'   chooseOpi("imo")
#'   opiInitialize(ip="192.168.1.7", port=44965)
#' }
#'
#' \dontrun{
#'   # Set up the imo
#'   chooseOpi("Compass")
#'   result <- opiInitialize(ip="192.168.1.7", port=44965)
#'   if (is.null(result$err))
#'     print(result$prl)
#' }
#' @export
opiInitialize     <- function(...) { opiDistributor("opiInitialize", ...) }

#' @rdname opiInitialize
#' @export
opiInitialise     <- function(...) { opiDistributor("opiInitialize", ...) }

#' @rdname opiSetBackground
#' @title Set background using OPI
#' @description Generic function for setting background of the chosen OPI
#' implementation that is set with \code{chooseOpi()}
#' @param ... Implementation specific parameters. See details.
#' @details
#' \subsection{Octopus900}{
#'   \code{opiSetBackground(lum=NA, color=NA, fixation=NA, fixIntensity=NA)} 
#' 
#'   Allowable \code{lum} and \code{color} are defined in the .OpiEnv environment.
#' 
#'   \itemize{
#'     \item\code{lum} is intensity of the background and can be one of
#'     \itemize{
#'       \item \code{.OpiEnv$O900$BG_OFF}, which turns background off.
#'       \item \code{.OpiEnv$O900$BG_1}, background of 1.27 cd/\eqn{\mbox{m}^2}{m^2}.
#'       \item \code{.OpiEnv$O900$BG_10}, background of 10 cd/\eqn{\mbox{m}^2}{m^2}.
#'       \item \code{.OpiEnv$O900$BG_100}, background of 100 cd/\eqn{\mbox{m}^2}{m^2}.
#'     }
#'     \item\code{color} can be one of the following choices.
#'     \itemize{
#'       \item\code{.OpiEnv$O900$MET_COL_WW} for white-on-white
#'       \item\code{.OpiEnv$O900$MET_COL_RW} for red-on-white
#'       \item\code{.OpiEnv$O900$MET_COL_BW} for blue-on-white
#'       \item\code{.OpiEnv$O900$MET_COL_WY} for white-on-yellow
#'       \item\code{.OpiEnv$O900$MET_COL_RY} for red-on-yellow
#'       \item\code{.OpiEnv$O900$MET_COL_BY} for blue-on-yellow
#'     }
#'     \item\code{fixation} is one of 
#'     \itemize{
#'       \item\code{.OpiEnv$O900$FIX_CENTRE} or \code{.OpiEnv$O900$FIX_CENTER}
#'       \item\code{.OpiEnv$O900$FIX_CROSS}
#'       \item\code{.OpiEnv$O900$FIX_RING}
#'     }
#'     \item\code{fixIntensity} is a percentage between 0 and 100. 0 is off, 100
#'       the brightest.
#'   }
#'   Note if you specify \code{fixation} you also have to specify \code{fixIntensity}.
#' }
#' \subsection{SimHenson and SimGaussian}{ 
#'   \code{opiSetBackground(col, gridCol)}
#'   
#'   \code{col} is the background color of the plot area used for displaying
#'   stimuli, and \code{gridCol} the color of the gridlines. Note the plot area
#'   will only be displayed if \code{opiInitialize} is called with a valid display
#'   argument.
#' }
#' \subsection{Octopus600}{ 
#'   This function has no effect.
#' }
#' \subsection{KowaAP7000}{ 
#'   \code{opiSetBackground(lum, color, fixation)} 
#' 
#'   \code{lum} and \code{color} are dependant for the Kowa AP-7000. A white
#'   background must be 10 cd/\eqn{\mbox{m}^2}{m^2}, and a yellow background must
#'   be 100 cd/\eqn{\mbox{m}^2}{m^2}.
#' 
#'   If \code{lum} is 10 and \code{color} is not set, then
#'   \code{.OpiEnv$KowaAP7000$BACKGROUND_WHITE} is assumed.
#'   
#'   If \code{lum} is 100 and \code{color} is not set,
#'   then \code{.OpiEnv$KowaAP7000$BACKGROUND_YELLOW} is assumed.
#'   
#'   If both \code{lum} and \code{color} is set, then \code{lum} is ignored
#'   (a warning will be generated
#'   
#'   if \code{lum} is incompatible with \code{color}).
#'   
#'   \code{fixation} is one of
#'   \itemize{
#'     \item \code{.OpiEnv$KowaAP7000$FIX_CENTER}, fixation marker in the centre.
#'     \item \code{.OpiEnv$KowaAP7000$FIX_CENTRE}, fixation marker in the centre.
#'     \item \code{.OpiEnv$KowaAP7000$FIX_AUX},    fixation marker is ???.
#'     \item \code{.OpiEnv$KowaAP7000$FIX_MACULA}, fixation marker is a circle(?).
#'     \item \code{.OpiEnv$KowaAP7000$FIX_AUX_LEFT}, fixation marker is as for AUX
#'       but only lower left.
#'   }
#' }
#' \subsection{Compass}{
#'   \code{opiSetBackground(fixation=NA, tracking_on=NA)}
#'   \itemize{
#'     \item{\code{fixation}=c(x,y,t)} where
#'     \itemize{
#'       \item{\code{x}} is one of -20, -6, -3, 0, 3, 6, 20 degrees.
#'       \item{\code{y}} is 0 degrees.
#'       \item{\code{t}} is 0 for a spot fixation marker at \code{c(x,y)}, or 1 for a
#'         square centred on one of \code{(-3,0)}, \code{(0,0)}, \code{(+3,0)}.
#'     }
#'     \item{\code{tracking_on}} is either 0 (tracking off) or 1 (tracking on).
#'   }
#'   Note: tracking will be relative to the PRL established with the fixation
#'   marker used at setup (call to OPI-OPEN), so when tracking is on you should
#'   use the same fixation location as in the setup.
#' }
#' \subsection{Daydream}{
#'   \code{opiSetBackground(lum=NA, color=NA, fixation="Cross", fixation_size=11, fixation_color=c(0,128,0), eye="L")}
#'   \itemize{
#'     \item{\code{lum}} in cd/\eqn{\mbox{m}^2}{m^2} is set to nearest grey value in \code{lut} from \code{opiInitialize}.
#'     \item{\code{color}} is ignored.
#'     \item{\code{fixation}} can only be \code{'Cross'} at the moment.
#'     \item{\code{fixation_size}} is the number of pixels one cross-hair is in
#'       length.
#'     \item{\code{fixation_color}} RGB value of coor of fixation cross. Values
#'       in range [0,255].
#'   }
#' }
#' @return Returns NULL if succeeded, otherwise an implementation-dependent
#' error as follows.
#' \subsection{Octopus900}{ 
#'   -1 indicates \code{opiInitialize} has not been called.
#'   
#'   -2 indicates could not set the background color.
#'   
#'   -3 indicates could not set the fixation marker.
#'   
#'   -4 indicates that all input parameters were NA.
#' }
#' @seealso \code{\link{chooseOpi}}
#' @examples
#' chooseOpi("SimGaussian")
#' if (!is.null(opiInitialize(sd=2, display=c(-30,30,-30,30))))
#'   stop("opiInitialize failed")
#' if (!is.null(opiSetBackground(col="white",gridCol="grey")))
#'   stop("opiSetBackground failed, which is very surprising!")
#'
#' \dontrun{
#'   chooseOpi("Octopus900")
#'   oi <- opiInitialize(eyeSuiteJarLocation="c:/EyeSuite/",
#'                       eyeSuiteSettingsLocation="c:/Documents and Settings/All Users/Haag-Streit/",
#'                       eye="left")
#'   if(!is.null(oi))
#'     stop("opiInitialize failed")
#'   if(!is.null(opiSetBackground(fixation=.OpiEnv$O900$FIX_CENTRE)))
#'     stop("opiSetBackground failed")
#'   if(!is.null(opiSetBackground(fixation=.OpiEnv$O900$FIX_RING, fixIntensity=0)))
#'     stop("opiSetBackground failed")
#'   if(!is.null(opiSetBackground(color=.OpiEnv$O900$MET_COL_BY)))
#'     stop("opiSetBackground failed")
#'   if(!is.null(opiSetBackground(lum=.OpiEnv$O900$BG_100, color=.OpiEnv$O900$MET_COL_RW)))
#'     stop("opiSetBackground failed")
#'   opiClose()
#' }
#' @export
opiSetBackground  <- function(...) { opiDistributor("opiSetBackground", ...) }

#' @rdname opiQueryDevice
#' @title Query device using OPI
#' @description Generic function for getting details of the chosen OPI
#' implementation that is set with \code{chooseOpi()}
#' @param ... Implementation specific parameters. See details.
#' @details
#' \subsection{Octopus600}{
#'   If the chosen OPI is \code{Octopus600}, then this function returns
#'   information about the patient. See the Value section for details.
#' }
#' \subsection{KowaAP7000}{
#'   If the chosen OPI is \code{KowaAP7000}, then this function returns the current
#'   location of the pupil. See the Value section for details.
#' }
#' \subsection{Daydream}{
#'   Returns all constants in \code{.OpiEnv$DayDream} as a list.
#' }
#' @seealso \code{\link{chooseOpi}}
#' @examples
#' chooseOpi("SimGaussian")
#' if (!is.null(opiInitialize(sd=2)))
#'   stop("opiInitialize failed")
#' print(opiQueryDevice())
#' @return Returns a list that contains \code{isSim} and implementation-dependent
#' data.
#'
#' \code{isSim} is \code{TRUE} if the device is a simulation, or \code{FALSE} if
#' the device is a physical machine.
#' \subsection{Octopus600}{
#'   Returns a list of 10 items:
#'   \enumerate{
#'     \item answerButton [0 = not pressed, 1 = pressed ]
#'     \item headSensor [0 = no forehead detected, 1 = forehead detected ]
#'     \item eyeLidClosureLeft [0 = eye is open, 1 = eye is closed ]
#'     \item eyeLidClosureRight [0 = eye is open, 1 = eye is closed ]
#'     \item fixationLostLeft [1 = eye pos lost, 0 = eye pos ok)
#'     \item fixationLostRight [1 = eye pos lost, 0 = eye pos ok)
#'     \item pupilPositionXLeft [in px]
#'     \item pupilPositionYLeft [in px]
#'     \item pupilPositionXRight [in px]
#'     \item pupilPositionYRight [in px]
#'   }
#' }
#' \subsection{KowaAP7000}{
#'   Returns a list of 4 items:
#'   \itemize{
#'     \item \code{pupilX}, the x-coordinate of the pupil position in pixels.
#'     \item \code{pupilY}, the y-coordinate of the pupil position in pixels.
#'     \item \code{purkinjeX}, the x-coordinate of the purkinje position in pixels.
#'     \item \code{purkinjeY}, the y-coordinate of the purkinje position in pixels.
#'   }
#'   It also prints a list of constants that OPI knows about for the AP-7000.
#' }
#' @export
opiQueryDevice    <- function(...) { opiDistributor("opiQueryDevice", ...) }

#' @rdname opiClose
#' @title Close using OPI
#' @description Generic function for closing the chosen OPI implementation that is set
#' with \code{chooseOpi()}
#' @param ... Implementation specific parameters. See details.
#' @return Returns NULL if close succeeded, otherwise an implementation-dependent
#' error.
#' \subsection{Compass}{
#'   Returns a list of \code{err}, which is an error code, and \code{fixations},
#'   which is a matrix with three columns: time (same as \code{time_hw}
#'   in \code{opiPresent}), x (degrees relative to the centre of the image
#'   returned by \code{opiInitialise} - not the PRL), y (as for x), and one row
#'   per fixation.
#' }
#' @seealso \code{\link{chooseOpi}}
#' @examples
#' chooseOpi("SimGaussian")
#' if (!is.null(opiInitialize(sd=2)))
#'   stop("opiInitialize failed")
#' if (!is.null(opiClose()))
#'   stop("opiClose failed, which is very surprising!")
#' @export
opiClose <- function(...) { opiDistributor("opiClose", ...) }

#' @rdname opiGetParams
#' @title Get OPI method parameters
#' @description Get parameters of OPI functions which depends on the
#' implementation set with \code{chooseOPI()}
#' @param method Method for which to get parameters and defaults.
#' @param ... Implementation specific parameters. See details.
#' @return Returns a list of parameters and their default vlues of the
#' method \code{method} depending on the OPI implementation selected with
#' \code{chooseOPI()}.
#' @examples
#' chooseOpi("SimHenson")
#' opiGetParams("opiInitialize")
#' opiGetParams("opiPresent")
#' @export
opiGetParams <- function(method, ...)
  return(as.list(formals(opi.implementations[[.OpiEnv$chooser]][[method]])))