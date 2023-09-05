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
# Author: Andrew Turpin    (andrew.turpin@lei.org.au)
# Date: June 2012
# Modified:    Sep 2014 - added Octopus 600
#           16 Dec 2014 - added Kowa AP 7000
#           25 Feb 2016 - added imo
#           22 Jul 2016 - added Compass
#
# Copyright [2012] [Andrew Turpin and Jonathan Denniss]
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

packageStartupMessage("OPI version 2.11.0")

#' @export
.OpiEnv <- new.env(size = 20)
assign("chooser", NA, envir = .OpiEnv)

################################################################################
# A list of available OPI implementations for chooseOpi to choose from, and
# the opi* functions to index using .OpiEnv$chooser.
################################################################################
#' @rdname opiDistributor
#' @format \code{opi.implementations} is a \code{list} containing a catalog of
#' all specific methods that are dependent on the OPI implementation selected
#' with chooseOpi
#' @export
opi.implementations <- list(
    list(
        name             = "Compass",
        opiInitialize    = "compass.opiInitialize",
        opiClose         = "compass.opiClose",
        opiSetBackground = "compass.opiSetBackground",
        opiQueryDevice   = "compass.opiQueryDevice",
        opiPresent       = "compass.opiPresent"
    ),
    list(
        name             = "imo",
        opiInitialize    = "imo.opiInitialize",
        opiClose         = "imo.opiClose",
        opiSetBackground = "imo.opiSetBackground",
        opiQueryDevice   = "imo.opiQueryDevice",
        opiPresent       = "imo.opiPresent"
    ),
    list(
        name             = "KowaAP7000",
        opiInitialize    = "kowaAP7000.opiInitialize",
        opiClose         = "kowaAP7000.opiClose",
        opiSetBackground = "kowaAP7000.opiSetBackground",
        opiQueryDevice   = "kowaAP7000.opiQueryDevice",
        opiPresent       = "kowaAP7000.opiPresent"
    ),
    list(
        name             = "Octopus900",
        opiInitialize    = "octo900.opiInitialize",
        opiClose         = "octo900.opiClose",
        opiSetBackground = "octo900.opiSetBackground",
        opiQueryDevice   = "octo900.opiQueryDevice",
        opiPresent       = "octo900.opiPresent"
    ),
    list(
        name             = "Octopus900F310",
        opiInitialize    = "octo900.opiInitialize",
        opiClose         = "octo900.opiClose",
        opiSetBackground = "octo900.opiSetBackground",
        opiQueryDevice   = "octo900.opiQueryDevice",
        opiPresent       = "octo900.opiPresentF310"
    ),
    list(
        name             = "Octopus600",
        opiInitialize    = "octo600.opiInitialize",
        opiClose         = "octo600.opiClose",
        opiSetBackground = "octo600.opiSetBackground",
        opiQueryDevice   = "octo600.opiQueryDevice",
        opiPresent       = "octo600.opiPresent"
    ),
    list(
        name             = "SimNo",
        opiInitialize    = "simNo.opiInitialize",
        opiClose         = "simNo.opiClose",
        opiSetBackground = "simNo.opiSetBackground",
        opiQueryDevice   = "simNo.opiQueryDevice",
        opiPresent       = "simNo.opiPresent"
    ),
    list(
        name             = "SimYes",
        opiInitialize    = "simYes.opiInitialize",
        opiClose         = "simYes.opiClose",
        opiSetBackground = "simYes.opiSetBackground",
        opiQueryDevice   = "simYes.opiQueryDevice",
        opiPresent       = "simYes.opiPresent"
    ),
    list(
        name             = "SimHenson",
        opiInitialize    = "simH.opiInitialize",
        opiClose         = "simH.opiClose",
        opiSetBackground = "simH.opiSetBackground",
        opiQueryDevice   = "simH.opiQueryDevice",
        opiPresent       = "simH.opiPresent"
    ),
    list(
        name             = "SimGaussian",
        opiInitialize    = "simG.opiInitialize",
        opiClose         = "simG.opiClose",
        opiSetBackground = "simG.opiSetBackground",
        opiQueryDevice   = "simG.opiQueryDevice",
        opiPresent       = "simG.opiPresent"
    ),
    list(
        name             = "SimHensonRT",
        opiInitialize    = "simH_RT.opiInitialize",
        opiClose         = "simH_RT.opiClose",
        opiSetBackground = "simH_RT.opiSetBackground",
        opiQueryDevice   = "simH_RT.opiQueryDevice",
        opiPresent       = "simH_RT.opiPresent"
    ),
    list(
        name             = "Daydream",
        opiInitialize    = "daydream.opiInitialize",
        opiClose         = "daydream.opiClose",
        opiSetBackground = "daydream.opiSetBackground",
        opiQueryDevice   = "daydream.opiQueryDevice",
        opiPresent       = "daydream.opiPresent"
    ),
    list(
        name             = "Display",
        opiInitialize    = "display.opiInitialize",
        opiClose         = "display.opiClose",
        opiSetBackground = "display.opiSetBackground",
        opiQueryDevice   = "display.opiQueryDevice",
        opiPresent       = "display.opiPresent"
    ),
    list(
      name             = "PhoneHMD",
      opiInitialize    = "PhoneHMD.opiInitialize",
      opiClose         = "PhoneHMD.opiClose",
      opiSetBackground = "PhoneHMD.opiSetBackground",
      opiQueryDevice   = "PhoneHMD.opiQueryDevice",
      opiPresent       = "PhoneHMD.opiPresent"
    )
)

################################################################################
# Input parameters
#   opiImplementation  Either "Octopus900", "HEP", "SimHenson", "SimGaussian", ...
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
#'   \item\code{"HEP"}        not working so well in HEPs.
#'   \item\code{"KowaAP7000"} for interfacing with Kowa AP-7000.
#'   \item\code{"Imo"} for interfacing with CrewT's Imo head mounted perimeter.
#'   \item\code{"DayDream"} for interfacing with an Android phone in a Google Daydream
#'   \item\code{"Display"} for interfacing with a shiny plot area on the current machine.
#'   \item\code{"PhoneHMD"} for interfacing with phones using VR. At the moment, only
#'     Android compatible phones are working. The VR headset must be compatible with
#'     Cardboard
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
        assign("chooser", NA, envir = .OpiEnv)
        warning(paste("chooseOpi() cannot find opiImplementation", opiImplementation))
        return(FALSE)
    } else {
        assign("chooser", i, envir = .OpiEnv)
        return(TRUE)
    }
}

#' @rdname chooseOpi
#' @export
chooseOPI <- chooseOpi

####################################################################################
# Simply send the opi*() call to the right implementation
####################################################################################
#' @rdname opiDistributor
#' @title FOR INTERNAL USE ONLY
#' @description The method \code{opiDistributor} searches for the specific
#' method of a general OPI \code{operation}, which depends on the OPI
#' implementation selected with \code{\link{chooseOpi}}. It returns an error if no
#' OPI implementation has been selected yet. A catalog of all specific methods are
#' listed in \code{opi.implementations}.
#' @param operation A general OPI operation of the following methods to: \code{opiInitialize},
#'   \code{opiPresent} \code{opiClose}, \code{opiSetBackground},
#'   \code{opiQueryDevice}
#' @param ... other parameters to pass to the methods
#' @export
opiDistributor <- function(operation, ...) {
    if (!exists("chooser", where=.OpiEnv) || is.na(.OpiEnv$chooser)) {
        msg <- "You have not chosen a valid OPI implementation. Use chooseOpi()"
        warning(msg)
        return(msg)
    }
    toCall <- opi.implementations[[.OpiEnv$chooser]][[operation]]
    allowedArgs <- names(formals(toCall))
    haveArgs    <- names(list(...))
#print(paste("Allowed args: ", allowedArgs))
#print(paste("Have args: ", haveArgs))
    argsToPass  <- intersect(allowedArgs, haveArgs)
    argsNotPassed  <- setdiff(haveArgs, c(argsToPass, "ttHelper")) # Silently ignore ttHelper function

    if (length(argsNotPassed) > 0)
        warning(paste(operation, "Ignored argument ", argsNotPassed, "\n"))
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
#' \item{time_rec}{Only returned for Compass. Time since epoch that the
#'   opiPresent command was received by the Compass in ms.}
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
#' @export
opiPresent        <- function(stim,nextStim=NULL,...) {
    opiDistributor("opiPresent", stim=stim, nextStim=nextStim, ...)
}

#' @rdname opiInitialize
#' @title Initialize OPI
#' @description Generic function for initialization of the chosen OPI
#' implementation that is set with \code{chooseOpi()}
#' @param ... Implementation specific parameters. See details.
#' @return Returns NULL if initialization succeeded, otherwise an
#' implementation-dependent error.
#' @references
#' David B. Henson, Shaila Chaudry, Paul H. Artes, E. Brian Faragher, and Alec
#' Ansons. Response Variability in the Visual Field: Comparison of Optic Neuritis,
#' Glaucoma, Ocular Hypertension, and Normal Eyes. Investigative Ophthalmology &
#' Visual Science, February 2000, Vol. 41(2).
#' @seealso \code{\link{chooseOpi}}, \code{\link{opiSetBackground}},
#' \code{\link{opiClose}}, \code{\link{opiPresent}}
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
#' @return Returns NULL if succeeded, otherwise an implementation-dependent
#' error as follows.
#' @seealso \code{\link{chooseOpi}}
#' @export
opiSetBackground  <- function(...) { opiDistributor("opiSetBackground", ...) }

#' @rdname opiQueryDevice
#' @title Query device using OPI
#' @description Generic function for getting details of the chosen OPI
#' implementation that is set with \code{chooseOpi()}
#' @param ... Implementation specific parameters. See details.
#' @seealso \code{\link{chooseOpi}}
#' @return Returns a list that contains \code{isSim} and implementation-dependent
#' data.
#'
#' \code{isSim} is \code{TRUE} if the device is a simulation, or \code{FALSE} if
#' the device is a physical machine.
#' @export
opiQueryDevice    <- function(...) { opiDistributor("opiQueryDevice", ...) }

#' @rdname opiClose
#' @title Close using OPI
#' @description Generic function for closing the chosen OPI implementation that is set
#' with \code{chooseOpi()}
#' @param ... Implementation specific parameters. See details.
#' @return Returns NULL if close succeeded, otherwise an implementation-dependent
#' error.
#' @seealso \code{\link{chooseOpi}}
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
