#
# OPI for Octopus 900
#
# This version creates a socket to O900Server.java and sends/receives
# commands. It requires the server to be running correctly.
#
# Author: Andrew Turpin
# Author: David Lawson    (XXX)
#
# Copyright [2022] [Andrew Turpin]
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
#


###################################################################
# .opi_env$O900$... are lots of colors and Fixation constants set in setupBackgroundConstants()
###################################################################
if (exists(".opi_env") && !exists("O900", where = .opi_env))
    assign("O900", new.env(25), envir = .opi_env)

###########################################################################
# Get values for fixation, color and bg intensity constants
# from EyeSuite classes, and set globals
#       .opi_env$O900$*
# to the values of those constants.
# INPUT: None.
# OUTPUT: None.
# SIDE EFFECTS: sets .opi_env$O900$* if possible.
#               store the names and values in .opi_env$O900$constList
###########################################################################
setupBackgroundConstants <- function() {

    constList <- NULL

    getC <- function(cName) {
        writeLines(paste("OPI_GET_CONSTANT", cName), .opi_env$O900$socket)
        res <- readLines(.opi_env$O900$socket, n = 1)
        if (res == "OZ900Fail") {
           warning(paste("Cannot set",cName,"constant for the O900."))
        } else {
           assign(cName, as.double(res), envir = .opi_env$O900)
           assign("constList", c(.opi_env$O900$constList, list(list(cName, as.double(res)))),
                  envir = .opi_env$O900)
        }
    }

    getC("FIX_CENTRE")
    getC("FIX_CROSS")
    getC("FIX_RING")
    getC("BG_OFF")
    getC("BG_1")        # 1.27 cd/m2 == 127 passed to MsgInitializePerimUnit
    getC("BG_10")       # 10 cd/m2 == 1000
    getC("BG_100")      # 100 cd/m2 == 10000

    assign("FIX_CENTER", .opi_env$O900$FIX_CENTRE, envir = .opi_env$O900) # help Americans

    # get the color fields from OCTO900
    getC("STIM_WHITE")
    getC("STIM_BLUE")
    getC("STIM_RED")
    getC("BG_WHITE")
    getC("BG_YELLOW")
    getC("MET_COL_WW")
    getC("MET_COL_BY")
    getC("MET_COL_RW")
    getC("MET_COL_BLUE_WHITE")
    getC("MET_COL_RED_YELLOW")
    getC("MET_COL_WHITE_YELLOW")
    getC("MET_COL_USER")

    assign("MET_COL_BW", .opi_env$O900$MET_COL_BLUE_WHITE,   envir = .opi_env$O900)
    assign("MET_COL_RY", .opi_env$O900$MET_COL_RED_YELLOW,   envir = .opi_env$O900)
    assign("MET_COL_WY", .opi_env$O900$MET_COL_WHITE_YELLOW, envir = .opi_env$O900)
}

#' @title Implementation of opiInitialise for the Octopus900 machine.
#' @description
#'
#' This is for internal use only. Use [opiInitialise()] with
#' these Arguments and you will get the Value back.
#'
#' @usage NULL
#'
#' @param serverPort port number on which server is listening for "Octopus900"
#' @param eyeSuiteSettingsLocation dir name containing EyeSuite settings for "Octopus900"
#' @param eye eye; "right" or "left" for "Octopus900", "Octopus600"
#' @param gazeFeed NA or a folder name for "Octopus900"
#' @param bigWheel FALSE (standard machine), TRUE for modified aperture wheel for "Octopus900"
#' @param pres_buzzer 0 (no buzzer),1, 2, 3 (max volume) for "Octopus900"
#' @param resp_buzzer 0 (no buzzer),1, 2, 3 (max volume) for "Octopus900"
#' @param zero_dB_is_10000_asb Is 0 dB 10000 apostilb (TRUE) or or 4000 (FALSE) for "Octopus900"
#'
#' @return A list containing \code{err} which is
#'   * NULL if successful
#'   * 1 if Octopus900 is already initialised by a previous call to \code{opiInitialize}
#'   * 2 if some error occurred that prevented initialisation.
#'
#' @details
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
#'   button press (response). The volume can be one of 0 (no buzzer), 1, 2, or 3
#'   (max volume). If both buzzers are more than zero, the maximum of the two will
#'   be used as the volume.
#'
#'   If \code{zero_dB_is_10000_asb} is \code{TRUE} then 0 dB is taken as 10000
#'   apostilbs, otherwise 0 dB is taken as 4000 apostilbs.
#'
#' @examples
#' \dontrun{
#'   chooseOpi("Octopus900")
#'   res <- opiInitialize(serverPort = 50001,
#'                        eyeSuiteSettingsLocation = "C:/ProgramData/Haag-Streit/EyeSuite/",
#'                        eye = "", gazeFeed = "", bigWheel = FALSE,
#'                        pres_buzzer = 0, resp_buzzer = 0, zero_dB_is_10000_asb = TRUE)
#'   if (!is.null(res$err))
#'       stop("opiInitialize failed")
#' }
#
opiInitialise_for_Octopus900 <- function(serverPort = 50001,
                                       eyeSuiteSettingsLocation = "C:/ProgramData/Haag-Streit/EyeSuite/",
                                       eye = "", gazeFeed = "", bigWheel = FALSE,
                                       pres_buzzer = 0, resp_buzzer = 0, zero_dB_is_10000_asb = TRUE) {
    if (!bigWheel) {
        assign("GOLDMANN", c(6.5, 13, 26, 52, 104) / 60, envir=.opi_env$O900)
    } else {
        mm <- c(0.125,0.25,0.5,1,1.41,2,2.83,4,5.66,8,11.3,16,22.6,32,64,128,256)
        ind <- c(32,28,31,26,30,29,27,24,25,23,21,22,39,38,20,37,36)
        GOLDMANN <- rep(NA,39)
        GOLDMANN[ind] <- (sqrt(mm/pi)*180/pi/149.1954)
        assign("GOLDMANN", GOLDMANN, envir=.opi_env$O900)
    }

    if (zero_dB_is_10000_asb)
        assign("zero_db_in_asb", 10000, envir = .opi_env$O900)
    else
        assign("zero_db_in_asb",  4000, envir = .opi_env$O900)

    if (is.na(pres_buzzer) || pres_buzzer < 0) pres_buzzer <- 0
    if (is.na(resp_buzzer) || resp_buzzer < 0) resp_buzzer <- 0
    if (pres_buzzer > 3) pres_buzzer <- 3
    if (resp_buzzer > 3) resp_buzzer <- 3

    cat("Looking for server... ")
    suppressWarnings(tryCatch(
        v <- socketConnection(host = "localhost", serverPort,
                               blocking = TRUE, open = "w+b",
                               timeout = 10)
        , error = function(e) {
           stop(paste(" cannot find a server on port", serverPort))
        }
    ))
    close(v)

    print("found server :)")

    if (is.na(eyeSuiteSettingsLocation))
        stop("You must specify the EyeSuite settings folder in your call to opiInitialize")
    if (is.na(eye) || eye == "")
        stop("You must specify which eye ('left' or 'right') in your call to opiInitialize")
    if (eye != "left" && eye != "right")
        stop("The eye argument of opiInitialize must be 'left' or 'right'")

    socket <- tryCatch(
        socketConnection(host="localhost", serverPort, open = "w+b", blocking = TRUE, timeout = 1000),
        error = function(e) stop(paste("Cannot connect to Octopus 900 on port", serverPort))
    )
    assign("socket", socket, envir = .opi_env$O900)
    msg <- paste0("OPI_INITIALIZE \"",eyeSuiteSettingsLocation,"\"\ ",eye, " ", pres_buzzer, " ", resp_buzzer, " ", as.integer(zero_dB_is_10000_asb))
    msg <- paste0(msg, " ", ifelse(is.na(gazeFeed) || is.null(gazeFeed) || gazeFeed == "", "NA", gazeFeed))
    writeLines(msg, socket)
    res <- readLines(socket, n = 1)

    setupBackgroundConstants()

    if (res == "0")
        return(list(err = NULL))
    else
        return(list(err = res))
}

#' @title Implementation of opiPresent for the Octopus090 machine.
#' @description
#'
#' This is for internal use only. Use [opiPresent()] with the same arguments and
#' the class of `stim` as one of `opiStaticStimulus`, `opiTemporalStimulus`, or `opiKineticStimulus`.
#'
#' @usage NULL
#'
#' @param stim Stimulus to present (a list, see \code{octo900.present*} for details).
#' @param nextStim The stimulus to present after stim
#'        (it is not presented, but projector can move to it during response window)
#' @param F310 If \code{F310} is \code{FALSE}, response is taken from O900 button.
#'             If \code{F310} is \code{TRUE}, response is taken from external controller for (static stimuli only).
#'
#' @examples
#' \dontrun{
#'   chooseOpi("Octopus900")
#'   if (!is.null(opiInitialize()$err))
#'       stop("opiInitialize failed")
#'   s <- list(x=9, y=9, level=dbTocd(db), size=0.43, color="white",
#'             duration=200, responseWindow=1500, checkFixationOK=NULL)
#'   class(s) <- "opiStaticStimulus"
#'   print(opiPresent(s, NULL))
#' }
#'
#' @seealso [octo900.presentStatic], [octo900.presentTemporal], [octo900.presentKinetic]
#'
opiPresent_for_Octopus900 <- function(stim, nextStim = NULL, F310 = FALSE) {
    if (inherits(stim, "opiStaticStimulus"))
        return(octo900.presentStatic(stim, nextStim, F310))

    if (F310)
        return(list(err = "F310 is only supported for static stimuli"))

    if (inherits(stim, "opiTemporalStimulus"))
        return(octo900.presentTemporal(stim, nextStim))

    if (inherits(stim, "opiKineticStimulus"))
        return(octo900.presentKinetic(stim))

    return(list(err = "class of `stim` is not one of opiStaticStimulus, opiTemporalStimulus, or opiKineticStimulus"))
}

#' @title Present static on O900 (internal use)
#' @description
#' Implementation of opiPresent for the Octopus090 machine.
#' Version for opiStaticStimulus.
#'
#' This is for internal use only. Use [opiPresent()] with
#' `stim` as class `opiStaticStimulus` and you will get the Value back.
#'
#' @usage NULL
#'
#' @param stim Stimulus to present (a list, see details).
#' @param nextStim The stimulus to present after stim
#'        (it is not presented, but projector can move to it during response window)
#' @param F310 If \code{F310} is \code{FALSE}, response is taken from internal button.
#'             If \code{F310} is \code{TRUE} , response is taken from external controller
#
#' @return A list containing
#'
#'   * \code{err}  String message or NULL for no error.
#'   * \code{seen} 1 if seen, 0 otherwise. (See details for F310)
#'   * \code{time} Reaction time (if seen).
#
#' @details
#' \code{stim} is a list containing at least the following 3 elements:
#'
#'   * \code{x}, x-coordinate in degrees (floating point) (range $\[-30,30\]$).
#'   * \code{y}, y-coordinate in degrees (floating point) (range $\[-30,30\]$).
#'   * \code{level} is luminance in cd/\eqn{\mbox{m}^2}{m^2}, and is rounded to the nearest
#'     whole dB for display (range 0 to 50). 0dB is 10000aps.
#'
#' It can also contain:
#'   * \code{responseWindow} from start of stimulus presentation in milliseconds (default is 1500).
#'   * \code{duration} of stimulus on in milliseconds (default 200).
#'   * \code{color} one of \code{.opi_env$O900$STIM_WHITE}, \code{.opi_env$O900$STIM_BLUE} or
#'        \code{.opi_env$O900$STIM_RED}.  It must be same as that initialised
#'        by [opiSetup()] or [opiInitialize()]
#'        (default \code{.opi_env$O900$STIM_WHITE}).
#'   * \code{size} of stimulus diameter in degrees (default Size III == 0.43).
#'         This is rounded to the nearest support Goldmann size.
#'
#' If responses are taken from the F310 Controller then
#'  * If the L button is pressed, \code{seen} is set to 1.
#'  * If the R button is pressed, \code{seen} is set to 2.
#'  * If no button is pressed within \code{responseWindow}, then \code{seen} is set to 0.
#'
#' If stim is null, always return err = NULL status.
###########################################################################
octo900.presentStatic <- function(stim, nextStim, F310 = FALSE) {
    if (is.null(stim)) return(list(err = NULL))

    if (is.null(stim$x)) stop("opiPresent: no x value given in static stim")
    if (is.null(stim$y)) stop("opiPresent: no y value given in static stim")
    if (is.null(stim$level)) stop("opiPresent: no level value given in static stim")
    if (is.null(stim$size)) {
        warning("opiPresent: no stim size specified. Assuming Goldmann III = 26/60 degrees.")
        stim$size <- 26 / 60
    }
    if (is.null(stim$duration)) {
        warning("opiPresent: no stim duration specified. Assuming 200ms.")
        stim$duration <- 200
    }
    if (is.null(stim$responseWindow)) {
        warning("opiPresent: no stim responseWindow specified. Assuming 1500ms.")
        stim$responseWindow <- 1500
    }

    if (is.null(stim$color)) stim$color <- .opi_env$O900$STIM_WHITE

    if (!(stim$color %in% c(.opi_env$O900$STIM_WHITE, .opi_env$O900$STIM_BLUE, .opi_env$O900$STIM_RED))) {
        warning("opiPresent: bad stim color. assuming .opi_env$O900$STIM_WHITE")
        stim$color <- .opi_env$O900$STIM_WHITE
    }

    if(min(abs(.opi_env$O900$GOLDMANN - stim$size), na.rm=TRUE) > 0.05)
        warning("opiPresent: Rounding stimulus size to nearest Goldmann size")

    if (F310)
        msg <- "OPI_PRESENT_STATIC_F310"
    else
        msg <- "OPI_PRESENT_STATIC"

    msg <- paste(msg, stim$x * 10.0, stim$y * 10.0, cdTodb(stim$level, .opi_env$O900$zero_db_in_asb/pi) * 10.0)
    msg <- paste(msg, (which.min(abs(.opi_env$O900$GOLDMANN - stim$size))))
    msg <- paste(msg, stim$duration)
    msg <- paste(msg, stim$responseWindow)
    msg <- paste(msg, stim$color)
    if (!is.null(nextStim)) {
        msg <- paste(msg, nextStim$x * 10.0, nextStim$y * 10.0)
    } else {
        msg <- paste(msg, stim$x * 10.0, stim$y * 10.0)
    }

    #print(msg)
    writeLines(msg, .opi_env$O900$socket)
    #Sys.sleep(1)
    res <- readLines(.opi_env$O900$socket, n = 1)
    s <- strsplit(res, "|||", fixed = TRUE)[[1]]
    if (s[1] == "0") {
        err <- NULL
    } else {
        err <- s[1]
    }
    pupilX <- ifelse(length(s) > 3, s[4], NA)
    pupilY <- ifelse(length(s) > 4, s[5], NA)
    return(list(
        err = err,
        seen = as.numeric(s[2]),
        time = as.numeric(s[3]),
        pupilX = as.numeric(sub(",", ".", pupilX)),
        pupilY = as.numeric(sub(",", ".", pupilY))
    ))
}


#' @title Present Temporal stimuli on O900 (internal use)
#' @description
#' Implementation of opiPresent for Temporal stimuli on the Octopus090 machine.
#'
#' This is for internal use only. Use [opiPresent()] with
#' these Arguments (`stim` as class `opiTemporalStimulus`)
#' and you will get the Value back.
#'
#' @usage NULL
#'
#' @param stim Stimulus to present (a list, see details).
#' @param nextStim The stimulus to present after stim
#'        (it is not presented, but projector can move to it during response window)
#'
#' @return A list containing
#'   * \code{err}  String message or NULL for no error.
#'   * \code{seen} 1 if seen, 0 otherwise.
#'   * \code{time} Reaction time (if seen).
#'
#' @details
#' \code{stim} is a list containing at least the following 3 elements:
#'
#'   * \code{x}, x-coordinate in degrees (floating point) (range $\[-30,30\]$).
#'   * \code{y}, y-coordinate in degrees (floating point) (range $\[-30,30\]$).
#'   * \code{rate} is frequency in Hz.
#'
#' It can also contain:
#'   * \code{responseWindow} from start of stimulus presentation in milliseconds (default is 1500).
#'   * \code{duration} of stimulus on in milliseconds (default 200).
#'   * \code{size} of stimulus diameter in degrees (default Size III == 0.43).
#'         This is rounded to the nearest support Goldmann size.
#'
#' If stim is null, always return \code{err = NULL} status.
###########################################################################
octo900.presentTemporal <- function(stim, nextStim = NULL) {
    if (is.null(stim)) return(list(err = NULL))

    if (is.null(stim$x)) stop("opiPresent: no x value given in static stim")
    if (is.null(stim$y)) stop("opiPresent: no y value given in static stim")
    if (is.null(stim$rate)) stop("opiPresent: no rate value given in temporal stim")
    if (is.null(stim$size)) {
        warning("opiPresent: no stim size specified. Assuming Goldmann III = 26/60 degrees.")
        stim$size <- 26 / 60
    }
    if (is.null(stim$duration)) {
        warning("opiPresent: no stim duration specified. Assuming 200ms.")
        stim$duration <- 200
    }
    if (is.null(stim$responseWindow)) {
        warning("opiPresent: no stim responseWindow specified. Assuming 1500ms.")
        stim$responseWindow <- 1500
    }

    if (!is.null(stim$color))
        warning("opiPresent Temporal stimulus for O900: stim color ignored.")

    if (min(abs(.opi_env$O900$GOLDMANN - stim$size)) != 0)
        warning("opiPresent: Rounding stimulus size to nearest Goldmann size")

    msg <- "OPI_PRESENT_TEMPORAL "
    msg <- paste(c(msg, stim$x * 10.0, stim$y * 10.0, stim$rate), collapse = " ")
    msg <- paste(msg, (which.min(abs(.opi_env$O900$GOLDMANN - stim$size))))
    msg <- paste(msg, stim$duration)
    msg <- paste(msg, stim$responseWindow)
    if (!is.null(nextStim)) {
        msg <- paste(msg, nextStim$x * 10.0, nextStim$y * 10.0)
    } else {
        msg <- paste(msg, stim$x * 10.0, stim$y * 10.0)
    }

    writeLines(msg, .opi_env$O900$socket)
    res <- readLines(.opi_env$O900$socket, n = 1)
    s <- strsplit(res, "|||", fixed = TRUE)[[1]]

    if (s[1] == "0") {
        err <- NULL
    } else {
        err <- s[1]
    }

    return(list(
        err  = err,
        seen = as.numeric(s[2]),
        time = as.numeric(s[3])
    ))

}#presentTemporal()

#' @title Present Kinetic stimuli on O900 (internal use)
#' @description
#' Implementation of opiPresent for Kinetic stimuli on the Octopus090 machine.
#'
#' This is for internal use only. Use [opiPresent()] with
#' these Arguments and `stim` as class `opiKineticStimulus`
#' and you will get the Value back.
#'
#' @usage NULL
#'
#' @param stim Stimulus to present (a list, see details).
#' @param nextStim Ignored.
#'
#' @return A list containing
#'
#'   * \code{err}  String message or NULL for no error.
#'   * \code{seen} 1 if seen, 0 otherwise.
#'   * \code{time} Reaction time (if seen).
#'   * \code{x} Coordinate where button was pressed (degrees - i guess).
#'   * \code{y} Coordinate where button was pressed (degrees - i guess).
#'
#' @details
#' \code{stim} is a list containing at least the following 3 elements:
#'
#'   * \code{path}, A list of $(x,y)$ coordinates in degrees that is usable by \code{xy.coords()}.
#'   * \code{sizes}, A list where \code{sizes[i]} is the size of stimulus (diameter in degrees)
#'                   to use for the section of path specified by \code{path[i]..path[i+1]}.
#'                   Rounded to nearest Goldmann size.
#'   * \code{levels} A list where \code{levels[i]} is the stimulus luminance in cd/\eqn{\mbox{m}^2}{m^2}
#'                   to use for the section of path specified by \code{path[i]..path[i+1]}.
#'   * \code{speeds} A list where \code{speeds[i]} is the speed in degrees per second
#'                   to use for the section of path specified by \code{path[i]..path[i+1]}.
#'
# If stim is null, always return no error status.
##########################################
octo900.presentKinetic <- function(stim) {
    if (is.null(stim)) return(list(err = NULL))

    if (is.null(stim$path)) stop("opiPresent: no path values given in kinetic stim")
    if (is.null(stim$sizes)) stop("opiPresent: no sizes values given in kinetic stim")
    if (is.null(stim$levels)) stop("opiPresent: no levels values given in kinetic stim")
    if (is.null(stim$speeds)) stop("opiPresent: no speeds values given in kinetic stim")

    if (!is.null(stim$color))
        warning("opiPresent O900 kinetic stimulus: stim color ignored.")

    # convert sizes to GOLDMANN
    stim$sizes <- sapply(stim$sizes, function(s) {
        i <- which.min(abs(.opi_env$O900$GOLDMANN - s))
        if (abs(.opi_env$O900$GOLDMANN[i] - s) > 0.000001) {
           warning("opiPresent: Rounding stimulus size", s, "to nearest Goldmann size")
        }
        return(i)
    })

    msg <- "OPI_PRESENT_KINETIC "
    xs <- grDevices::xy.coords(stim$path)$x
    ys <- grDevices::xy.coords(stim$path)$y
    msg <- paste(c(msg, length(xs), xs, ys), collapse = " ")
    msg <- paste(c(msg, sapply(stim$levels, cdTodb, maxStim = .opi_env$O900$zero_db_in_asb / pi)), collapse = " ")
    msg <- paste(c(msg, stim$sizes), collapse = " ")

    # convert degrees/second into total time for path segment in seconds
    pathLengths <- NULL
    for (i in 2:length(xs)) {
        d <- sqrt((xs[i] - xs[i - 1])^2 + (ys[i] - ys[i - 1])^2)
        stim$speeds[i - 1] <- d / stim$speeds[i - 1]
    }
    msg <- paste(c(msg, stim$speeds), collapse = " ")

    writeLines(msg, .opi_env$O900$socket)
    res <- readLines(.opi_env$O900$socket, n=1)
    s <- strsplit(res, "|||", fixed = TRUE)[[1]]

    if (s[1] == "0") {
        err <- NULL
    } else {
        err <- s[1]
    }

    return(list(
        err = err,
        seen = as.numeric(s[2]),
        time = as.numeric(s[3]),
        x = as.numeric(s[4]) / 1000,
        y = as.numeric(s[5]) / 1000,
        pupilX = ifelse(length(s) > 5, as.numeric(s[6]), NA),
        pupilY = ifelse(length(s) > 6, as.numeric(s[7]), NA)
    ))
}

#' @title Implementation of opiSetup for the Octopus900 machine.
#' @description
#'
#' This is for internal use only.
#' Use [opiSetup()] with the same parameters.
#'
#' @usage NULL
#'
#' @param lum Luminance level in cd/m^2
#' @param color Stimulus color (see details)
#' @param fixation fixation target
#' @param fixIntensity fixation point intensity
#'
#' @return A list with element \code{err} which is
#'  * NULL on success
#'  * -1 indicates \code{opiInitialize} has not been called.
#'  * -2 indicates could not set the background color.
#'  * -3 indicates could not set the fixation marker.
#'  * or a string message about bad parameters
#'
#' @details
#'   Allowable \code{lum} and \code{color} are defined in the \code{.opi_env$O900} environment.
#'
#'   * \code{lum} is intensity of the background and can be one of
#'     - \code{.opi_env$O900$BG_OFF}, which turns background off.
#'     - \code{.opi_env$O900$BG_1}, background of 1.27 cd/\eqn{\mbox{m}^2}{m^2}.
#'     - \code{.opi_env$O900$BG_10}, background of 10 cd/\eqn{\mbox{m}^2}{m^2}.
#'     - \code{.opi_env$O900$BG_100}, background of 100 cd/\eqn{\mbox{m}^2}{m^2}.
#'   * \code{color} can be one of the following choices.
#'      - \code{.opi_env$O900$MET_COL_WW} for white-on-white
#'      - \code{.opi_env$O900$MET_COL_RW} for red-on-white
#'      - \code{.opi_env$O900$MET_COL_BW} for blue-on-white
#'      - \code{.opi_env$O900$MET_COL_WY} for white-on-yellow
#'      - \code{.opi_env$O900$MET_COL_RY} for red-on-yellow
#'      - \code{.opi_env$O900$MET_COL_BY} for blue-on-yellow
#'    * \code{fixation} is one of
#'       - \code{.opi_env$O900$FIX_CENTRE} or \code{.opi_env$O900$FIX_CENTER}
#'       - \code{.opi_env$O900$FIX_CROSS}
#'       - \code{.opi_env$O900$FIX_RING}
#'    * \code{fixIntensity} is a percentage between 0 and 100. 0 is off, 100 the brightest.
#'
#'   Note if you specify \code{fixation} you also have to specify \code{fixIntensity}.
#'
#' @examples
#' \dontrun{
#'   chooseOpi("Octopus900")
#'   oi <- opiInitialize(eyeSuiteJarLocation="c:/EyeSuite/",
#'                       eyeSuiteSettingsLocation="c:/Documents and Settings/All Users/Haag-Streit/",
#'                       eye="left")
#'   if(!is.null(oi$err))
#'       stop("opiInitialize failed")
#'   if(!is.null(opiSetup(fixation=.opi_env$O900$FIX_CENTRE)$err))
#'       stop("opiSetup failed")
#'   if(!is.null(opiSetup(fixation=.opi_env$O900$FIX_RING, fixIntensity=0)$err))
#'       stop("opiSetup failed")
#'   if(!is.null(opiSetup(color=.opi_env$O900$MET_COL_BY)$err))
#'       stop("opiSetup failed")
#'   if(!is.null(opiSetup(lum=.opi_env$O900$BG_100, color=.opi_env$O900$MET_COL_RW)$err))
#'       stop("opiSetup failed")
#'   opiClose()
#' }
opiSetup_for_Octopus900 <- function(lum = NA, color = NA, fixation = NA, fixIntensity = NA) {
        if (all(is.na(c(lum, color, fixation, fixIntensity))))
            return(list(err = "At least one parameter must be not NA in opiSetBackground"))

        if (!is.na(fixation) && is.na(fixIntensity))
            return(list(err = "If fixation is specified, fixIntensity must also be given in opiSetBackground"))

        if (!is.na(fixIntensity) && is.na(fixation)) {
            return(list(err = "If fixIntensity is specified, fixation must also be given in opiSetBackground"))
        }

        if (is.na(lum)) lum <- -1
        if (is.na(color)) color <- -1
        if (is.na(fixation)) fixation <- -1
        if (is.na(fixIntensity)) fixIntensity <- -1

        msg <- paste("OPI_SET_BACKGROUND", color, lum, fixation, fixIntensity)
        writeLines(msg, .opi_env$O900$socket)
        ret <- as.numeric(readLines(.opi_env$O900$socket, n = 1))

        if (ret == "0") {
           return(list(err = NULL))
        } else {
           return(list(err = ret))
        }
}


#' @title Implementation of opiClose for the Octopus900 machine.
#' @description
#'
#' This is for internal use only. Use [opiClose()] with the same parameters.
#'
#' @usage NULL
#'
#' @return Returns \code{list(err = NULL)}.
opiClose_for_Octopus900 <- function() {
    writeLines("OPI_CLOSE", .opi_env$O900$socket)
    close(.opi_env$O900$socket)
    return(list(err = NULL))
}

#' @title Implementation of opiQueryDevice for the Octopus900 machine.
#' @description
#'
#' This is for internal use only. Use [opiQueryDevice()] with
#' these Arguments and you will get the Value back.
#'
#' @usage NULL
#'
#' @details Prints defined constants in OPI package pertaining to Octopus 900.
#'
#'@return List containing \code{isSim = FALSE}.
#'
opiQueryDevice_for_Octopus900 <- function() {
    cat("Defined constants\n")
    cat("-----------------\n")
    lapply(.opi_env$O900$constList, function(x) {
        lapply(x, cat, " ")
        cat("\n")
    })

    return(list(isSim = FALSE))
}