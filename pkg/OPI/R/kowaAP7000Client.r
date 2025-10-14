#
# OPI for Kowa AP 7000
#
# Based on octopus900Client.r.
#
# Author: Andrew Turpin    (andrew.turpin@lei.org.au)
# Date: December 2014
#
# Copyright [2015] [Andrew Turpin]
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
# .opi_env$KowaAP7000$socket is the connection to the AP 7000
# .opi_env$KowaAP7000$...    a variety of constants for colors, etc
###################################################################
if (exists(".opi_env") && !exists("KowaAP7000", where=.opi_env)) {
    assign("KowaAP7000", new.env(25), envir=.opi_env)

    .opi_env$KowaAP7000$BACKGROUND_WHITE  <- 0  # white, 10 cd/m^2
    .opi_env$KowaAP7000$BACKGROUND_YELLOW <- 1  # yellow, 100 cd/m^2

    .opi_env$KowaAP7000$FIX_CENTRE   <- 0   # fixation markers
    .opi_env$KowaAP7000$FIX_CENTER   <- 0   # usa spelling
    .opi_env$KowaAP7000$FIX_AUX      <- 1
    .opi_env$KowaAP7000$FIX_MACULA   <- 2
    .opi_env$KowaAP7000$FIX_AUX_LEFT <- 3

    .opi_env$KowaAP7000$SIZES_DEGREES <- c(6.5, 13, 26, 52, 104) / 60 # Goldmann target sizes in degrees

    .opi_env$KowaAP7000$COLOR_WHITE <- 0
    .opi_env$KowaAP7000$COLOR_GREEN <- 1
    .opi_env$KowaAP7000$COLOR_BLUE  <- 2
    .opi_env$KowaAP7000$COLOR_RED   <- 3

    # Utility functions for validating inputs
    .opi_env$KowaAP7000$minCheck <- function(x, limit, txt) {
        if (x < limit) {
	    opiClose()
            stop(paste("opiPresent: ", txt, "is too small (minimum ", limit, ")"))
	}
    }
    .opi_env$KowaAP7000$maxCheck <- function(x, limit, txt) {
        if (x > limit) {
	    opiClose()
            stop(paste("opiPresent: ", txt, "is too big (maximum ", limit, ")"))
	}
    }
    .opi_env$KowaAP7000$checkOK <- function(txt) {
        res <- readLines(.opi_env$KowaAP7000$socket, n=1)
        #cat("ap7000 sends back>>>", res, "<<<\n")
        if (res != "OK")
            warning(paste(txt, "did not return OK from AP-7000"))
    }
}

#' @title Implementation of opiInitialise for the Octopus900 machine.
#' @description
#' This is for internal use only. Use [opiInitialise()] with
#' these Arguments and you will get the Value back.
#'
#' @usage NULL
#'
#' @param ip IP address of AP7000 machine (as string)
#' @param port Port number on which AP7000 server is listening
#' @details
#'
#'   If the chosen OPI implementation is \code{KowaAP7000}, then you must specify
#'   the IP address and port of the AP-7000 server.
#'
#' @return \code{list(err = NULL)}

#' @examples
#' \dontrun{
#'   # Set up the Kowa AP-7000
#'   chooseOpi("KowaAP7000")
#'   opiInitialize(ip="192.168.1.7", port=44965)
#' }
opiInitialise_for_KowaAP7000 <- function(ip = "192.168.1.2", port = 44965) {
    cat("Looking for server... ")
    suppressWarnings(tryCatch(
        v <- socketConnection(host = ip, port,
                      blocking = TRUE, open = "w+b",
                      timeout = 10)
        , error=function(e) {
            stop(paste(" cannot find a server at", ip, "on port",port))
        }
    ))
    close(v)

    print("found server :)")

    socket <- tryCatch(
        socketConnection(host=ip, port, open = "w+b", blocking = TRUE, timeout = 1000),
        error=function(e) stop(paste("Cannot connect to AP 7000 at",ip,"on port", port))
    )

    assign("socket", socket, envir = .opi_env$KowaAP7000)
    msg <- paste0("OPI-SET-MODE\r")
    writeLines(msg, socket)
    res <- readLines(.opi_env$KowaAP7000$socket, n=1)

    if (res != "OK")
        stop(paste("Trouble initialising AP-7000. OPI-SET-MODE returns ",res))

    assign("machine_is_initialised", TRUE, .opi_env)

    return(list(err = NULL))
}

#' @title Implementation of opiPresent for the KowaAP7000 machine.
#' @description
#'
#' This is for internal use only. Use [opiPresent()] with the same arguments and
#' the class of `stim` as one of `opiStaticStimulus`, `opiTemporalStimulus`, or `opiKineticStimulus`.
#'
#' @usage NULL
#'
#' @param stim Stimulus to present (a list, see \code{kowa.*} for details).
#' @param nextStim The stimulus to present after stim
#'        (it is not presented, but projector can move to it during response window)
#'
#' @return See [kowa.presentStatic], [kowa.presentTemporal], or [kowa.presentKinetic].
#' 
#' @examples
#' \dontrun{
#'   chooseOpi("KowaAP7000")
#'   if (!is.null(opiInitialize()$err))
#'       stop("opiInitialize failed")
#'   s <- list(x=9, y=9, level=dbTocd(db), size=0.43, color="white",
#'             duration=200, responseWindow=1500, checkFixationOK=NULL)
#'   class(s) <- "opiStaticStimulus"
#'   print(opiPresent(s, NULL))
#' }
#'
#' @seealso [kowa.presentStatic], [kowa.presentTemporal], [kowa.presentKinetic]
#'
opiPresent_for_KowaAP7000 <- function(stim, nextStim = NULL) {
    if (inherits(stim, "opiStaticStimulus"))
        return(kowa.presentStatic(stim, nextStim))

    if (inherits(stim, "opiTemporalStimulus"))
        return(kowa.presentTemporal(stim, nextStim))

    if (inherits(stim, "opiKineticStimulus"))
        return(kowa.presentKinetic(stim))

    return(list(err = "class of `stim` is not one of opiStaticStimulus, opiTemporalStimulus, or opiKineticStimulus"))
}

#' @title Present static on Kowa AP7000 (internal use)
#' @description
#' Implementation of opiPresent for the Kowa AP7000 machine.
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
#
#' @return A list containing
#'
#'   * \code{err}  String message or NULL for no error.
#'   * \code{seen} 1 if seen, 0 otherwise.
#'   * \code{time} Reaction time (if seen).
#'   * \code{pupilX}
#'   * \code{pupilY}
#'   * \code{purkinjeX}
#'   * \code{purkinjeY}
#'
#' @details
#' \code{stim} is a list containing at least the following 3 elements:
#'
#'   * \code{x}, x-coordinate in degrees (floating point) (range $\[-80,80\]$).
#'   * \code{y}, y-coordinate in degrees (floating point) (range $\[-70,65\]$).
#'   * \code{level} is luminance in cd/\eqn{\mbox{m}^2}{m^2}, and is rounded to the nearest
#'     whole dB for display (range 0 to 50). 0dB is 10000aps.
#'   * \code{duration} of stimulus on in milliseconds (range $\[100, 1200\]$).
#'   * \code{responseWindow} from start of stimulus presentation in milliseconds (max 5000).
#'   * \code{size} one of \code{.opi_env$KowaAP7000$SIZES_DEGREES}.
#'   * \code{color} one of \code{.opi_env$KowaAP7000$COLOR_WHITE}, \code{.opi_env$KowaAP7000$COLOR_GREEN}, \code{.opi_env$KowaAP7000$COLOR_BLUE}, or \code{.opi_env$KowaAP7000$COLOR_RED}
#'
kowa.presentStatic <- function(stim, nextStim) {
    if (is.null(stim)) {
        warning("opiPresent: NULL stimulus")
        return(list(err="NULL stimulus not supported", seen=NA, time=NA, pupilX=NA, pupilY=NA))
    }

    if(min(abs(.opi_env$KowaAP7000$SIZES_DEGREES - stim$size)) != 0)
        warning("opiPresent: Rounding stimulus size to nearest Goldmann size")

    if (!is.element(stim$color, c(.opi_env$KowaAP7000$COLOR_WHITE,
                                  .opi_env$KowaAP7000$COLOR_GREEN,
                                  .opi_env$KowaAP7000$COLOR_BLUE ,
                                  .opi_env$KowaAP7000$COLOR_RED  ))) {
        opiClose()
        stop("opiPresent: stimulus color is not supported.")
    }

    .opi_env$KowaAP7000$minCheck(stim$x, -80, "Stimulus x")
    .opi_env$KowaAP7000$maxCheck(stim$x,  80, "Stimulus x")
    .opi_env$KowaAP7000$minCheck(stim$y, -70, "Stimulus y")
    .opi_env$KowaAP7000$maxCheck(stim$y,  65, "Stimulus y")
    .opi_env$KowaAP7000$minCheck(stim$duration,  100, "Stimulus duration")
    .opi_env$KowaAP7000$maxCheck(stim$duration, 1200, "Stimulus duration")
    .opi_env$KowaAP7000$minCheck(stim$responseWindow,  stim$duration, "Stimulus responseWindow")
    .opi_env$KowaAP7000$maxCheck(stim$responseWindow,           5000, "Stimulus responseWindow")
    .opi_env$KowaAP7000$minCheck(stim$level,  10000/pi/10^5, "Stimulus level")
    .opi_env$KowaAP7000$maxCheck(stim$level,  10000/pi     , "Stimulus level")

    if (!is.null(nextStim))
        warning("opiPresent: nextStim ignored")

    msg <- "OPI-PRESENT-STATIC"
    msg <- paste(msg, stim$x, stim$y, cdTodb(stim$level, 10000/pi))
    msg <- paste(msg, (which.min(abs(.opi_env$KowaAP7000$SIZES_DEGREES - stim$size))))
    msg <- paste(msg, stim$color)
    msg <- paste(msg, stim$duration)
    msg <- paste(msg, stim$responseWindow)
    msg <- paste(msg, "\r", sep="")

    writeLines(msg, .opi_env$KowaAP7000$socket)
    res <- readLines(.opi_env$KowaAP7000$socket, n=1)
    s <- strsplit(res, " ", fixed=TRUE)[[1]]

    if (s[1] != "OK")
        warning("opiPresent failed")

    return(list(
      err=NULL,
      seen=ifelse(s[2] == "1", TRUE, FALSE),    # assumes 1 or 0, not "true" or "false"
      time=as.numeric(s[3]),
      pupilX=as.numeric(s[4]),
      pupilY=as.numeric(s[5]),
      purkinjeX=as.numeric(s[6]),
      purkinjeY=as.numeric(s[7])
    ))
}

#' @title Present kinetic on Kowa AP7000 (internal use)
#' @description
#' Implementation of opiPresent for the Kowa AP7000 machine.
#' Version for opiKineticStimulus.
#'
#' This is for internal use only. Use [opiPresent()] with
#' `stim` as class `opiStaticStimulus` and you will get the Value back.
#'
#' @usage NULL
#'
#' @param stim Stimulus to present (a list, see details).
#
#' @return A list containing
#'
#'   * \code{err}  String message or NULL for no error.
#'   * \code{seen} 1 if seen, 0 otherwise.
#'   * \code{time} Reaction time (if seen).
#'   * \code{x} Location of button press in degrees.
#'   * \code{y} Location of button press in degrees.
#'
#' @details
#' \code{stim} is a list containing at least the following 3 elements:
#'
#'   * \code{path}, a list containing \code{x} 2 x-coordinate in degrees (floating point) (range $\[-80,80\]$) and \code{y}, list of 2 y-coordinate in degrees (floating point) (range $\[-70,65\]$).
#'   * \code{sizes} list of 1 size; one of \code{.opi_env$KowaAP7000$SIZES_DEGREES}.
#'   * \code{colors} list of 1 color; one of \code{.opi_env$KowaAP7000$COLOR_WHITE}, \code{.opi_env$KowaAP7000$COLOR_GREEN}, \code{.opi_env$KowaAP7000$COLOR_BLUE}, or \code{.opi_env$KowaAP7000$COLOR_RED}
#'   * \code{levels} list of 1 level; luminance in cd/\eqn{\mbox{m}^2}{m^2}, and is rounded to the nearest whole dB for display (range 0 to 50). 0dB is 10000aps.
#'   * \code{speeds} list of 1 speed; degrees per second range $\[3, 5\]$.
#'   * \code{duration} of stimulus on in milliseconds (range $\[100, 1200\]$).
#'   * \code{responseWindow} from start of stimulus presentation in milliseconds (max 5000).
#'
kowa.presentKinetic <- function(stim, ...) {
    if (is.null(stim)) {
        warning("opiPresent: NULL stimulus")
        return(list(err="NULL stimulus not supported", seen=NA, x=NA, y=NA))
    }

    if (length(grDevices::xy.coords(stim$path)$x) > 2)
        warning("opiPresent (kinetic): Kowa AP-7000 only supports paths of length 2 (start and end).  Ignoring all but the first two elements of stim$path etc")

        # convert sizes to .opi_env$KowaAP7000$SIZES_DEGREES
    stim$sizes <- sapply(stim$sizes, function(s) {
         i <- which.min(abs(.opi_env$KowaAP7000$SIZES_DEGREES - s))
         if(abs(.opi_env$KowaAP7000$SIZES_DEGREES[i] - s) > 0.000001) {
             warning(paste("opiPresent: Rounding stimulus size",s,"to nearest Goldmann size"))
         }
         return(i)
    })

    if (!is.element(stim$colors[1], c(.opi_env$KowaAP7000$COLOR_WHITE,
                                  .opi_env$KowaAP7000$COLOR_GREEN,
                                  .opi_env$KowaAP7000$COLOR_BLUE ,
                                  .opi_env$KowaAP7000$COLOR_RED  ))) {
        opiClose()
        stop("opiPresent: stimulus color is not supported.")
     }

    .opi_env$KowaAP7000$minCheck(grDevices::xy.coords(stim$path)$x[1], -80, "Start x")
    .opi_env$KowaAP7000$maxCheck(grDevices::xy.coords(stim$path)$x[1], 80, "Start x")
    .opi_env$KowaAP7000$minCheck(grDevices::xy.coords(stim$path)$x[2], -80, "End x")
    .opi_env$KowaAP7000$maxCheck(grDevices::xy.coords(stim$path)$x[2], 80, "End x")
    .opi_env$KowaAP7000$minCheck(grDevices::xy.coords(stim$path)$y[1], -70, "Start y")
    .opi_env$KowaAP7000$maxCheck(grDevices::xy.coords(stim$path)$y[1], 65, "Start y")
    .opi_env$KowaAP7000$minCheck(grDevices::xy.coords(stim$path)$y[2], -70, "End y")
    .opi_env$KowaAP7000$maxCheck(grDevices::xy.coords(stim$path)$y[2], 65, "End y")
    .opi_env$KowaAP7000$minCheck(stim$levels[1],  10000/pi/10^5, "Stimulus level")
    .opi_env$KowaAP7000$maxCheck(stim$levels[1],  10000/pi     , "Stimulus level")
    .opi_env$KowaAP7000$minCheck(stim$speeds[1],  3, "Stimulus speed")
    .opi_env$KowaAP7000$maxCheck(stim$speeds[1],  5, "Stimulus speed")

    msg <- "OPI-PRESENT-KINETIC "
    xs <- grDevices::xy.coords(stim$path)$x[1]
    ys <- grDevices::xy.coords(stim$path)$y[1]
    msg <- paste(msg, grDevices::xy.coords(stim$path)$x[1])
    msg <- paste(msg, grDevices::xy.coords(stim$path)$y[1])
    msg <- paste(msg, grDevices::xy.coords(stim$path)$x[2])
    msg <- paste(msg, grDevices::xy.coords(stim$path)$y[2])
    msg <- paste(msg, cdTodb(stim$levels[1], maxStim=10000/pi))
    msg <- paste(msg, stim$sizes[1])
    msg <- paste(msg, stim$colors[1])
    msg <- paste(msg, stim$speeds[1])
	msg <- paste(msg, "\r", sep="")

    writeLines(msg, .opi_env$KowaAP7000$socket)
    res <- readLines(.opi_env$KowaAP7000$socket, n=1)
    s <- strsplit(res, " ", fixed=TRUE)[[1]]

    if (s[1] != "OK")
        warning("opiPresent Kinetic failed")

    return(list(
        err =NULL,
        seen=ifelse(s[2] == "1", TRUE, FALSE),    # assumes 1 or 0, not "true" or "false"
        time=NA,
        x=as.numeric(s[3]),     # in degrees
        y=as.numeric(s[4])       # in degrees
    ))
}

#' @title Present temporal stim on Kowa AP7000 (internal use)
#' @description
#' Implementation of opiPresent for the Kowa AP7000 machine.
#' Version for opiKineticStimulus.
#'
#' This is for internal use only. Use [opiPresent()] with
#' `stim` as class `opiStaticStimulus` and you will get the Value back.
#'
#' @usage NULL
kowa.presentTemporal <- function(stim, nextStim=NULL, ...) {
    opiClose()
    stop("opiPresent: Kowa AP 7000 does not support temporal stimuli")
}#opiPresent.opiTemporalStimulus()

#' @title opiSetBackground
#' @description
#' Implementation of opiSetup for the Kowa AP7000 machine.
#'
#' This is for internal use only. Use [opiSetup()] with these Arguments.
#'
#'
#' @param lum Must be 10 for a white background and 100 for a yellow.
#' @param color One of \code{.opi_env$KowaAP7000$BACKGROUND_WHITE} or \code{.opi_env$KowaAP7000$BACKGROUND_YELLOW}.
#' @param fixation One of
#'     * \code{.opi_env$KowaAP7000$FIX_CENTER}, fixation marker in the centre.
#'     * \code{.opi_env$KowaAP7000$FIX_CENTRE}, fixation marker in the centre.
#'     * \code{.opi_env$KowaAP7000$FIX_AUX},    fixation marker is ???.
#'     * \code{.opi_env$KowaAP7000$FIX_MACULA}, fixation marker is a circle(?).
#'     * \code{.opi_env$KowaAP7000$FIX_AUX_LEFT}, fixation marker is as for AUX but only lower left.
#'
#' @details 
#'   If \code{lum} is 10 and \code{color} is not set, then
#'   \code{.opi_env$KowaAP7000$BACKGROUND_WHITE} is assumed.
#'
#'   If \code{lum} is 100 and \code{color} is not set,
#'   then \code{.opi_env$KowaAP7000$BACKGROUND_YELLOW} is assumed.
#'
#'   If both \code{lum} and \code{color} is set, then \code{lum} is ignored
#'   (a warning will be generated
#'
#'   If \code{lum} is incompatible with \code{color}).
#'
#'
#' @return
#'   Always returns \code{list(err = NULL)}
#'
opiSetup_for_KowaAP7000 <- function(lum=NA, color=NA, fixation=NA) {
    if (!is.na(fixation)) {
        .opi_env$KowaAP7000$minCheck(fixation, 0, "Fixation")
        .opi_env$KowaAP7000$maxCheck(fixation, 3, "Fixation")

        msg <- paste("OPI-SET-FIXATION ", fixation, "\r", sep="")
        writeLines(msg, .opi_env$KowaAP7000$socket)
        .opi_env$KowaAP7000$checkOK("opiSetBackground fixation")
    }

    if (!is.na(lum) && !is.na(color)) {
        if (lum == 10 && color != .opi_env$KowaAP7000$BACKGROUND_WHITE)
            warning("Can only have a 10 cd/m^2 background that is white")
        if (lum == 100 && color != .opi_env$KowaAP7000$BACKGROUND_YELLOW)
            warning("Can only have a 100 cd/m^2 background that is yellow")
    }

    if (!is.na(lum) && is.na(color)) {
        if (lum == 10) {
            color <- .opi_env$KowaAP7000$BACKGROUND_WHITE
            warning("Can only have a 10 cd/m^2 background that is white")
        } else if (lum == 100) {
            color <- .opi_env$KowaAP7000$BACKGROUND_YELLOW
            warning("Can only have a 100 cd/m^2 background that is yellow")
        } else {
            opiClose()
            stop("opiSetBackground: Can only have 10 cd/m^2 (white) or 100 cd/m^2 (yellow)")
        }
    }

    if (!is.na(color)) {
        .opi_env$KowaAP7000$minCheck(color, 0, "Background color")
        .opi_env$KowaAP7000$maxCheck(color, 1, "Background color")
        msg <- paste0("OPI-SET-BACKGROUND ", color,"\r")
        writeLines(msg, .opi_env$KowaAP7000$socket)
        .opi_env$KowaAP7000$checkOK("opiSetBackground color")
    }

    return(list(err = NULL))
}

#' @title Implementation of opiClose for the Kowa AP7000 machine.
#' @description
#'
#' This is for internal use only. Use [opiClose()] with the same parameters.
#'
#' @usage NULL
#'
#' @return Returns \code{list(err = NULL)}.
opiClose_for_KowaAP7000 <- function() {
    writeLines("OPI-CLOSE\r", .opi_env$KowaAP7000$socket)
    .opi_env$KowaAP7000$checkOK("opiClose")
    close(.opi_env$KowaAP7000$socket)
    return(list(err = NULL))
}

#' @title Implementation of opiQueryDevice for the Kowa AP7000 machine.
#' @description
#'
#' This is for internal use only. Use [opiQueryDevice()] with
#' these Arguments and you will get the Value back.
#'
#' @usage NULL
#'
#' @details Prints defined constants in OPI package pertaining to Kowa AP7000.
#'
#'@return List containing
#'   * \code{isSim = FALSE}.
#'   * \code{pupilX}, the x-coordinate of the pupil position in pixels.
#'   * \code{pupilY}, the y-coordinate of the pupil position in pixels.
#'   * \code{purkinjeX}, the x-coordinate of the purkinje position in pixels.
#'   * \code{purkinjeY}, the y-coordinate of the purkinje position in pixels.
#'
opiQueryDevice_for_KowaAP7000 <- function() {
    cat("Defined constants and functions\n")
    cat("-------------------------------\n")
    ls(envir=.opi_env$KowaAP7000)

    writeLines("OPI-GET-PUPILPOS\r", .opi_env$KowaAP7000$socket)
    res <- readLines(.opi_env$KowaAP7000$socket, n=1)
    s <- strsplit(res, " ", fixed=TRUE)[[1]]

    if (s[1] != "OK")
        warning("opiQueryDevice failed")

    return(list(
        isSim=FALSE,
        pupilX=strtoi(s[2]),
        pupilY=strtoi(s[3]),       # in pixels
        purkinjeX=strtoi(s[4]),
        purkinjeY=strtoi(s[5])       # in pixels
    ))
}
