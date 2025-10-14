#
# OPI for Compass
#
# Author: Andrew Turpin    (aturpin@unimelb.edu.au)
# Date: July 2016 (In Padova!)
#
# Copyright [2022] [Andrew Turpin & Ivan Marin-Franch]
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

###################################################################
# .opi_env$Compass$socket is the connection to the Compass
# .opi_env$Compass$...    a variety of constants, etc
###################################################################
if (exists(".opi_env") && !exists("Compass", where = .opi_env)) {
    assign("Compass", new.env(25), envir = .opi_env)
    .opi_env$Compass$endian <- "big"   # endian-ness of the compass OS

    .opi_env$Compass$ZERO_DB_IN_ASB <- 10000

    .opi_env$Compass$MAX_DB <- 50
    .opi_env$Compass$MIN_DB <- 0
    .opi_env$Compass$MIN_X  <- -30
    .opi_env$Compass$MAX_X  <- 30
    .opi_env$Compass$MIN_Y  <- -30
    .opi_env$Compass$MAX_Y  <- 30
    .opi_env$Compass$MIN_RESP_WINDOW  <- 0
    .opi_env$Compass$MAX_RESP_WINDOW  <- 2680
    .opi_env$Compass$MIN_DURATION  <- 1

    .opi_env$Compass$SEEN     <- 1
    .opi_env$Compass$NOT_SEEN <- 0

    # Utility functions for validating inputs
    .opi_env$Compass$minCheck <- function(x, limit, txt) {
        if (is.null(x))
            return()

        if (x < limit) {
            opiClose()
            stop(paste("opiPresent: ", txt, "is too small (minimum ", limit, ")"))
        }
    }
    .opi_env$Compass$maxCheck <- function(x, limit, txt) {
        if (is.null(x))
            return()
        if (x > limit) {
            opiClose()
            stop(paste("opiPresent: ", txt, "is too big (maximum ", limit, ")"))
        }
    }
}

#' Implementation of opiInitialise for the ImoVifa machine.
#'
#' This is for internal use only. Use [opiInitialise()] with
#' these Arguments and you will get the Value back.
#'
#' @usage NULL
#' @param ip IP address on which server is listening as a string
#' @param port Port number on which server is listening
#' @param ... Could be used for fake compass, simulations, etc
#'
#' @return A list with elements:
#'  * \code{err} NULL if successful, not otherwise.
#'  * \code{prl} A pair giving the (x,y) in degrees of the Preferred Retinal
#'       Locus detected in the initial alignment.
#'  * \code{onh} a pair giving the (x,y) in degrees of the ONH as selected by
#'     the user.
#'  * \code{image} raw bytes being the JPEG compressed infra-red image acquired
#'     during alignment.
#'
#' @details
#'   Warning: this returns a list, not a single error code.
#'
#' @examples
#' \dontrun{
#'   # Set up the Compass
#'   chooseOpi("Compass")
#'   result <- opiInitialize(ip = "192.168.1.7", port = 44965)
#'   if (is.null(result$err))
#'     print(result$prl)
#' }
opiInitialise_for_Compass <- function(ip = "192.168.1.2", port = 44965, ...) {
    if ("socket" %in% ls(envir = .opi_env$Compass))
        return(list(err = "Compass already connected. Did you opiClose()?", prl = NULL, onh = NULL, image = NULL))

    tries <- 1
    while (tries < 5) {
        cat("\nLooking for server ", tries, "... ")
        socket <- tryCatch(
            socketConnection(host = ip, port, open = "w+b", blocking = TRUE, timeout = 1000),
            error = function(e) stop(paste("Cannot connect to Compass at", ip, "on port", port))
        )
        cat("found at", ip, port, ":)\n")

        assign("socket", socket, envir = .opi_env$Compass)

        msg <- "OPI-OPEN"
        writeLines(msg, socket)
        n <- readBin(socket, "integer", size = 4, endian = .opi_env$Compass$endian)

        print(paste("opiInitialize expecting", n, "bytes"))

        if (length(n) > 0)
            break

        closeAllConnections()
        tries <- tries + 1
        Sys.sleep(0.3)
    }

    if (length(n) == 0) {
        warning("Compass did get the answer it expected from the OPEN command.(5 times!) Suggest closeAllConnections() and try again")
        close(socket)
        rm("socket", envir = .opi_env$Compass)
        return(list(err = "Bad open", prl = NULL, onh = NULL, image = NULL))
    } else {
        #print(paste("opiInitialize read: ", n))
        prlx <- readBin(socket, "double", size = 4, endian = .opi_env$Compass$endian)
        prly <- readBin(socket, "double", size = 4, endian = .opi_env$Compass$endian)
        onhx <- readBin(socket, "double", size = 4, endian = .opi_env$Compass$endian)
        onhy <- readBin(socket, "double", size = 4, endian = .opi_env$Compass$endian)
        im <- openssl::base64_encode(readBin(socket, "raw", n = (n - 16), size = 1, endian = .opi_env$Compass$endian))

        assign("machine_is_initialised", TRUE, .opi_env)

        return(list(err = NULL, prl = c(prlx, prly), onh = c(onhx, onhy), image = im))
    }
}

#' Implementation of opiPresent for the Compass machine.
#'
#' This is for internal use only. Use [opiSetup()] with
#' these Arguments and you will get the Value back.
#'
#' @usage NULL
#'
#' @param stim A list of stimulus parameters (see Details).
#' @param nextStim Unused - included for compliance with OPI standard.
#'
#' @return A list containing:
#'  * \code{err} 0 all clear, >= 1 some error codes (eg cannot track, etc) (integer)
#'  * \code{seen} \code{FALSE} for not seen, \code{TRUE} for seen (button pressed in response window)
#'  * \code{time} Response time in ms (integer) since stimulus onset, -1 for not seen
#'  * \code{time_rec} Time since epoch when command was received at Compass (integer ms)
#'  * \code{time_pres} Time since epoch that stimulus was presented (integer ms)
#'  * \code{num_track_events} Number of tracking events that occurred during presentation (integer)
#'  * \code{num_motor_fails} Number of times motor could not follow fixation movement during presentation (integer)
#'  * \code{pupil_diam} Pupil diameter in mm (float)
#'  * \code{loc_x} Pixels integer, location in image of presentation (integer)
#'  * \code{loc_y} Pixels integer, location in image of presentation (integer)
#'
#' @details
#'   If the chosen OPI implementation is \code{Compass}, then \code{nextStim}
#'   is ignored. Note that the dB level is rounded to the nearest integer.
#'
#'   If tracking is on, then this will block until the tracking is obtained,
#'   and the stimulus presented.
#'
#' \code{stim} is a list containing some or all of the following elements:
#'
#'   * \code{x}, x-coordinate in degrees (floating point) (range $\[-30,30\]$).
#'   * \code{y}, y-coordinate in degrees (floating point) (range $\[-30,30\]$).
#'   * \code{level} is luminance in cd/\eqn{\mbox{m}^2}{m^2}, and is rounded to the nearest
#'     whole dB for display (range 0 to 50). 0dB is 10000aps.
#'   * \code{responseWindow} is in milliseconds (range 0 to 2680).
#'
#' Stimulus duration is assumed to be 200ms, and size is assumed to be
#' Goldmann III (0.43 degrees diameter),  color is assumed to be white.
#' These cannot be changed.
#'
#' @examples
#' \dontrun{
#'   # Set up the Compass
#'   chooseOpi("Compass")
#'   result <- opiInitialize(ip = "192.168.1.7", port = 44965)
#'   if (!is.null(result$err))
#'     stop("Initialisation failed")
#'
#'    #' @param x X location of stim in degrees
#'    #' @param y Y location of stim in degrees
#'    #' @param size If 3, Goldmann III, else V
#'    #' @param db Value in dB
#'    #' @return stim object ready for opiPresent
#'    makeStim <- function(x, y, size, db) {
#'        s <- list(x = x, y = y, level = dbTocd(db, 10000 / pi),
#'            size = ifelse(size == 3, 0.43, 1.77),
#'            duration = 200, responseWindow = 1500)
#'        class(s) <- "opiStaticStimulus"
#'        return(s)
#'    }
#'
#'    result <- opiPresent(makeStim(9, 9, 3, 10))
#' }
#'
#
opiPresent_for_Compass <- function(stim, nextStim = NULL) {
    if (is.null(stim)) {
        warning("opiPresent: NULL stimulus")
        return(list(err = "The NULL stimulus not supported", seen = NA, time = NA))
    }

    .opi_env$Compass$minCheck(stim$x, .opi_env$Compass$MIN_X, "Stimulus x")
    .opi_env$Compass$maxCheck(stim$x, .opi_env$Compass$MAX_X, "Stimulus x")
    .opi_env$Compass$minCheck(stim$y, .opi_env$Compass$MIN_Y, "Stimulus y")
    .opi_env$Compass$maxCheck(stim$y, .opi_env$Compass$MAX_Y, "Stimulus y")
    .opi_env$Compass$minCheck(stim$duration, .opi_env$Compass$MIN_DURATION, "Stimulus duration")
    .opi_env$Compass$minCheck(stim$responseWindow, .opi_env$Compass$MIN_RESP_WINDOW, "Stimulus responseWindow")
    .opi_env$Compass$maxCheck(stim$responseWindow, .opi_env$Compass$MAX_RESP_WINDOW, "Stimulus responseWindow")
    .opi_env$Compass$minCheck(stim$t, .opi_env$Compass$MIN_DURATION, "Stimulus duration")
    .opi_env$Compass$minCheck(stim$w, .opi_env$Compass$MIN_RESP_WINDOW, "Stimulus responseWindow")
    .opi_env$Compass$maxCheck(stim$w, .opi_env$Compass$MAX_RESP_WINDOW, "Stimulus responseWindow")
    lev <- round(cdTodb(stim$level, .opi_env$Compass$ZERO_DB_IN_ASB / pi), 0)
    .opi_env$Compass$minCheck(lev, .opi_env$Compass$MIN_DB, "Stimulus level")
    .opi_env$Compass$maxCheck(lev, .opi_env$Compass$MAX_DB, "Stimulus level")

    if (!is.null(nextStim))
        warning("opiPresent: nextStim ignored")

    msg <- "OPI-PRESENT-STATIC"
    msg <- paste(msg, stim$x, stim$y, lev, "3", stim$duration, stim$responseWindow)

    present_bad <- TRUE
    present_count <- 0
    while (present_bad) {
        present_count <- present_count + 1
        if (present_count %% 10 == 0)
            warning(paste("opiPresent: I have tried presenting", present_count, "times."))

        writeLines(msg, .opi_env$Compass$socket)
        res <- readLines(.opi_env$Compass$socket, n = 1)
        s <- strsplit(res, " ", fixed = TRUE)[[1]]

        present_bad <- s[1] > 0
    }

    # BUG - num_track_events, etc are for a single call to the protocol, and not aggregated over the
    # 'present_bad' loop.

    return(list(
      err              = NULL,
      seen             = ifelse(s[2] == "1", TRUE, FALSE),    # assumes 1 or 0, not "true" or "false"
      time             = as.numeric(s[3]),
      time_hw          = as.numeric(s[4]),
      time_rec         = as.numeric(s[5]),
      time_resp        = as.numeric(s[6]),
      num_track_events = as.numeric(s[7]),
      num_motor_fails  = as.numeric(s[8]),
      pupil_diam       = as.numeric(s[9]),
      loc_x            = as.numeric(s[10]),
      loc_y            = as.numeric(s[11])
    ))
}

#' Implementation of opiSetup for the Compass machine.
#'
#' This is for internal use only. Use [opiSetup()] with
#' these Arguments and you will get the Value back.
#'
#' @usage NULL
#'
#' @param settings is a list that could contain:
#'   * \code{fixation} \code{c(x,y,t)} where
#'     * \code{x} is one of -20, -6, -3, 0, 3, 6, 20 degrees.
#'     * \code{y} is 0 degrees.
#'     * \code{t} is 0 for a spot fixation marker at \code{c(x,y)}, or 1 for a
#'                 square centred on one of \code{(-3,0)}, \code{(0,0)}, \code{(+3,0)}.
#'   * \code{tracking_on} \code{TRUE} for tracking on, \code{FALSE} for off
#'
#' @return
#'   A list containing \code{err} which is \code{NULL} for success, or some string description for fail.
#'
#' @details
#'   Note: tracking will be relative to the PRL established with the fixation
#'   marker used at setup (call to OPI-OPEN), so when tracking is on you should
#'   use the same fixation location as in the setup.
#'
opiSetup_for_Compass <- function(settings) {
    if ("tracking_on" %in% names(settings)) {
        tracking_on <- settings$tracking_on
        if (tracking_on) {
            writeLines("OPI-SET-TRACKING 1", .opi_env$Compass$socket)
            res <- readLines(.opi_env$Compass$socket, n = 1)
            s <- strsplit(res, " ", fixed = TRUE)[[1]]
            if (s[1] != 0) {
                return(list(err = paste("opiSetup: failed turn tracking on ", s[1])))
            }
        } else {
            writeLines("OPI-SET-TRACKING 0", .opi_env$Compass$socket)
            res <- readLines(.opi_env$Compass$socket, n = 1)
            s <- strsplit(res, " ", fixed = TRUE)[[1]]
            if (s[1] != 0) {
                return(list(err = paste("opiSetup: failed turn tracking off ", s[1])))
            }
        }
    }

    if ("fixation" %in% settings) {
        fixation <- settings$fixation
        if (length(fixation) != 3) {
            return(list(err = "opiSetup: fixation parameter must have 3 fields c(x,y,t)"))
        }
        x <- fixation[1]
        y <- fixation[2]
        t <- fixation[3]

        if (!(x %in% c(-20, -6, -3, 0, 3, 6, 20))) {
            return(list(err = "opiSetup: fixation x must be in c(-20, -6, -3, 0, 3, 6, 20)"))
        }
        if (y != 0) {
            return(list(err = "opiSetup: fixation y must be 0"))
        }
        if (t == 1 && (!(x %in% c(-3, 0, 3)))) {
            return(list(err = "opiSetup: fixation type 1 can only be at ({-3,0,+3}, 0)"))
        }
        writeLines(paste("OPI-SET-FIXATION",x,y,t), .opi_env$Compass$socket)
        res <- readLines(.opi_env$Compass$socket, n = 1)
        s <- strsplit(res, " ", fixed = TRUE)[[1]]
        if (s[1] != 0) {
            return(list(err = paste("opiSetBackground: failed to set fixation: ", s[1])))
        }
    }

    return(list(err = NULL))
}

###########################################################################
# return list(err=NULL, fixations=matrix of fixations)
#       matrix has one row per fixation
#       col-1 timestamp (ms since epoch)
#       col-2 x in degrees
#       col-3 y in degrees
###########################################################################

#' Implementation of opiClose for the Compass machine.
#'
#' This is for internal use only. Use [opiClose()] with
#' these Arguments and you will get the Value back.
#'
#' @usage NULL
#'
#' @return A list with elements
#'  * \code{err}, which is an error code, NULL for no error
#'  * \code{fixations}, which is a matrix one row per fixation and three columns:
#'    - \code{time} (same as \code{time_hw} in \code{opiPresent})
#'    - \code{x} (degrees relative to the centre of the image returned by \code{opiInitialise} - not the PRL)
#'    - \code{y} (as for x)
#
opiClose_for_Compass <- function() {
    writeLines("OPI-CLOSE", .opi_env$Compass$socket)

    num_bytes <- readBin(.opi_env$Compass$socket, "integer", size = 4, endian = .opi_env$Compass$endian)
    print(paste("Num bytes", num_bytes))

    if (num_bytes == 0) {
        warning("opiClose() returned no bytes, dunno what that means")
        close(.opi_env$Compass$socket)
        rm("socket", envir = .opi_env$Compass)
        return(list(err = "No Bytes, connection closed."))
    }

    num_triples <- num_bytes / 12
    fixations <- matrix(NA, ncol = 3, nrow = num_triples)
    for (i in 1:num_triples) {
        fixations[i, 1] <- readBin(.opi_env$Compass$socket, "integer", n = 1, size = 4, endian = .opi_env$Compass$endian)
        fixations[i, 2:3] <- readBin(.opi_env$Compass$socket, "double", n = 2, size = 4,  endian = .opi_env$Compass$endian)
    }

    close(.opi_env$Compass$socket)
    rm("socket", envir = .opi_env$Compass)

    return(list(err = NULL, fixations = fixations))
}

#' Implementation of opiQueryDevice for the ImoVifa machine.
#'
#' This is for internal use only. Use [opiQueryDevice()] with
#' these Arguments and you will get the Value back.
#'
#' @usage NULL
#'
#' @return A list containing constants and their values used in the OPI Compass module.
opiQueryDevice_for_Compass <- function() {
    return(lapply(ls(.opi_env$Compass), function(v) c(as.character(v), get(v, .opi_env$Compass))))
}