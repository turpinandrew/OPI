#
# OPI for MAIA
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
# .opi_env$MAIA$socket is the connection to the MAIA
# .opi_env$MAIA$...    a variety of constants, etc
###################################################################
if (exists(".opi_env") && !exists("MAIA", where = .opi_env)) {
    assign("MAIA", new.env(25), envir = .opi_env)
    .opi_env$MAIA$endian <- "little"   # endian-ness of the MAIA OS

    .opi_env$MAIA$ZERO_DB_IN_ASB <- 10000

    .opi_env$MAIA$MAX_DB <- 36
    .opi_env$MAIA$MIN_DB <- 0
    .opi_env$MAIA$MIN_X  <- -30
    .opi_env$MAIA$MAX_X  <- 30
    .opi_env$MAIA$MIN_Y  <- -30
    .opi_env$MAIA$MAX_Y  <- 30
    .opi_env$MAIA$MIN_RESP_WINDOW  <- 0
    .opi_env$MAIA$MAX_RESP_WINDOW  <- 2000
    .opi_env$MAIA$MIN_DURATION  <- 1
    .opi_env$MAIA$STIM_COLORS  <- c('w', 'c', 'r')
    .opi_env$MAIA$FIX_LOC  <- c(-4.8, -2.4, 0, 2.4, 4.8)
    .opi_env$MAIA$MIN_FIX_POWER <- 0
    .opi_env$MAIA$MAX_FIX_POWER <- 1023

    .opi_env$MAIA$SEEN     <- 1
    .opi_env$MAIA$NOT_SEEN <- 0

    # Utility functions for validating inputs
    .opi_env$MAIA$minCheck <- function(x, limit, txt) {
        if (is.null(x))
            return()

        if (x < limit) {
            opiClose()
            stop(paste("opiPresent: ", txt, "is too small (minimum ", limit, ")"))
        }
    }
    .opi_env$MAIA$maxCheck <- function(x, limit, txt) {
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
#' @param ... Could be used for fake MAIA, simulations, etc
#'
#' @return A list with elements:
#'  * \code{err} NULL if successful, not otherwise.
#'
#' @details
#'   Establishes socket connection but sends not messages (see `opiSetup_for_MAIA`).
#'
#' @examples
#' \dontrun{
#'   # Set up the MAIA
#'   chooseOpi("MAIA")
#'   result <- opiInitialize(ip = "192.168.1.7", port = 5555)
#'   if (is.null(result$err))
#'     print(result$err)
#' }
opiInitialise_for_MAIA <- function(ip = "192.168.1.2", port = 5555, ...) {
    if ("socket" %in% ls(envir = .opi_env$MAIA))
        return(list(err = "MAIA already connected. Did you opiClose()?", prl = NULL, onh = NULL, image = NULL))

    cat("\nLooking for server ...")
    socket <- tryCatch(
        socketConnection(host = ip, port, open = "w+b", blocking = TRUE, timeout = 1000),
        error = function(e) stop(paste("Cannot connect to MAIA at", ip, "on port", port))
    )
    cat("found at", ip, port, ":)\n")

    assign("socket", socket, envir = .opi_env$MAIA)

    assign("machine_is_initialised", TRUE, .opi_env)

    return(err = NULL)
}

#' Implementation of opiPresent for the MAIA machine.
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
#'  * \code{time_rec} Time since epoch when command was received at MAIA (integer ms)
#'  * \code{time_pres} Time since epoch that stimulus was presented (integer ms)
#'  * \code{num_track_events} Number of tracking events that occurred during presentation (integer)
#'  * \code{num_motor_fails} Number of times motor could not follow fixation movement during presentation (integer)
#'  * \code{pupil_diam} Pupil diameter in mm (float)
#'  * \code{loc_x} X location in image of presentation (pixels, integer)
#'  * \code{loc_y} Y location in image of presentation (pixels, integer)
#'
#' @details
#'   If the chosen OPI implementation is \code{MAIA}, then \code{nextStim}
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
#'   # Set up the MAIA
#'   chooseOpi("MAIA")
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
opiPresent_for_MAIA <- function(stim, nextStim = NULL) {
    if (is.null(stim)) {
        warning("opiPresent: NULL stimulus")
        return(list(err = "The NULL stimulus not supported", seen = NA, time = NA))
    }

    .opi_env$MAIA$minCheck(stim$x, .opi_env$MAIA$MIN_X, "Stimulus x")
    .opi_env$MAIA$maxCheck(stim$x, .opi_env$MAIA$MAX_X, "Stimulus x")
    .opi_env$MAIA$minCheck(stim$y, .opi_env$MAIA$MIN_Y, "Stimulus y")
    .opi_env$MAIA$maxCheck(stim$y, .opi_env$MAIA$MAX_Y, "Stimulus y")
    .opi_env$MAIA$minCheck(stim$duration, .opi_env$MAIA$MIN_DURATION, "Stimulus duration")
    .opi_env$MAIA$minCheck(stim$responseWindow, .opi_env$MAIA$MIN_RESP_WINDOW, "Stimulus responseWindow")
    .opi_env$MAIA$maxCheck(stim$responseWindow, .opi_env$MAIA$MAX_RESP_WINDOW, "Stimulus responseWindow")
    .opi_env$MAIA$minCheck(stim$t, .opi_env$MAIA$MIN_DURATION, "Stimulus duration")
    .opi_env$MAIA$minCheck(stim$w, .opi_env$MAIA$MIN_RESP_WINDOW, "Stimulus responseWindow")
    .opi_env$MAIA$maxCheck(stim$w, .opi_env$MAIA$MAX_RESP_WINDOW, "Stimulus responseWindow")
    lev <- round(cdTodb(stim$level, .opi_env$MAIA$ZERO_DB_IN_ASB / pi), 0)
    .opi_env$MAIA$minCheck(lev, .opi_env$MAIA$MIN_DB, "Stimulus level")
    .opi_env$MAIA$maxCheck(lev, .opi_env$MAIA$MAX_DB, "Stimulus level")

    if (! "id" %in% names(stim)) {
        warning("opiPresent: no id for stim provided, constructing as 1000*round(x,3) + y")
        stim$id <- 1000 * round(stim$x, 3) + stim$y
    }

    if ("color" %in% names(stim) && !(stim$color %in% .opi_env$MAIA$STIM_COLORS)) {
        warning("opiPresent: color not in STIM_COLORS")
        return(list(err = sprintf("opiPresent: color %s not in %s",
            stim$color, paste(.opi_env$MAIA$STIM_COLORS, collapse = " ")), seen = NA, time = NA))
    } else {
        stim$color <- "w"
    }

    if (!is.null(nextStim))
        warning("opiPresent: nextStim ignored")

    msg <- "OPI-PRESENT-STATIC"
    msg <- paste(msg, stim$id, stim$x, stim$y, stim$color, 1, lev, 0.43, stim$duration, stim$responseWindow)

    present_bad <- TRUE
    present_count <- 0
    while (present_bad) {
        present_count <- present_count + 1
        if (present_count %% 10 == 0)
            warning(paste("opiPresent: I have tried presenting", present_count, "times."))

#print(paste(present_count, "Sending opipresent:", Sys.time()))
        writeLines(msg, .opi_env$MAIA$socket)
#print(paste(present_count, "Reading opipresent:", Sys.time()))
        #res <- readLines(.opi_env$MAIA$socket, n = 1)
        res <- NULL
        keepGoing <- TRUE
        while(keepGoing) {
            cc <- readBin(.opi_env$MAIA$socket, "raw", size = 1, endian = .opi_env$MAIA$endian)
            if (cc == 0x0a)
                keepGoing <- FALSE
            else
                res <- c(res, cc)
        }
#print(paste(present_count, "Done opipresent   :", Sys.time()))
        s <- strsplit(rawToChar(res), " ", fixed = TRUE)[[1]]

        present_bad <- s[1] > 0
    }

    # BUG - num_track_events, etc are for a single call to the protocol, and not aggregated over the
    # 'present_bad' loop.

    return(list(
      err              = NULL,
      seen             = ifelse(s[2] == "1", TRUE, FALSE),
      time             = as.numeric(s[3]),
      time_hw          = as.numeric(s[4]),
      time_rec         = as.numeric(s[5]),
      time_resp        = as.numeric(s[6]),
      num_track_events = as.numeric(s[7]),
      num_motor_fails  = as.numeric(s[8]),
      loc_x            = as.numeric(s[9]),
      loc_y            = as.numeric(s[10])
    ))
}

#' Implementation of opiSetup for the MAIA machine.
#'
#' This is for internal use only. Use [opiSetup()] with
#' these Arguments and you will get the Value back.
#'
#' @usage NULL
#'
#' @param settings is a list that could contain:
#'   * \code{fixation} \code{list(x,y,t,c)} where
#'     * \code{x} is one of 0, +-2.4, +-4.8 degrees.
#'     * \code{y} is one of 0, +-2.4, +-4.8 degrees.
#'     * \code{t} is 0 for big circle off, 1 for big circle on.
#'     * \code{p} power integer in range 0..1023.
#'   * \code{bg} which is 0 for no background and other for some background
#'   * \code{tracking} which is 0 for no tracking and other for some tracking
#'   * \code{open} If present, will send OPI-OPEN to MAIA and look for prl, onh, fundus image
#'
#' @return
#'   A list containing
#'     * \code{err} which is \code{NULL} for success
#'     * \code{message} for some messages from MAIA (probably "0")
#'  For `open` list might (but might not) contain:
#'    * \code{prl} A pair giving the (x,y) in degrees of the Preferred Retinal
#'       Locus detected in the initial alignment.
#'    * \code{onh} a pair giving the (x,y) in degrees of the ONH as selected by the user.
#'    * \code{image} raw bytes being the JPEG compressed infra-red image acquired during alignment.
#'
#' @details
#' Note that corner locations are missing, so both `x` and `y` cannot be +-4.8.
#'
opiSetup_for_MAIA <- function(settings) {
    error <- ""
    extra_return <- NULL

    if ("fixation" %in% names(settings)) {
        if (length(settings$fixation) != 4)
            return(list(err = "opiSetup: fixation must be a list of length 4"))

        if (!all(sort(names(settings$fixation)) == c("p", "t", "x", "y")))
            return(list(err = "opiSetup: fixation list must have names x, y, t, p"))

        x <- settings$fixation$x
        y <- settings$fixation$y
        t <- settings$fixation$t
        p <- settings$fixation$p

        .opi_env$MAIA$minCheck(p, .opi_env$MAIA$MIN_FIX_POWER, "Fixation power")
        .opi_env$MAIA$maxCheck(p, .opi_env$MAIA$MAX_FIX_POWER, "Fixation power")

        if (! x %in% .opi_env$MAIA$FIX_LOC)
            return(list(err = sprintf("opiSetup: fixation x must be one of %s", paste(.opi_env$MAIA$FIX_LOC, collapse = " "))))
        if (! y %in% .opi_env$MAIA$FIX_LOC)
            return(list(err = sprintf("opiSetup: fixation y must be one of %s", paste(.opi_env$MAIA$FIX_LOC, collapse = " "))))

        if (abs(x) == 4.8 && abs(y) == 4.8)
            return(list(err = "opiSetup: fixation x and y cannot both be +-4.8"))

        writeLines(sprintf("OPI-SET-FIXATION %s %s %s %s", x, y, t, p), .opi_env$MAIA$socket)
        res <- readLines(.opi_env$MAIA$socket, n = 1)
        if (substr(res, 1, 1) == "1")
            error <- paste(error, "\nopiSetup(fixation) failed")
    }

    if ("bg" %in% names(settings)) {
        bg <- ifelse(settings$bg == 0, 0, 1)

        writeLines(sprintf("OPI-SET-BACKGROUND %s", bg), .opi_env$MAIA$socket)
        res <- readLines(.opi_env$MAIA$socket, n = 1)
        if (substr(res, 1, 1) == "1")
            error <- paste(error, "\nopiSetup(bg) failed")
    }

    if ("tracking" %in% names(settings)) {
        trackingOn <- ifelse(settings$tracking == 0, 0, 1)

        writeLines(sprintf("OPI-SET-TRACKING %s", trackingOn), .opi_env$MAIA$socket)
        res <- readLines(.opi_env$MAIA$socket, n = 1)
        if (substr(res, 1, 1) == "1")
            error <- paste(error, "\nopiSetup(tracking) failed")

        #res <- readBin(.opi_env$MAIA$socket, "integer", size = 4, endian = .opi_env$MAIA$endian)
        #if (res == 1)
        #    error <- paste(error, "\nopiSetup(tracking) failed")
    }

    if ("open" %in% names(settings)) {
        msg <- "OPI-OPEN"
        writeLines(msg, .opi_env$MAIA$socket)
        n <- readBin(.opi_env$MAIA$socket, "integer", size = 4, endian = .opi_env$MAIA$endian)
print(paste0("OPI-OPEN: n=", n))
        if (n > 15) {
            prlx <- readBin(.opi_env$MAIA$socket, "double", size = 4, endian = .opi_env$MAIA$endian)
            prly <- readBin(.opi_env$MAIA$socket, "double", size = 4, endian = .opi_env$MAIA$endian)
            onhx <- readBin(.opi_env$MAIA$socket, "double", size = 4, endian = .opi_env$MAIA$endian)
            onhy <- readBin(.opi_env$MAIA$socket, "double", size = 4, endian = .opi_env$MAIA$endian)
            im <- openssl::base64_encode(readBin(.opi_env$MAIA$socket, "raw", n = (n - 16), size = 1, endian = .opi_env$MAIA$endian))

            extra_return <- list(err = NULL, prl = c(prlx, prly), onh = c(onhx, onhy), image = im)
        } else if (n != -1) {
            error <- paste(error, "\nopiSetup(open) failed")
        }
    }

    if (nchar(error) == 0)
        return(c(list(err = NULL), extra_return))
    else
        return(c(list(err = error), extra_return))
}

###########################################################################
# return list(err=NULL, fixations=matrix of fixations)
#       matrix has one row per fixation
#       col-1 timestamp (ms since epoch)
#       col-2 x in degrees
#       col-3 y in degrees
###########################################################################

#' Implementation of opiClose for the MAIA machine.
#'
#' This is for internal use only. Use [opiClose()] with
#' these Arguments and you will get the Value back.
#'
#' @usage NULL
#'
#' @return A list with elements
#'  * \code{err}, which is an error code, NULL for no error
#'  * \code{fixations}, which is a matrix one row per fixation and four columns:
#'    - \code{time} (same as \code{time_hw} in \code{opiPresent})
#'    - \code{flags} Bit 0(same as \code{time_hw} in \code{opiPresent})
#'       * bit 0: 1 if the tracking event is valid (quality>threshold), 0 otherwise
#'         (in this case, x and y shall be =0)
#'       * bit 2: 1 if a stimulus was being presented during the tracking event, 0 otherwise
#'       * bit 3: 1 if the tracking was enabled during the tracking event, 0 otherwise
#'       * bits 16..31: ID of the stimulus being presented (if bit 2 == 1)
#'    - \code{x} (degrees relative to the centre of the image returned by \code{opiInitialise} - not the PRL)
#'    - \code{y} (as for x)
#
opiClose_for_MAIA <- function() {
    writeLines("OPI-CLOSE", .opi_env$MAIA$socket)

    num_bytes <- readBin(.opi_env$MAIA$socket, "integer", size = 4, endian = .opi_env$MAIA$endian)
    print(paste("Num bytes", num_bytes))

    if (num_bytes == 0) {
        warning("opiClose() returned no bytes, dunno what that means")
        close(.opi_env$MAIA$socket)
        rm("socket", envir = .opi_env$MAIA)
        return(list(err = "No Bytes, connection closed."))
    }

    num_tuples <- num_bytes / 16
    fixations <- matrix(NA, ncol = 4, nrow = num_tuples)
    for (i in seq_len(num_tuples)) {
        fixations[i, 1] <- readBin(.opi_env$MAIA$socket, "integer", n = 1, size = 4, endian = .opi_env$MAIA$endian)
        fixations[i, 2] <- readBin(.opi_env$MAIA$socket, "integer", n = 1, size = 4, endian = .opi_env$MAIA$endian)
        fixations[i, 3:4] <- readBin(.opi_env$MAIA$socket, "double", n = 2, size = 4,  endian = .opi_env$MAIA$endian)
    }

    close(.opi_env$MAIA$socket)
    rm("socket", envir = .opi_env$MAIA)

    return(list(err = NULL, fixations = fixations))
}

#' Implementation of opiQueryDevice for the ImoVifa machine.
#'
#' This is for internal use only. Use [opiQueryDevice()] with
#' these Arguments and you will get the Value back.
#'
#' @usage NULL
#'
#' @return A list containing constants and their values used in the OPI MAIA module.
opiQueryDevice_for_MAIA <- function() {
    return(lapply(ls(.opi_env$MAIA), function(v) c(as.character(v), get(v, .opi_env$MAIA))))
}