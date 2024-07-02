#
# MOCS algorithm for a single location, nAFC possible with beeps.
#
# Author: Andrew Turpin
#         (Based on discussions with Tony Redmond July 2012).
# Date: May 2015
# Modified Tue 21 Mar 2023: changed licence from gnu to Apache 2.0
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

#' @rdname MOCS
#' @title Method of Constant Stimuli (MOCS)
#' @description MOCS performs either a yes/no or n-interval-forced-choice Method of
#' Constant Stimuli test
#' @param params A matrix where each row is \code{x y i n correct_n ll1 ll2 ... llm} where
#'   * \code{x} is X coordinate of location
#'   * \code{y} is Y coordinate of location
#'   * \code{i} is a location number (assigned by caller)
#'   * \code{n} is Number of times this location/luminance(s) should be repeated
#'   * \code{correct_n} is the index i of the luminance level (\code{lli}) that
#'       should be treated as a ``correct'' response (the correct interval). For a
#'       standard MOCS, this will be 1; for a 2AFC, this will be 1 or 2. This number will
#'       be in the range \code{[1,m]}.
#'   * \code{lli} is the i'th luminance level to be used at this location for
#'       interval i of the presentation in cd/\eqn{\mbox{m}^2}{m^2}. For a standard MOCS,
#'       i=1, and the \code{params} matrix will have 5 columns. For a 2AFC, there will be
#'       two lli's, and \code{params} will have 6 columns.
#'
#' @param order Control the order in which the stimuli are presented.
#'   * \code{"random"} Randomise the order of trials/locations.
#'   * \code{"fixed"} Present each row of \code{params} in order of \code{1:nrow(params)}, ignoring the \code{n} (4th) column in \code{params}.
#'
#' @param responseWindowMeth Control time perimeter waits for response.
#'   * \code{"speed"} After an average of the last \code{speedHistory} response times, with a minimum of \code{responseFloor}. Initially #'       \code{responseFloor}.
#'   * \code{"constant"} Always use \code{responseFloor}.
#'   * \code{"forceKey"} Wait for a keyboard input.
#'
#' @param responseFloor Minimum response window (for any \code{responseWindowMeth} except \code{"forceKey"}).
#' @param responseHistory Number of past yeses to average to get response window
#'   (only used if \code{responseWindowMeth} is \code{"speed"}).
#' @param keyHandler Function to get a keyboard input and returns as for \code{opiPresent}:
#'   \code{err}, \code{seen} and \code{time}. The parameters passed to
#'   the function are the correct interval number (column 4 of \code{params}), and the
#'   result of \code{opiPresent}. See Examples.
#' @param interStimMin Regardless of response, wait \code{runif(interStimMin, interStimMax)} ms.
#' @param interStimMax Regardless of response, wait \code{runif(interStimMin, interStimMax)} ms.
#' @param beep_function A function that takes the string \code{'correct'}, the string
#' \code{'incorrect'}, or a stimulus number and plays an appropriate sound.  See examples.
#' @param makeStim A helper function to take a row of \code{params} and a response window length
#' in ms, and create a list of OPI stimuli types for passing to opiPresent. This may include a
#' \code{checkFixationOK} function. See Example.
#' @param stim_print A function that takes an \code{opiStaticStimulus} and return list from
#' \code{opiPresent} and returns a string to print for each presentation. It is called
#' immediately after each \code{opiPresent}, and the string is prepended with the
#' (x,y) coordinates of the presentation and ends with a newline.
#' @param ... Extra parameters to pass to the opiPresent function.
#' @details Whether the test is yes/no or forced-choice is determined by the number of columns
#' in \code{params}. The code simply presents all columns from 5 onwards and collects a
#' response at the end. So if there is only 5 columns, it is a yes/no task. If there are 6
#' columns it is a 2-interval-forced-choice. Generally, an nIFC experiment has 4+n columns in
#' \code{params}.
#'
#' Note that when the \code{order} is \code{"random"}, the number of trials in the test will be
#' the sum of the 3rd column of \code{params}. When the \code{order} is \code{"fixed"}, there is
#' only one presentation per row, regardless of the value in the 3rd column of \code{params}.
#'
#' If a response is received before the final trial in a nIFC experiment, it is ignored.
#'
#' If the \code{checkFixationOK} function is present in a stimulus, then it is called after each
#' presentation, and the result is ``anded'' with each stimulus in a trial to get a TRUE/FALSE
#' for fixating on all stimuli in a trial.
#' 
#' @return Returns a data.frame with one row per stimulus copied from params with extra columns
#' appended: checkFixation checks, and the return values from \code{opiPresent()}
#' (see example). These last values will differ depending on which
#' machine/simulation you are running (as chosen with \code{chooseOpi()}.
#'   * column 1: x
#'   * column 2: y
#'   * column 3: location number
#'   * column 4: number of times to repeat this stim
#'   * column 5: correct stimulus index
#'   * column 6: TRUE/FALSE was fixating for all presentations in this trial according to \code{checkFixationOK}
#'   * column 7...: columns from params
#'   * ...: columns from opiPresent return
#'
#' @references
#' A. Turpin, P.H. Artes and A.M. McKendrick. "The Open Perimetry Interface: An enabling tool for
#' clinical visual psychophysics", Journal of Vision 12(11) 2012.
#' @seealso \code{\link{dbTocd}}, \code{\link{opiPresent}}
#' @examples
#' # For the Octopus 900
#' # Check if pupil centre is within 10 pixels of (160,140)
#' checkFixationOK <- function(ret) return(sqrt((ret$pupilX - 160)^2 + (ret$pupilY - 140)^2) < 10)
#'
#' # Return a list of opi stim objects (list of class opiStaticStimulus) for each level (dB) in
#' # p[5:length(p)]. Each stim has responseWindow BETWEEN_FLASH_TIME, except the last which has
#' # rwin. This one assumes p is on old Octopus 900 dB scale (0dB == 4000 cd/m^2).
#' makeStim <- function(p, rwin) {
#'   BETWEEN_FLASH_TIME <- 750   # ms
#'   res <- NULL
#'   for(i in 5:length(p)) {
#'     s <- list(x=p[1], y=p[2], level=dbTocd(p[i],4000/pi), size=0.43, duration=200,
#'               responseWindow=ifelse(i < length(p), BETWEEN_FLASH_TIME, rwin),
#'               checkFixationOK=NULL)
#'     class(s) <- "opiStaticStimulus"
#'     res <- c(res, list(s))
#'   }
#'   return(res)
#' }
#'
#' ################################################################
#' # Read in a key press 'z' is correct==1, 'm' otherwise
#' #    correct is either 1 or 2, whichever is the correct interval
#' #
#' # Return list(seen={TRUE|FALSE}, time=time, err=NULL))
#' #        seen is TRUE if correct key pressed
#' ################################################################
#' \dontrun{
#'   if (length(dir(".", "getKeyPress.py")) < 1)
#'     stop('Python script getKeyPress.py missing?')
#' }
#'
#' keyHandler <- function(correct, ret) {
#'   return(list(seen=TRUE, time=0, err=NULL))
#'   ONE <- "b'z'"
#'   TWO <- "b'm'"
#'   time <- Sys.time()
#'   key <- 'q'
#'   while (key != ONE && key != TWO) {
#'     a <- system('python getKeyPress.py', intern=TRUE)
#'     key <- a # substr(a, nchar(a), nchar(a))
#'     print(paste('Key pressed: ',key,'from',a))
#'     if (key == "b'8'")
#'       stop('Key 8 pressed')
#'   }
#'   time <- Sys.time() - time
#'   if ((key == ONE && correct == 1) || (key == TWO && correct == 2))
#'     return(list(seen=TRUE, time=time, err=NULL))
#'   else
#'     return(list(seen=FALSE, time=time, err=NULL))
#' }
#'
#' ################################################################
#' # Read in return value from opipresent with F310 controller.
#' # First param is correct, next is 1 for left button, 2 for right button
#' # Left button (LB) is correct for interval 1, RB for interval 2
#' #    correct is either 1 or 2, whichever is the correct interval
#' #
#' # Return list(seen={TRUE|FALSE}, time=time, err=NULL))
#' #        seen is TRUE if correct key pressed
#' ################################################################
#' F310Handler <- function(correct, opiResult) {
#'   z <- opiResult$seen == correct
#'   opiResult$seen <- z
#'   return(opiResult)
#' }
#'
#' ################################################################
#' # 2 example beep_function
#' ################################################################
#' \dontrun{
#'   require(beepr)
#'   myBeep <- function(type='None') {
#'     if (type == 'correct') {
#'       beepr::beep(2)  # coin noise
#'       Sys.sleep(0.5)
#'     }
#'     if (type == 'incorrect') {
#'       beepr::beep(1) # system("rundll32 user32.dll,MessageBeep -1") # system beep
#'       #Sys.sleep(0.0)
#'     }
#'   }
#'   require(audio)
#'   myBeep <- function(type="None") {
#'     if (type == 'correct') {
#'       wait(audio::play(sin(1:10000/10)))
#'     }
#'     if (type == 'incorrect') {
#'       wait(audio::play(sin(1:10000/20)))
#'     }
#'   }
#' }
#'
#' ################################################################
#' # An example stim_print function
#' ################################################################
#' \dontrun{
#'   stim_print <- function(s, ret) {
#'     sprintf("%4.1f %2.0f",cdTodb(s$level,10000/pi), ret$seen)
#'   }
#' }
#' @export
MOCS <- function(params = NA,
                 order = "random",
                 responseWindowMeth = "constant",
                 responseFloor = 1500,
                 responseHistory = 5,
                 keyHandler = function(correct, ret) return(list(seen = TRUE, time = 0, err = NULL)),
                 interStimMin = 200,
                 interStimMax = 500,
                 beep_function,
                 makeStim,
                 stim_print, ...) {

        ################################################
        # expand the params matrix to every presentation
        # and order of rows in the matrix appropriately
        ################################################
    mocs <- NULL
    if (order == "random") {
        for (i in 1:nrow(params)) {
            reps <- params[i, 4]
            mocs <- rbind(mocs, matrix(params[i, ], ncol = ncol(params), nrow = reps, byrow = TRUE))
        }
        mocs <- mocs[order(stats::runif(nrow(mocs))), ]
    } else if (order == "fixed") {
        mocs <- params
    } else {
        stop(paste("Invalid order in MOCS: ", order))
    }
    mocs <- rbind(mocs, rep(0, ncol(mocs)))  # add dummy for final nextStim

        ####################################################
        # Set up response window time data structures
        ####################################################
    if (responseWindowMeth == "speed") {
        respTimeHistory <- rep(responseFloor, responseHistory)
    } else if (!is.element(responseWindowMeth, c("constant", "forceKey"))) {
        stop(paste("Invalid responseWindowMeth in MOCS: ", responseWindowMeth))
    }

        ####################################################
        # loop through every presentation except last (is dummy)
        ####################################################
    error_count <- 0
    results <- NULL
    nextStims <- makeStim(as.double(mocs[1, ]), responseFloor)
    for (i in 1:(nrow(mocs) - 1)) {
        if (responseWindowMeth == "constant") {
            rwin <- responseFloor
        } else if (responseWindowMeth == "forceKey") {
            rwin <- 0
        } else {
            rwin <- max(responseFloor, mean(respTimeHistory))
        }
        stims     <- nextStims
        nextStims <- makeStim(as.double(mocs[i + 1, ]), rwin)

        cat(sprintf('\nTrial,%g,Location,%g', i, mocs[i, 3]))
        all_fixations_good <- TRUE
        for (stimNum in 1:length(stims)) {
            beep_function(stimNum)
            s <- stims[[stimNum]]

            if (stimNum == length(stims)) {
              ret <- opiPresent(stim = s, nextStim = nextStims[[stimNum]], ...)
            } else {
              startTime <- Sys.time()
              ret <- opiPresent(stim = s, nextStim = NULL, ...)
            }

            fixation_good <- TRUE
            if (!is.null(s$checkFixationOK))
                fixation_good <- s$checkFixationOK(ret)
            all_fixations_good <- all_fixations_good && fixation_good

            cat(sprintf(",%f,%f,%f,", s$x, s$y, fixation_good))
            cat(stim_print(s, ret))

            if (stimNum < length(stims)) {
                # just check that the response window wasn't shortened by a response
              while (Sys.time() - startTime < s$responseWindow / 1000)
                Sys.sleep(0.05)
            }
        }

        if (responseWindowMeth == "forceKey")
          res <- keyHandler(mocs[i, 6], res)

        if (is.null(res$err)) {
            if (res$seen)
                beep_function('correct')
            else
                beep_function('incorrect')

            if (res$seen && responseWindowMeth == "speed")
                respTimeHistory <- c(utils::tail(respTimeHistory, -1), res$time)
        } else {
            warning("Opi Present return error in MOCS")
            error_count <- error_count + 1
        }

        cat(sprintf(',%g,%g\n', res$seen,  res$time))

        Sys.sleep(stats::runif(1, min = interStimMin, max = interStimMax)/1000)

        results <- rbind(results, c(mocs[i, 1:5], all_fixations_good, mocs[i, 6:ncol(mocs)], res))
    }

    if (error_count > 0)
        warning(paste("There were", error_count, "Opi Present return errors in MOCS"))

    return(results)
}#MOCS()

###################################
# tests
###################################
###t0 <- matrix(c(
###    9,9, 3, 1, 3145  ,
###    6,6, 4, 1,  313  ,
###    3,3, 5, 1,   31.4
###), ncol = 5, byrow = TRUE)
###
###t1 <- matrix(c(
###    9,9, 3, 1, 3145  , 314,
###    6,6, 4, 2,  313  , 314,
###    3,3, 5, 2,   31.4, 314
###), ncol = 6, byrow = TRUE)
###
###a1 <- list(sin(1:10000/20), sin(1:10000/20), sin(1:10000/10), sin(1:10000/30))
###
###BETWEEN_FLASH_TIME <- 500   # ms
###
###makeStim <- function(p, rwin) {
###    res <- NULL
###    for(i in 5:length(p)) {
###
###        s <- list(x = p[1], y = p[2], level = p[5], size = 0.43, duration = 200,
###                  responseWindow = ifelse(i < length(p), 0, BETWEEN_FLASH_TIME),
###                  checkFixationOK = NULL
###             )
###        class(s) <- "opiStaticStimulus"
###        res <- c(res, list(s))
###    }
###    return(res)
###}
###
#### correct is either 1 or 2, whichever is the correct interval
###keyHandler <- function(correct) {
###    time <- Sys.time()
###    key <- 'q'
###    while (key != 'z' && key != 'm') {
###        a <- system('python getKeyPress.py', intern=TRUE)
###        key <- substr(a, nchar(a), nchar(a))
###    }
###    time <- Sys.time() - time
###    #print(paste('Key pressed: ',key))
###
###    if ((key == 'z' && correct == 1) || (key == 'm' && correct == 2))
###        return(list(seen=TRUE, time=time, err=NULL))
###    else
###        return(list(seen=FALSE, time=time, err=NULL))
###}
###
###require(OPI)
####chooseOpi("SimNo")
###chooseOpi("SimHenson")
###opiInitialise()
####r <- MOCS(params=t0, interStimMin=0, interStimMax=0, makeStim=makeStim, tt=10, fpr=0.0, fnr=0)
####r <- MOCS(params=t1, interStimMin=0, interStimMax=0, makeStim=makeStim, tt=10, fpr=0, fnr=0)
####r <- MOCS(params=t1, audio=a1, responseWindowMeth="forceKey", keyHandler=keyHandler, interStimMin=0, interStimMax=0, makeStim=makeStim, tt=10, fpr=0, fnr=0)
###
####print(r)
###
###opiClose()
