#
# MOCS algorithm for a single location, nAFC possible with beeps.
#
# Author: Andrew Turpin    (aturpin@unimelb.edu.au)
#         (Based on disucssions with Tony Redmond July 2012).
# Date: May 2015
#
# Copyright 2015 Andrew Turpin
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

################################################################################
# Perform a MOCS, possibly with alternate-forced-choice stimuli.
# The number of AFC are given by the number of columns in params.
#
# Input parameters
#   params  A matrix where each row is 
#           x y loc_num number-of-present'ns correct_lum_num luminance-level-1 ll2 ll3 ...
#           Each row of params is presented number-of-presentations times in the
#           order determined by the "order" paramter. For a yes/no MOCS, there is 
#           only one luminance level. For @AFC, there are two, etc.
#
#   order     Control the order in which the stimuli are presented.
#               "random" - uniform random for all trials.
#               "fixed"   - just present in order of 1:nrow(params), ignoring 
#                           number-of-presentations column.
#
#   responseWindowMeth Control time perimeter waits for response. 
#               "speed" - after an average of the last 'speedHistory' 
#                         response times, with a minimum of 'responseFloor'.
#                         Initially responseFloor.
#               "constant" - always use responseFloor.
#               "forceKey" - wait for a keyboard input.
#
#   responseHistory - number of past yesses to average to get reposnse window
#                      (only used if responseWindowMeth == "speed")
#
#   responseFloor Minimum response window (for any responseWindowMeth except forceKey). 
#                   
#   keyHandler   Function to get a keyboard input and returns as for opiPresent:
#                list(seen={TRUE|FALSE}, response time (in ms), error code).
#                Param to function is correct lum level (col 4 of params), and 
#                result of opiPresent.
#                   
#   interStimMin Regardless of response, wait runif(interStimMin, interStimMax) ms.
#   interStimMax
#
#   beep_function A funtion that takes 'correct', 'incorrect', or a stimulus number
#                 and plays an appropriate sound.
#
#   makeStim  A helper function to take a row of params[] and a response window 
#             length in ms, and create a list of OPI stimuli types for 
#             passing to opiPresent. Might include checkFixationOK function.
#
#   stim_print A function that takes opiStaticStimulus and return list from opiPresent
#              and returns a string to print.
#
#   ...       Parameters for opiPresent
#
# Returns a data.frame with one row per stim, 
#       col 1 Location number (row number in input params matrix)
#       col 2 x 
#       col 3 y 
#       col 4 correct_lum_num 
#       col 5 true/false all fixations in trial good according to checkFixationOK (TRUE if no checkFixationOK)
#       ncol(params)-1 are same as params[5:],
#       column last-2 = correct/incorrect
#       column last-1 = response time 
#       column last   = err code
#
# Also prints x,y,fixations_good,stim_print(stim, return) for each trial
################################################################################
MOCS <- function(params=NA, 
                 order="random",
                 responseWindowMeth="constant", 
                 responseFloor=1500, 
                 responseHistory=5, 
                 keyHandler=function(correct,ret) return(list(TRUE, 0, NULL)),
                 interStimMin=200,
                 interStimMax=500,
                 beep_function,
                 makeStim,
                 stim_print, ...) {

        ################################################
        # expand the params matrix to every presentation
        # and order of rows in the matrix appropriately
        ################################################
    mocs <- NULL
    if (order == "random") {
        for(i in 1:nrow(params)) {
            reps <- params[i,4]
            mocs <- rbind(mocs, matrix(params[i,], ncol=ncol(params), nrow=reps, byrow=T))
        }
        mocs <- mocs[order(runif(nrow(mocs))), ]
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
    nextStims <- makeStim(as.double(mocs[1,]), responseFloor)
    for(i in 1:(nrow(mocs)-1)) {
        if (responseWindowMeth == "constant") {
            rwin <- responseFloor
        } else if (responseWindowMeth == "forceKey") {
            rwin <- 0
        } else {
            rwin <- max(responseFloor, mean(respTimeHistory))
        } 
        stims     <- nextStims
        nextStims <- makeStim(as.double(mocs[i+1,]), rwin)

        cat(sprintf('Trial,%g,Location,%g',i, mocs[i,3]))
        all_fixations_good <- TRUE
        for (stimNum in 1:length(stims)) {
            beep_function(stimNum)
            s <- stims[[stimNum]]
            if (stimNum == length(stims)) {
              ret <- opiPresent(stim=s, nextStim=nextStims[[stimNum]], ...)

              fixation_good <- TRUE
              if (!is.null(s$checkFixationOK))
                fixation_good <- s$checkFixationOK(ret)
              all_fixations_good <- all_fixations_good && fixation_good
            
              cat(sprintf(",%f,%f,%f,",s$x,s$y, fixation_good))
              cat(stim_print(s,ret))
            } else {
              startTime <- Sys.time()

              ret <- opiPresent(stim=s, nextStim=NULL, ...)

              fixation_good <- TRUE
              if (!is.null(s$checkFixationOK))
                fixation_good <- s$checkFixationOK(ret)
              all_fixations_good <- all_fixations_good && fixation_good
            
              cat(sprintf(",%f,%f,%f,",s$x,s$y, fixation_good))
              cat(stim_print(s,ret))

                # just check that the reponse window wasn't scuppered by a response
              while (Sys.time() - startTime < s$responseWindow/1000)
                Sys.sleep(0.05)
            }
        }

        if (responseWindowMeth == "forceKey")
          ret <- keyHandler(mocs[i, 6], ret)
 
        if (is.null(ret$err)) {
            if (ret$seen) 
                beep_function('correct')
            else 
                beep_function('incorrect')
        
            if (ret$seen && responseWindowMeth == "speed") 
                respTimeHistory <- c(tail(respTimeHistory, -1), ret$time)
        } else {
            warning("Opi Present return error in MOCS")
            error_count <- error_count + 1 
        }

        cat(sprintf(',%g,%g\n',ret$seen,  ret$time))
        
        Sys.sleep(runif(1, min=interStimMin, max=interStimMax)/1000)

        results <- rbind(results, c(mocs[i,1:5], all_fixations_good, mocs[i,6:ncol(mocs)], ret))
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
###), ncol=5, byrow=TRUE)
###
###t1 <- matrix(c(
###    9,9, 3, 1, 3145  , 314,
###    6,6, 4, 2,  313  , 314,
###    3,3, 5, 2,   31.4, 314
###), ncol=6, byrow=TRUE)
###
###a1 <- list(sin(1:10000/20), sin(1:10000/20), sin(1:10000/10), sin(1:10000/30))
###
###BETWEEN_FLASH_TIME <- 500   # ms
###
###makeStim <- function(p, rwin) {
###    res <- NULL
###    for(i in 5:length(p)) {
###
###        s <- list(x=p[1], y=p[2], level=p[5], size=0.43, duration=200,
###                  responseWindow=ifelse(i < length(p), 0, BETWEEN_FLASH_TIME),
###                  checkFixationOK=NULL
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
