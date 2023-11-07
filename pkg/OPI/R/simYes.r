#
# An implementation of the OPI that simulates a patient that always responds.
#
# Author: Andrew Turpin    (andrew.turpin@lei.org.au)
# Date: October 2012
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

simYes.opiClose         <- function() { return(NULL) }
simYes.opiQueryDevice   <- function() { return (list(type="SimNo", isSim=TRUE)) }

################################################################################
# Input
#   display Dimensions of plot area to display stim. c(-x,+x,-y,+y) No display if NULL
#
# Return NULL if succesful, string error message otherwise  
################################################################################
simYes.opiInitialize <- function(display = NA) {
    if(simDisplay.setupDisplay(display))
        warning("opiInitialize (SimNo): display parameter may not contain 4 numbers.")

    return(NULL)
}

################################################################################
# Set background of plot area to col
# Return:
#   NULL - succsess
#   -1   - opiInitialize not called
################################################################################
#' @rdname opiSetBackground
#' @details
#' # SimYes
#'   DETAILS
#'
simYes.opiSetBackground <- function(col, gridCol) { 
    return (simDisplay.setBackground(col, gridCol))
}

#' @rdname opiPresent
#' @details
#' # SimYes
#'   \code{opiPresent(stim, nextStim=NULL)}
#'   
#'   If the chosen OPI implementation is \code{SimYes}, then the response to a
#'   stimuli is always yes, hence \code{\link{opiPresent}} always returns
#'   \code{err=NULL}, \code{seen=TRUE}, and \code{time=0}.
#'
simYes.opiPresent <- function(stim, nextStim=NULL) {
    simDisplay.present(stim$x, stim$y, stim$color, stim$duration, stim$responseWindow)

    return ( list(
        err = NULL,
        seen= TRUE,
        time= 0
    ))
}
