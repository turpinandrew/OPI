#
# OPI for O600
#
# Authors:
#   Andrew Turpin    (andrew.turpin@lei.org.au)
#   David Lawson     (david.lawson@unimelb.edu.au)
# Date: July 2014
#
# Copyright [2016] [Andrew Turpin and David Lawson]
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

if (exists(".opi_env") && !exists("O600", where=.opi_env))
    assign("O600", new.env(6), envir=.opi_env)

#######################################################################
# Networking helper functions
#######################################################################

send <- function(packet, socket) {
  writeBin(packet, socket, size = 4, endian = "big")
}

pad <- function(data) {
  padsize = 128 - length(data)
  data = append(data, rep(0, padsize))
  return(data)
}

sendCommand <- function(socket, id, ...) {
  packet <- c(id, 0, 0, 10, 0, 0, 0)
  packet <- append(packet, c(...))
  packet <- pad(packet)
  packet <- as.integer(packet)

  #print("Sending:")
  #print(packet)

  send(packet, socket)

  response <- readBin(socket, integer(), n = 128, size = 4, signed = TRUE, endian = "big")

  #print("R<----ceived:")
  #print(re<----ponse)

  if (length(response) == 0)
    stop("O600 not responding")

  if (response[3] != 0) {
    warning(paste("O600 returned error ID", response[3]))
  }

  return(list(response, response[3]))
}

#' @title Implementation of opiInitialise for the O600 machine.
#' @description
#' This is for internal use only. Use [opiInitialise()] with
#' these Arguments and you will get the Value back.
#'
#' @usage NULL
#'
#' @param ipAddress The IP address of the O600 as a string.
#' @param eye Either "left" or "right".
#' @param pupilTracking TRUE to turn on IR illumination and set pupil
#'   black level (which happens at the first stimulus presentation).
#' @param pulsar TRUE for pulsar stimulus, FALSE for size III white-on-white.
#' @param eyeControl One of
#' 
#'     * 0 is off
#'     * 1 is eye blink
#'     * 2 is eye blink, forehead rest, fixation control
#'     * 3 is eye blink, forehead rest, fixation control, fast eye movements
#'
#' @details
#'
#' The default
#' background and stimulus setup is to white-on-white perimetry.
#' 
#' Uses port 50000 on the O600.
#'
#' @examples
#' \dontrun{
#'   # Set up the O600
#'   chooseOpi("O600")
#'   opiInitialize(ip="192.168.1.7", eye = "left")
#' }
opiInitialise_for_O600 <- function(ipAddress = "", eye = "",
                                  pupilTracking = FALSE,
                                  pulsar = FALSE, eyeControl = 0) {
  if (nchar(ipAddress) == 0)
    stop("You must specify an IP address in opiInitialize()")

  if (eye != "left" && eye != "right")
    stop("You must set eye=left or eye=right in opiInitialize()")

  if (!is.element(eyeControl, 0:3))
    stop("eyeControl must be equal to 0, 1, 2 or 3")

  socket = tryCatch(
    socketConnection(host = ipAddress, 50000, open = "w+b", blocking = TRUE, timeout = 5),
    error = function(e) stop(paste("Cannot connect to O600 on", ipAddress))
  )

  assign("socket", socket, envir = .opi_env$O600)

  print("Connected to O600")

  # set_eyecontrol()
  res = sendCommand(.opi_env$O600$socket, 2005, eyeControl, 60, 47, 136, 75)
  if (res[[2]] != 0)
    return(res[[2]])

  # initialise_perimeter()
  res = sendCommand(.opi_env$O600$socket, 2001, ifelse(pulsar, 3183, 1000))
  if (res[[2]] != 0)
    return(res[[2]])

  print(paste("initialise_perimeter returned freqLeft =", res[[1]][9], "and freqRight =", res[[1]][10]))

  if (pupilTracking) {
    # set_ir_illumination()
    res = sendCommand(.opi_env$O600$socket, 2007, eye=="left", eye=="right")
    if (res[[2]] != 0)
      return(res[[2]])
  }

  # set_fixationmark()
  if (pulsar) {
    res = sendCommand(.opi_env$O600$socket, 2003, eye=="left", 1, 2, 255) # yellow dot
    if (res[[2]] != 0)
      return(res[[2]])
  } else {
    res = sendCommand(.opi_env$O600$socket, 2003, eye=="left", 2, 2, 255) # yellow cross
    if (res[[2]] != 0)
      return(res[[2]])
  }

  assign("pupilTrackingEnabled", pupilTracking, envir = .opi_env$O600)
  assign("pupilBlackLevelSet", !pupilTracking, envir = .opi_env$O600)
  assign("eye", eye, envir = .opi_env$O600)
  assign("pulsar", pulsar, envir = .opi_env$O600)

	return(list(err = NULL))
}

#' @title Implementation of opiPresent for the O600 machine.
#' @description
#'
#' This is for internal use only. Use [opiPresent()] with the same arguments.
#'
#' @usage NULL
#'
#' @param stim Stimulus to present which is a list with the following elements:
#'
#'  * \code{x}  positionX (in 1/10deg)
#'  * \code{y}  positionY (in 1/10deg)
#'  * \code{level} dLog (intensity; 0dB is 4000 apostilbs) (in 1/10 dB)
#'  * \code{duration} Stimulus presentation duration in ms, for W/W 100ms, for pulsar 500ms
#'  * \code{responseWindow} Maximal allowed reaction time in ms, >=500ms and <4s
#'  * \code{sound} # Bit 1 = sound for patient response button ON; Bit2=1 sound for fixation lost ON
#' 
#' @param nextStim The stimulus to present after stim
#'        (it is not presented, but projector can move to it during response window)
#'
#' @return A list containing
#'
#'   * \code{err}  String message or NULL for no error.
#'   * \code{seen} 1 if seen, 0 otherwise.
#'   * \code{time} Reaction time (if seen).
#' 
#' @examples
#' \dontrun{
#'   chooseOpi("O600")
#'   if (!is.null(opiInitialize()$err))
#'       stop("opiInitialize failed")
#'   s <- list(x=9, y=9, level=dbTocd(db), size=0.43, color="white",
#'             duration=200, responseWindow=1500, checkFixationOK=NULL)
#'   print(opiPresent(s, NULL))
#' }
#'
#'
opiPresent_for_O600 <- function(stim, nextStim=NULL) { 

  leftEye = .opi_env$O600$eye == "left"

  if (!.opi_env$O600$pupilBlackLevelSet) {
    # adjustPupilBlackLevel()
    res = sendCommand(.opi_env$O600$socket, 2029, leftEye)
    if (res[[2]] != 0)
      return(list(err = res[[2]], seen=NA, time=NA))
    else
      assign("pupilBlackLevelSet", TRUE, envir = .opi_env$O600)
  }

  # display_stimulus()
  res <- sendCommand(
    .opi_env$O600$socket, 2000,
    0, #checkBGIllumi [Do always set to 0]
    0, #BGIntensity [If checkBGIllumi is set to 0, don't care]
    stim$x*10, #positionX [in 1/10deg]
    stim$y*10, #positionY [in 1/10deg]
    .opi_env$O600$pulsar*5, #method [0 = White-On-White, 5 = pulsar]
    0, #color [don't care]
    3, #stimulusSize [don't care] (has to be 3 for W-on-W, don't care for pulsar)
    cdTodb(stim$level, 4000/pi)*10, #dLog (intensity) [in 1/10 dB]
    stim$duration, #duration [stimulus presentation duration in ms, for W/W 100ms, for pulsar 500ms]
    leftEye, #selectedEye [0 = OD, 1 = OS]
    stim$responseWindow, #maxAllowedReactionTime (maximal allowed reaction time in ms, >=500ms and <4s)
    0, #type [0 = present normal stimulus, 1 = present positive catch trial, 2 = present negative catch trial]
    #sound [0 = no sound; Bit0=1 sound for stimulus presentation ON;
    #Bit1=1 sound for patient response button ON; Bit2=1 sound for fixation lost ON]
    ifelse(is.element("sound", names(stim)), stim$sound, 0)
  )

  if (res[[2]] != 0)
    return(list(err = res[[2]], seen=NA, time=NA))

  pupilSize = res[[1]][21]
  reactionTimePAK = res[[1]][22]
  answer = res[[1]][23]

  return(list(
    err = 0,
    seen = answer == 1,
    time = reactionTimePAK,
    answer = answer
  ))

}

#' @title opiSetup for the O600 machine.
#' @description
#' Implementation of opiSetup for the O600 machine.
#'
#' This is for internal use only. Use [opiSetup()] with these Arguments.
#'
#' @usage NULL
#'
#' @param bgColor Background color 0 to 255.
#' @param fixType fixation type 1, 2, 3 or 4.
#' @param fixColor fixation color 0, 2, 4 or 5.
#' @param fixIntensity fixation point intensity 0 to 255.
#'
#' @return A list containing one of the following
#'
#'   * \code{err = -1} to be implemented
#'   * \code{err = -2} O600 sent back an error; bad background parameters
#'   * \code{err = -3} O600 sent back an error; bad fixation parameters
#'   * \code{err = NULL} Success
#'
opiSetup_for_O600 <- function(bgColor=NA, fixType=NA, fixColor=NA, fixIntensity=255) {

  if (!is.element(fixType, 1:4))
    stop("Fixation type must be 1, 2, 3 or 4 in opiSetBackground()")

  if (!is.element(fixColor, c(0, 2, 4, 5)))
    stop("Fixation color must be 0, 2, 4 or 5 in opiSetBackground()")

  if (!is.na(bgColor) && !is.element(bgColor, 0:255))
    stop("Background color must be NA or an integer between 0 and 255")

  #todo return -1 if opiInitialize has not been successfully called

  # setBackground()
  res = sendCommand(.opi_env$O600$socket, 2021, ifelse(is.na(bgColor), 0, bgColor), ifelse(is.na(bgColor), .opi_env$O600$pulsar*5, -1))
  if (res[[2]] != 0)
    return(list(err = -2))

  # set_fixationmark()
  res = sendCommand(.opi_env$O600$socket, 2003, .opi_env$O600$eye == "left", fixType, fixColor, fixIntensity)
  if (res[[2]] != 0)
    return(list(err = -3))

  return(list(err = NULL))
}

#' @title Implementation of opiClose for the O600 machine.
#' @description
#'
#' This is for internal use only. Use [opiClose()] with the same parameters.
#'
#' @usage NULL
#'
#' @return Returns \code{list(err = NULL)}.
opiClose_for_O600 <- function() {
    close(.opi_env$O600$socket)
    return(list(err = NULL))
}

#' @title Implementation of opiQueryDevice for the O600 machine.
#' @description
#'
#' This is for internal use only. Use [opiQueryDevice()] with
#' these Arguments and you will get the Value back.
#'
#' @usage NULL
#'
#' @details Prints defined constants in OPI package pertaining to O600. 
#'
#'@return Returns a list of 10 items:
#' 
#'     * \code{answerButton} 0 = not pressed, 1 = pressed
#'     * \code{headSensor} 0 = no forehead detected, 1 = forehead detected
#'     * \code{eyeLidClosureLeft} 0 = eye is open, 1 = eye is closed
#'     * \code{eyeLidClosureRight} 0 = eye is open, 1 = eye is closed
#'     * \code{fixationLostLeft} 1 = eye pos lost, 0 = eye pos ok
#'     * \code{fixationLostRight} 1 = eye pos lost, 0 = eye pos ok
#'     * \code{pupilPositionXLeft} (in px)
#'     * \code{pupilPositionYLeft} (in px)
#'     * \code{pupilPositionXRight} (in px)
#'     * \code{pupilPositionYRight} (in px)
#'
opiQueryDevice_for_O600 <- function() {
  res <- sendCommand(.opi_env$O600$socket, 3004)

  ret <- list(
    answerButton        = res[[1]][8],
    headSensor          = res[[1]][9],
    eyeLidClosureLeft   = res[[1]][10],
    eyeLidClosureRight  = res[[1]][11],
    fixationLostLeft    = res[[1]][12],
    fixationLostRight   = res[[1]][13],
    pupilPositionXLeft  = res[[1]][14],
    pupilPositionYLeft  = res[[1]][15],
    pupilPositionXRight = res[[1]][16],
    pupilPositionYRight = res[[1]][17],
    isSim=FALSE
  )
  return(ret)
}