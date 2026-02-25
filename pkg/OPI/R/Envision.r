#
# OPI for Envision Head-Mounted Perimeter (HMP)
#
# This version creates a socket to an Envision portal that drives
# an Envision HMP
#
# Author: Ivan Marin-Franch
#
# Copyright [2026] [Andrew Turpin & Ivan Marin-Franch]
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

# environment for this machine in R
if (exists(".opi_env") && !exists("Envision", where = .opi_env))
    assign("Envision", new.env(), envir = .opi_env)

#' Implementation of opiInitialise for the Envision HMP.
#'
#' This is for internal use only. Use [opiInitialise()] after
#' \code{chooseOPI("Envision")} to call this function.
#'
#' @usage NULL
#'
#' @param address A list containing:
#'  * \code{port} TCP port of the Envision's OPI.
#'  * \code{ip} IP Address of the Envision's OPI.
#'
#' @return A list containing:
#'  * \code{err} \code{NULL} if there was no error, a string message if there is an error.
#'
#' @details
#' \code{port} can take on values in the range \code{[0, 65535]}.
#'
#' @examples
#' \dontrun{
#' chooseOpi("Envision")
#' result <- opiInitialise(address = list(port = 50001, ip = "localhost"))
#' }
#'
#' @seealso [opiInitialise()]
opiInitialise_for_Envision <- function(address = NULL) {
  if(is.null(address)) return(list(err = "Need parameter address."))
  if(is.null(address$ip)) return(list(err = "Address should have the field ip."))
  if(is.null(address$port)) return(list(err = "Address should have the field port."))
  if(exists("socket", where = .opi_env$Envision) & !is.null(.opi_env$Envision$socket))
    return(list(err = "Socket connection already exists. Perhaps not closed properly last time? Restart Envision's OPI and R."))
  socket <- suppressWarnings(tryCatch(socketConnection(host = address$ip, port = address$port,
                                                       blocking = TRUE, open = "w+b", timeout = 10),
                                      error = function(e) return(NULL)))
  if(is.null(socket))
    return(list(err = paste("Cannot find a server at", address$ip, "on port", address$port)))
  assign("socket", socket, .opi_env$Envision)
  if(is.null(.opi_env$Envision$socket))
    return(list(err = sprintf("Cannot Cannot find a server at %s on port %s", address$ip, address$port)))
  msg <- list(port = address$port, ip = address$ip)
  msg <- c(list(command = "initialize"), msg)
  msg <- msg[!unlist(lapply(msg, is.null))]
  msg <- jsonlite::toJSON(msg, auto_unbox = TRUE)
  writeLines(msg, .opi_env$Envision$socket)
  res <- readLines(.opi_env$Envision$socket, n = 1)
  if(length(res) == 0)
    return(list(err = "Envision's OPI is running but a connection was not closed properly using opiClose() last time it was used. Restart it."))
  res <- jsonlite::parse_json(res)
  if(!"error" %in% names(res))
    return(list(err = "Server did not return a list with element 'error' in opiInitialise"))
  if(!"msg" %in% names(res))
    return(list(err = "Server did not return a list with element 'msg' in opiInitialise"))
  if (res$error)
    return(list(err = res$msg))
  assign("machine_is_initialised", TRUE, .opi_env)
  return(list(err = NULL))
}

#' Implementation of opiQueryDevice for the Envision HMP.
#'
#' This is for internal use only. Use [opiQueryDevice()] after
#' \code{chooseOPI("Envision")} to call this function.
#'
#' @usage opiQueryDevice()
#'
#' @return A list containing:
#'  * \code{err} \code{NULL} if there was no error, a string message if there is an error.
#'  * \code{specs} A list with specifications (see Details) if there was no error.
#'  * \code{settings} A list with the settings (see Details) if there was no error.
#'
#' @details
#' \code{specs} is a list containing:
#'  * \code{hardware} String with the Hardware name.
#'  * \code{screens} Whether the hardware has 1 screen (for both eyes) or 2 (1 for each).
#'  * \code{fov} A list with the field of view for x-axis (\code{x}) and y-axis (\code{y}).
#'  * \code{resolution} A list with the resolution for width (\code{w}) and height (\code{h}).
#'  * \code{ipd} A list with min (\code{min}) and max (\code{max}) possible IPDs
#'               and the current value (\code{val}).
#'  * \code{depth} Display's bit depth.
#'  * \code{refresh} Display's refresh rate.
#'  * \code{domain} An array of feasible dB values by the hardware.
#'
#' \code{settings} is a list containing:
#'  * \code{bgeye} The eye where the background is shown ("OD", "OS", "OU").
#'  * \code{bglum} Background luminance in cd/m2.
#'  * \code{fixeye} The eye where a fixation target is shown ("OD", "OS", "OU").
#'  * \code{fixlum} Fixation luminance in cd/m2.
#'  * \code{fixtarget} Fixation target.
#'  * \code{fixcx} Fixation x-coordinate.
#'  * \code{fixcy} Fixation y-coordinate.
#'  * \code{fixdx} Fixation size in the x-axis (diameter for circles).
#'  * \code{fixdy} Fixation size in the y-axis (diameter for circles).
#'  * \code{fixtheta} Fixation rotation in degrees.
#'  * \code{tracking} Whether active correction is active.
#'
#' @examples
#' \dontrun{
#' chooseOpi("Envision")
#' opiInitialise(address = list(port = 50001, ip = "localhost"))
#' opiSetup(list(eye = "BOTH"))
#' result <- opiQueryDevice()
#' }
#'
#' @seealso [opiQueryDevice()]
opiQueryDevice_for_Envision <- function() {
  if(!exists(".opi_env") || !exists("Envision", envir = .opi_env) || !("socket" %in% names(.opi_env$Envision)) || is.null(.opi_env$Envision$socket))
    return(list(err = "Cannot call opiQueryDevice without an open socket to Envision's OPI. Did you call opiInitialise()?."))
  msg <- list()
  msg <- c(list(command = "query"), msg)
  msg <- msg[!unlist(lapply(msg, is.null))]
  msg <- jsonlite::toJSON(msg, auto_unbox = TRUE)
  writeLines(msg, .opi_env$Envision$socket)
  res <- readLines(.opi_env$Envision$socket, n = 1)
  if (length(res) == 0)
    return(list(err = "Envision's OPI is running but a connection was not closed properly using opiClose() last time it was used. Restart it."))
  res <- jsonlite::parse_json(res)
  if (!"error" %in% names(res))
    return(list(err = "Server did not return a list with element 'error' in opiQueryDevice"))
  if (!"msg" %in% names(res))
    return(list(err = "Server did not return a list with element 'msg' in opiQueryDevice"))
  if (res$error)
    return(list(err = res$msg))
  specs <- jsonlite::parse_json(res$specs)
  specs$domain <- unlist(res$domain)
  settings <- jsonlite::parse_json(res$settings)
  settings$bgeye <- toupper(settings$bgeye)
  settings$fixeye <- toupper(settings$fixeye)
  settings$fixtarget <- toupper(settings$fixtarget)
  return(list(err = NULL, specs = specs, settings = settings))
}

#' Implementation of opiSetup for the Envision HMP.
#'
#' This is for internal use only. Use [opiSetup()] after
#' \code{chooseOPI("Envision")} to call this function.
#'
#' @usage NULL
#'
#' @param settings A list containing:
#'  * \code{bgeye} The eye where the background is shown ("OD", "OS", "OU"). Default is "OU".
#'  * \code{bglum} Background luminance in cd/m2. Default is 10.
#'  * \code{fixeye} The eye where a fixation target is shown ("OD", "OS", "OU"). Default is "OU".
#'  * \code{fixlum} Fixation luminance in cd/m2. Default is 20.
#'  * \code{fixtarget} Fixation target. Default is "CROSS".
#'  * \code{fixcx} Fixation x-coordinate. Default is 0.
#'  * \code{fixcy} Fixation y-coordinate. Default is 0.
#'  * \code{fixdx} Fixation size in the x-axis (diameter for circles). Default is 2.
#'  * \code{fixdy} Fixation size in the y-axis (diameter for circles). Default is 2.
#'  * \code{fixtheta} Fixation rotation in degrees. Default is 0.
#'  * \code{tracking} Whether active correction is active. Default is False.
#'
#' @details
#' All fields are optional. Missing fields will be assigned their Default value.
#' \code{opiSetup()} sets all to default.
#'
#' @return A list containing:
#'  * \code{err} \code{NULL} if there was no error, a string message if there is an error.
#'
#' @examples
#' \dontrun{
#' chooseOpi("Envision")
#' opiInitialise(address = list(port = 50001, ip = "localhost"))
#' result <- opiSetup(settings = list(eye = "BOTH"))
#' }
#'
#' @seealso [opiSetup()]
opiSetup_for_Envision <- function(settings = NULL) {
  if(!exists(".opi_env") || !exists("Envision", envir = .opi_env) || !("socket" %in% names(.opi_env$Envision)) || is.null(.opi_env$Envision$socket))
    return(list(err = "Cannot call opiSetup without an open socket to Envision's OPI. Did you call opiInitialise()?."))
  defaults <- list(bgeye = "OU", bglum = 10, fixeye = "OU", fixlum = 20, fixtarget = "CROSS",
                   fixcx = 0, fixcy = 0, fixdx = 2, fixdy = 2, fixtheta = 0, tracking = TRUE)
  if(is.null(settings))
    settings <- defaults
  else for(c in names(defaults)) {
    if(!(c %in% names(settings)))
      settings[c] <- defaults[c]
  }
  settings$bgeye <- toupper(settings$bgeye)
  settings$fixeye <- toupper(settings$fixeye)
  settings$fixtarget <- toupper(settings$fixtarget)
  msg <- c(list(command = "setup"), settings)
  msg <- msg[!unlist(lapply(msg, is.null))]
  msg <- jsonlite::toJSON(msg, auto_unbox = TRUE)
  writeLines(msg, .opi_env$Envision$socket)
  res <- readLines(.opi_env$Envision$socket, n = 1)
  if (length(res) == 0)
    return(list(err = "Envision's OPI is running but a connection was not closed properly using opiClose() last time it was used. Restart it."))
  res <- jsonlite::parse_json(res)
  if (!"error" %in% names(res))
    return(list(err = "Server did not return a list with element 'error' in opiSetup"))
  if (!"msg" %in% names(res))
    return(list(err = "Server did not return a list with element 'msg' in opiSetup"))
  if (res$error)
    return(list(err = res$msg))
  return(list(err = NULL))
}

#' Implementation of opiPresent for the Envision HMP.
#'
#' This is for internal use only. Use [opiPresent()] after
#' \code{chooseOPI("Envision")} to call this function.
#'
#' @usage NULL
#'
#' @param stim A list containing:
#'  * \code{eye} Eye where to present the stimulus
#'  * \code{x} x-coordinate
#'  * \code{y} y-coordinate
#'  * \code{sx} Size in the x-axis (diameter for circles).
#'  * \code{sy} (Optional) Size in the y-axis (diameter for circles). Defaults to sx
#'  * \code{level} Stimulus level
#'  * \code{theta} (Optional) Stimulus rotation from 0 to 360 degrees. Defaults to 0
#'  * \code{t} (Optional) Stimulus presentation time. Defaults to 200 ms
#'  * \code{window} (Optional) Response window. Defaults to 1500 ms
#' @param ... Parameters for other opiPresent implementations that are ignored here.
#'
#' @return A list containing:
#'  * \code{err} \code{NULL} if there was no error, a string message if there is an error.
#'               If there was no error the the following fields are also returned.
#'  * \code{stamp} Time stamp of stimulus onset.
#'  * \code{time} Response time from stimulus onset if button pressed (ms).
#'  * \code{seen} \code{TRUE} if seen, \code{FALSE} if not.
#'  * \code{lpupil}, \code{rpupil} Fixation positions (\code{x}, \code{y}), and diameter (\code{d})
#'                   for both left (\code{lpupil}) and right pupils (\code{rpupil}).
#'
#' @examples
#' \dontrun{
#' chooseOpi("Envision")
#' opiInitialise(address = list(port = 50001, ip = "localhost"))
#' result <- opiPresent(stim = list(eye = "os", x = 5, y = 4, sx = 0.43, level = 0))
#' }
#'
#' @seealso [opiPresent()]
opiPresent_for_Envision <- function(stim, ...) {
  if(!exists(".opi_env") || !exists("Envision", envir = .opi_env) || !("socket" %in% names(.opi_env$Envision)) || is.null(.opi_env$Envision$socket))
    return(list(err = "Cannot call opiPresent without an open socket to Envision's OPI. Did you call opiInitialise()?."))
  if (is.null(stim)) return(list(err = "Nothing to opiPresent."))
  if(!all(c("eye", "x", "y", "sx", "level") %in% names(stim)))
    return(list(err = "Mandatory fields missing: eye, x, y, sx, level."))
  if(!("sy" %in% names(stim)))
    stim$sy <- stim$sx
  defaults <- list(theta = 0, t = 200, window = 1500)
  for(c in names(defaults)) {
    if(!(c %in% names(stim)))
      stim[c] <- defaults[c]
  }
  msg <- list(eye = stim$eye, cx = stim$x, cy = stim$y, dx = stim$sx, dy = stim$sy, level = stim$level, theta = stim$theta, t = stim$t, window = stim$window)
  msg <- c(list(command = "present"), msg)
  msg <- msg[!unlist(lapply(msg, is.null))]
  msg <- jsonlite::toJSON(msg, auto_unbox = TRUE)
  writeLines(msg, .opi_env$Envision$socket)
  res <- readLines(.opi_env$Envision$socket, n = 1)
  if (length(res) == 0)
    return(list(err = "Envision's OPI is running but a connection was not closed properly using opiClose() last time it was used. Restart it."))
  res <- jsonlite::parse_json(res)
  if (!"error" %in% names(res))
    return(list(err = "Server did not return a list with element 'error' in opiPresent"))
  if (!"msg" %in% names(res))
    return(list(err = "Server did not return a list with element 'msg' in opiPresent"))
  if (res$error)
    return(list(err = res$msg))
  res <- c(list(err = NULL), res$resp)
  res$lpupil <- jsonlite::parse_json(res$lpupil)
  res$rpupil <- jsonlite::parse_json(res$rpupil)
  return(res)
}

#' Implementation of opiClose for the Envision HMP.
#'
#' This is for internal use only. Use [opiClose()] after
#' \code{chooseOPI("Envision")} to call this function.
#'
#' @usage NULL
#'
#' @return A list containing:
#'  * \code{err} \code{NULL} if there was no error, a string message if there is an error.
#'
#' @examples
#' \dontrun{
#' chooseOpi("Envision")
#' opiInitialise(address = list(port = 50001, ip = "localhost"))
#' opiSetup(list(eye = "BOTH"))
#' result <- opiClose()
#' }
#'
#' @seealso [opiClose()]
opiClose_for_Envision <- function() {
  if(!exists(".opi_env") || !exists("Envision", envir = .opi_env) || !("socket" %in% names(.opi_env$Envision)) || is.null(.opi_env$Envision$socket))
    return(list(err = "Cannot call opiClose without an open socket to Envision's OPI. Did you call opiInitialise()?."))
  msg <- list()
  msg <- c(list(command = "close"), msg)
  msg <- msg[!unlist(lapply(msg, is.null))]
  msg <- jsonlite::toJSON(msg, auto_unbox = TRUE)
  writeLines(msg, .opi_env$Envision$socket)
  res <- readLines(.opi_env$Envision$socket, n = 1)
  close(.opi_env$Envision$socket)
  assign("socket", NULL, .opi_env$Envision)
  if (length(res) == 0)
    return(list(err = "Envision's OPI is running but a connection was not closed properly using opiClose() last time it was used. Restart it."))
  res <- jsonlite::parse_json(res)
  if (!"error" %in% names(res))
    return(list(err = "Server did not return a list with element 'error' in opiClose"))
  if (!"msg" %in% names(res))
    return(list(err = "Server did not return a list with element 'msg' in opiClose"))
  if (res$error)
    return(list(err = res$msg))
  return(list(err = NULL))
}

