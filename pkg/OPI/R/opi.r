# Open Perimetry Interface controlling class.
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
#

#.onAttach <- function(libname, pkgname) {
#    packageStartupMessage("OPI version", utils::packageVersion("OPI"))
#}

# Used for holding state of the OPI

#' @name .opi_env
#' @title Global environment for OPI to hold machine specific constants, etc.
#' @export
.opi_env <- new.env(size = 20)

assign("machine_list", list(
    # "Jovp",  Not needed as a standalone, use subclasses: Display, ...
    #"Icare",  Not needed as a standalone, use subclasses: O900, O600
    "MAIA",
    "Compass",
    "ImoVifa",
    "Tempo",
    "Kowa",
    "Octopus900",
    "O600",  # Never really supported?
    "SimNo",
    "SimYes",
    "SimHenson",
    "SimHensonRT",
    "SimGaussian",
    "PhoneHMD",
    "Display",
    "PicoVR"
), envir = .opi_env)

assign("chosen_machine", NULL, .opi_env) # Chosen machine from machine_list by chooseOPI()

#' chooseOPI selects an OPI machine to use.
#'
#' It should be called before any other OPI functions.
#'
#' @param machine Machine name to use. Set to NULL to get a list.
#' @returns NULL on success or list of machines otherwise.
#' @export
chooseOPI <- function(machine = NULL) {
    if (is.null(machine)) {
        cat(sprintf("%s is not a valid OPI machine.\nYou should choose from:\n", machine))
        print(unlist(.opi_env$machine_list))
        return(unlist(.opi_env$machine_list))
    }

    if (! machine %in% .opi_env$machine_list) {
        cat(sprintf("%s is not a valid OPI machine.\nYou should choose from:\n", machine))
        print(unlist(.opi_env$machine_list))
        return(unlist(.opi_env$machine_list))
    }

    if (machine == "Tempo") machine <- "ImoVifa"

    assign("chosen_machine", machine, .opi_env)
    assign("machine_is_initialised", FALSE, .opi_env)
    return(NULL)
}

#' @rdname chooseOPI
#' @export
chooseOpi <- chooseOPI

#' @title Calls opiInitialise_for_MACHINE as appropriate.
#' @description
#'
#' Establishes connection with the device and a Monitor (aka Server) if appropriate.
#' Sends any startup parameters that might be needed by the machine.
#' Specific parameters and return values can be seen in the machine specific versions
#' listed below in the ’See Also’.
#'
#' @param ... Parameters specific to each machine as described in the 'See Also' functions.
#'
#' @return A list containing at least the following elements:
#'   * \code{err} \code{NULL} if no error, otherwise a string describing the error.
#'
#' @seealso
#' [opiInitialise_for_ImoVifa()],
#' [opiInitialise_for_PhoneHMD()], [opiInitialise_for_Display()], [opiInitialise_for_PicoVR()],
#' [opiInitialise_for_Octopus900()],
#' [opiInitialise_for_Compass()],
#' [opiInitialise_for_SimNo()], [opiInitialise_for_SimYes()], [opiInitialise_for_SimHenson()],
#' [opiInitialise_for_SimHensonRT()],
#' [opiInitialise_for_SimGaussian()]
#  [opiInitialise_for_Kowa()],
#' @export
opiInitialise <- function(...) {
    if (is.null(.opi_env$chosen_machine))
        stop("you should use chooseOPI() before calling opiInitialise.")

    return(do.call(paste0("opiInitialise_for_", .opi_env$chosen_machine), args = list(...)))
}

#' @rdname opiInitialise
#' @export
opiInitialize <- opiInitialise

#' @title Calls opiQueryDevice_for_MACHINE as appropriate.
#' @description
#'
#' Returns a list that describes the current state of the machine.
#' Specific parameters and return values can be seen in the machine specific versions
#' listed below in the ’See Also’.
#'
#' @return A list specific to each machine.
#'
#' @seealso [opiQueryDevice_for_ImoVifa()],
#' [opiQueryDevice_for_Compass()],
#' [opiQueryDevice_for_Octopus900()],
# [opiQueryDevice_for_Kowa()], [opiQueryDevice_for_O600()],
#' [opiQueryDevice_for_PhoneHMD()], [opiQueryDevice_for_Display()], [opiQueryDevice_for_PicoVR()],
#' [opiQueryDevice_for_SimNo()], [opiQueryDevice_for_SimYes()], [opiQueryDevice_for_SimHenson()],
#' [opiQueryDevice_for_SimHensonRT()],
#' [opiQueryDevice_for_SimGaussian()]
#' @export
opiQueryDevice <- function() {
    if (is.null(.opi_env$chosen_machine))
        stop("you should use chooseOPI() before calling opiQueryDevice.")

    return(do.call(paste0("opiQueryDevice_for_", .opi_env$chosen_machine), args = list()))
}

#' @title Calls opiSetup_for_MACHINE as appropriate.
#' @description
#'
#' Specific parameters and return values can be seen in the machine specific versions
#' listed below in the ’See Also’.
#'
#' @param settings A list containing specific settings for a machine.
#'
#' @return Each implementation should(!) return a list with at least the following elements:
#'   * \code{err} \code{NULL} if no error, otherwise a string describing the error.
#'
#' @seealso 
#' [opiSetup_for_Compass()],
#' [opiSetup_for_Octopus900()],
# [opiSetup_for_Kowa()], [opiSetup_for_O600()],
#' [opiSetup_for_ImoVifa()],
#' [opiSetup_for_PhoneHMD()], [opiSetup_for_Display()], [opiSetup_for_PicoVR()],
#' [opiSetup_for_SimNo()], [opiSetup_for_SimYes()], [opiSetup_for_SimHenson()],
#' [opiSetup_for_SimHensonRT()],
#' [opiSetup_for_SimGaussian()]
#' @export
opiSetup <- function(settings) {
    if (is.null(.opi_env$chosen_machine))
        stop("you should use chooseOPI() before calling opiSetup().")
    if (!.opi_env$machine_is_initialised)
        stop("you should call opiInitialise() before calling opiSetup().")

    return(do.call(paste0("opiSetup_for_", .opi_env$chosen_machine), list(settings)))
}

#' @title Calls opiClose_for_MACHINE as appropriate.
#' @description
#'
#' Specific parameters and return values can be seen in the machine specific versions
#' listed below in the ’See Also’.
#'
#' @return Each implementation should(!) return a list with at least the following elements:
#'   * \code{err} \code{NULL} if no error, otherwise a string describing the error.
#'
#' @seealso 
#' [opiClose_for_Compass()],
#' [opiClose_for_Octopus900()],
#' [opiClose_for_ImoVifa()],
#' [opiClose_for_PhoneHMD()], [opiClose_for_Display()], [opiClose_for_PicoVR()],
#' [opiClose_for_SimNo()], [opiClose_for_SimYes()], [opiClose_for_SimHenson()],
#' [opiClose_for_SimHensonRT()],
#' [opiClose_for_SimGaussian()]
# [opiClose_for_Kowa()], [opiClose_for_O600()],
#' @export
opiClose <- function() {
    if (is.null(.opi_env$chosen_machine))
        stop("you should use chooseOPI() before calling opiClose().")
    if (!.opi_env$machine_is_initialised)
        stop("you should call opiInitialise() before calling opiClose().")

    assign("machine_is_initialised", FALSE, .opi_env)
    return(do.call(paste0("opiClose_for_", .opi_env$chosen_machine), args = list()))
}

#' @title Calls opiPresent_for_MACHINE as appropriate.
#' @description
#'
#' Specific parameters and return values can be seen in the machine specific versions
#' listed below in the ’See Also’.
#'
#' @param stim A stimulus object or list as described for each machine in the 'See Also' methods.
#' @param ...  Other arguments that might be needed by each machine in the 'See Also' methods.
#'
#' @return Each implementation should(!) return a list with at least the following elements:
#'   * \code{err} \code{NULL} if no error, otherwise a string describing the error.
#'   * \code{seen} \code{TRUE} if stimulus seen, \code{FALSE} otherwise
#'   * \code{time} Response time from onset of stimulus in milliseconds.
#'
#' @seealso 
#' [opiPresent_for_Compass()],
#' [opiPresent_for_Octopus900()],
#' [opiPresent_for_ImoVifa()],
#' [opiPresent_for_PhoneHMD()], [opiPresent_for_Display()], [opiPresent_for_PicoVR()],
#' [opiPresent_for_SimNo()], [opiPresent_for_SimYes()], [opiPresent_for_SimHenson()],
#' [opiPresent_for_SimHensonRT()],
#' [opiPresent_for_SimGaussian()]
# [opiPresent_for_Kowa()], [opiPresent_for_O600()],
#' @export
opiPresent <- function(stim, ...) {
    if (is.null(.opi_env$chosen_machine))
        stop("you should use chooseOPI() before calling opiPresent.")
    if (!.opi_env$machine_is_initialised)
        stop("you should call opiInitialise() before calling opiPresent().")

        # for backwards compatability for version < OPI 3.0
    if ("level" %in% names(stim) && !("lum" %in% names(stim))) {
        cc <- class(stim)
        stim <- c(stim, lum = stim$level)
        class(stim) <- cc   # preserve and opiStatic/Temporal/KineticStimulus classes
    }

    return(do.call(paste0("opiPresent_for_", .opi_env$chosen_machine), list(stim = stim, ...)))
}

#' @title Deprecated. Use [opiSetup()].
#' @description
#' In older OPIs it set background color and luminance in both eyes.
#' Deprecated for OPI >= v3.0.0 and replaced with [opiSetup()].
#' @usage NULL
#' @seealso [opiSetup()]
#' @export
opiSetBackground <- function(...) {
    warning("opiSetBackground is deprecated. Use opiSetup()")
    opiSetup(list(...))
}

#'
#' @title Open a socket on ip and port.
#' @description Internal use only.
#'
#' @param ip IP address of socket
#' @param port TCP port of socket
#' @param machineName Machine name for error message
#'
#' @return Socket or NULL on error
#'
open_socket <- function(ip, port, machineName) {
    cat("Looking for a server at ", ip, port, "...\n")

    suppressWarnings(socket <- tryCatch(
        socketConnection(host = ip, port,
                    blocking = TRUE, open = "w+b",
                    timeout = 10)
        , error = function(e) {
            warning(paste("Cannot find a server at", ip, "on port", port))
            return(NULL)
        }
    ))

    cat("Found server at", ip, port, "\n")

    return(socket)
}

#' For backwards compatibility. Used by Octopus900 and KowaAP7000.
opiStaticStimulus <- function() NULL
#' For backwards compatibility. Used by Octopus900 and KowaAP7000.
opiKineticStimulus <- function() NULL
#' For backwards compatibility. Used by Octopus900 and KowaAP7000.
opiTemporalStimulus <- function() NULL
