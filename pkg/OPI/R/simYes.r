#
# An implementation of the OPI that simulates a patient that always responds.
#
# Author: Andrew Turpin
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

#' @title opiClose_for_SimYes
#' @description Does nothing.
#' @usage NULL
#' @return A list with elements:
#'   * \code{err} Always \code{NULL}.
#'
opiClose_for_SimYes <- function() list(err = NULL)

#' @title opiQueryDevice_for_SimYes
#' @description
#' Returns name of the machine.
#'
#' @usage NULL
#' @return A list with elements:
#'   * \code{isSim} Always \code{TRUE}.
#'   * \code{machine} that is set to `"SimYes"`.
#'
opiQueryDevice_for_SimYes <- function() list(isSim = TRUE, machine = "SimYes")

#' @title opiInitialise_for_SimYes
#' @description
#' Does nothing.
#'
#' @usage NULL
#' @param ... Any object you like, it is ignored.
#'
#' @return A list with elements:
#'   * \code{err} Always \code{NULL}.
#'
opiInitialise_for_SimYes <- function(...) {
    assign("machine_is_initialised", TRUE, .opi_env)
    list(err = NULL)
}

#' @title opiSetup_for_SimYes
#' @description
#' Does nothing.
#'
#' @usage NULL
#' @param settings Any object you like, it is ignored.
#'
#' @return A list with elements:
#'   * \code{err} Always \code{NULL}.
#'
opiSetup_for_SimYes <- function(settings) list(err = NULL)

#' @title opiPresent_for_SimYes
#' @description
#' Always respond 'yes' immediately to any parameter.
#'
#' @usage NULL
#' @param stim Anything you like, it is ignored.
#' @param ... Any parameters you like, they are ignored.
#'
#' @return A list with elements:
#'   * \code{err} Always \code{FALSE}.
#'   * \code{seen} Always \code{TRUE}.
#'   * \code{time} Always \code{NA}.
#'
opiPresent_for_SimYes <- function(stim, ...) list(err = NULL, seen = TRUE, time = NA)
