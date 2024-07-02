#
# An implementation of the OPI that simulates a patient that never responds.
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

#' @title opiClose_for_SimNo
#' @description
#' Does nothing.
#' @usage NULL
#' @return A list with elements:
#'   * \code{error} Always \code{FALSE}.
#'   * \code{msg} A string "Close OK".
#'
opiClose_for_SimNo <- function() list(err = NULL)

#' @title opiQueryDevice_for_SimNo
#' @description
#' Returns name of the machine.
#' @usage NULL
#'
#' @return A list with elements:
#'   * \code{machine} that is set to `"SimNo"`.
#'   * \code{isSim} that is set to TRUE.
#'
opiQueryDevice_for_SimNo <- function() list(isSim = TRUE, machine = "SimNo")

#' @title opiInitialise_for_SimNo
#' @description
#' Does nothing.
#'
#' @usage NULL
#' @param ... Any object you like, it is ignored.
#'
#' @return A list with elements:
#'   * \code{err} Always \code{NULL}.
#'
opiInitialise_for_SimNo <- function(...) list(err = NULL)

#' @title opiSetup_for_SimNo
#' @description
#' Does nothing.
#'
#' @usage NULL
#' @param settings Anything you like, it is ignored.
#'
#' @return A list with elements:
#'   * \code{err} Always \code{NULL}.
#'
opiSetup_for_SimNo <- function(settings) list(err = NULL)

#' @title opiPresent_for_SimNo
#' @description
#' Always respond 'not seen' to any parameter.
#' No checking is done on the validity of `stim`.
#'
#' @usage NULL
#' @param stim Anything you like, it is ignored.
#' @param ... Any parameters you like, they are ignored.
#'
#' @return A list with elements:
#'   * \code{err} Always \code{NULL}.
#    * \code{seen} Always \code{FALSE}.
#'   * \code{time} Always \code{NA}.
#'
opiPresent_for_SimNo <- function(stim, ...) list(err = NULL, seen = FALSE, time = NA)