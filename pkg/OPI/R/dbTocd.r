# Open Perimetry Interface convert between perimetric dB to cd/m^2
#
# Copyright [2022] [Andrew Turpin, Ivan Marin-Franch, Jonathan Denniss]
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
# Author: Andrew Turpin
# Date: March 2023
#
# Modified Tue  8 Jul 2014: make generic for any dB scale
# Modified Tue 21 Mar 2023: changed licence from gnu to Apache 2.0 
#

#' Convert perimetric dB to cd/\eqn{\mbox{m}^2}{m^2}
#'
#' Given a value in dB, return the cd/\eqn{\mbox{m}^2}{m^2}
#' equivalent. Default is to use HFA units, so maximum stimulus is 10000
#' apostilbs.
#'
#' @param db Value to convert to cd/\eqn{\mbox{m}^2}{m^2}.
#' @param maxStim Stimulus value for 0dB in cd/\eqn{\mbox{m}^2}{m^2}.
#' @return cd/\eqn{\mbox{m}^2}{m^2} value for `db` dB.
#' @examples
#' # decibels to candela
#' cd <- dbTocd(0)   # 10000/pi
#' cd <- dbTocd(10)  # 1000/pi
#' cd <- dbTocd(20)  # 100/pi
#' cd <- dbTocd(30)  # 10/pi
#' cd <- dbTocd(40)  # 1/pi
#' @export
dbTocd <- function(db, maxStim = 10000 / pi) maxStim * 10^(-db / 10)

#' Convert cd/\eqn{\mbox{m}^2}{m^2} to perimetric dB.
#'
#' Given a value in cd/\eqn{\mbox{m}^2}{m^2}, return the equivalent dB value.
#' Default is to use HFA units, so maximum stimulus is 10000 apostilbs.
#'
#' @param cd Value to convert to dB in cd/\eqn{\mbox{m}^2}{m^2}. Must be > 0.
#' @param maxStim Stimulus value for 0dB in cd/\eqn{\mbox{m}^2}{m^2}.
#'
#' @return A dB value for `cd` cd/\eqn{\mbox{m}^2}{m^2}.
#' @examples
#' # candela to decibels
#' dB <- cdTodb(10000/pi)  # 0 dB
#' dB <- cdTodb(1000/pi)   # 10 dB
#' dB <- cdTodb(100/pi)    # 20 dB
#' dB <- cdTodb(10/pi)     # 30 dB
#' dB <- cdTodb(1/pi)      # 40 dB
#' dB <- cdTodb(0.1/pi)    # 50 dB
#' @export
cdTodb <- function(cd, maxStim = 10000/pi) {
    stopifnot(cd > 0)
    -10 * log10(cd / maxStim)
}
