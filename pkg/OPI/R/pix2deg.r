# Open Perimetry Interface functions for converting pixels to visual angle.
#
# Copyright [2023] [Andrew Turpin & Ivan Marin-Franch]
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

#'
#' Convert pixels to degrees for machine 'machine'
#'
#' @param xy a 2 element vector c(x,y) where x and y are in pixels
#' @param machine "compass" or ...?
#'
#' @return xy converted to degrees of visual field with the usual conventions or \code{NA} if machine is unknown
#'
#' @examples
#' pixTodeg(c(1000, 200), machine="compass") # c(1.290323, 24.516129) degrees
#' pixTodeg(c(1920/2, 1920/2)) # c(0,0) degrees
#' @export
pixTodeg <- function(xy, machine = "compass") {
    if (machine == "compass")
        return(c((xy[1] - 1920 / 2), 1920 / 2 - xy[2]) / 31)

    return(NA)
}

#' Convert degrees to pixels for machine 'machine'
#'
#' @param xy a 2 element vector c(x,y) where x and y are in pixels
#' @param machine "compass" or ...?
#' 
#' @return xy converted to pixels (top-left is (0,0)) for the machine or
#' \code{NA} if machine is unknown
#'
#' @examples
#' degTopix(c(0, 0), machine="compass")  # c(960, 960) pixels
#' degTopix(c(-15, 2)) # c(495, 898) pixels
#' @export
degTopix <- function(xy, machine = "compass") {
    if (machine == "compass")
        return(c(xy[1] * 31 + 1920 / 2, 1920 / 2 - xy[2]*31))

    return(NA)
}
