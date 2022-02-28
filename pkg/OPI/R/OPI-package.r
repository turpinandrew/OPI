#' @rdname OPI
#' @title Open Perimetry Interface
#' @description Implementation of the Open Perimetry Interface (OPI) for simulating
#' and controlling visual field machines using R. The OPI is a standard for
#' interfacing with visual field testing machines (perimeters). It specifies basic
#' functions that allow many visual field tests to be constructed. As of October 2017
#' it is fully implemented on the Octopus 900 and partially on the Heidelberg Edge
#' Perimeter, the Kowa AP 7000, the CrewT imo and the Centervue Compass.
#' It also has a cousin: the R package 'visualFields', which has tools for analysing
#' and manipulating visual field data. License: GPL-3
#' @details
#' \tabular{ll}{
#'   Package: \tab OPI\cr
#'   Type: \tab Package\cr
#'   Version: \tab 2.9\cr
#'   Date: \tab 2012-10-26\cr
#'   License: \tab GPL-3\cr
#' }
#' @seealso
#' \itemize{
#'   \item Andrew's website: \url{https://people.eng.unimelb.edu.au/aturpin/opi/index.html}
#'   \item Github OPI repository: \url{https://github.com/turpinandrew/OPI}
#'   \item OPI Discourse Forum: \url{https://openperimetry.org/}
#' }
#' @author Author and mantainer: Andrew Turpin <\email{aturpin@@unimelb.edu.au}>
#' @references
#' A. Turpin, P.H. Artes and A.M. McKendrick. "The Open Perimetry Interface: An enabling
#' tool for clinical visual psychophysics", Journal of Vision 12(11) 2012.
#'
#' @import methods
#' @importFrom grDevices dev.new dev.off dev.set dev.cur getGraphicsEvent grey xy.coords dev.size dev.list rgb col2rgb
#' @importFrom graphics grid par plot points rect rasterImage lines polygon symbols text
#' @importFrom stats pnorm runif rgamma
#' @importFrom utils head tail
#' @importFrom stats approx
"_PACKAGE"
