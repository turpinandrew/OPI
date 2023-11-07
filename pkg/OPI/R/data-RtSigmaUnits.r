#' @title Response times to white-on-white Goldmann Size III targets for 12 subjects
#' in sigma units
#' @description Response times to white-on-white Goldmann Size III targets for 12
#' subjects. The second column is the distance of the stimuli from measured threshold
#' in 'sigma' units. The threshold was determined by post-hoc fit of a cumulative
#' gaussian FoS curve to the data for each location and subject. Sigma is the
#' standard deviation of the fitted FoS.
#' @details
#' A data frame with 30620 observations on the following 3 variables.
#'   * \code{Rt} Reaction time in ms.
#'   * \code{Dist} Distance of stimuli from threshold in sigma units.
#'   * \code{Person} Identifier of each subject.
#' 
#' @keywords dataset
#' @seealso \code{\link{RtDbUnits}}
#' @references
#' A.M. McKendrick, J. Denniss and A. Turpin. "Response times across the visual
#' field: empirical observations and application to threshold determination".
#' Vision Research 101 2014.
"RtSigmaUnits"