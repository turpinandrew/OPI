#' @rdname opiTemporalStimulus
#' @title Stimulus parameter list
#' @description List containing stimulus parameters with an S3 class attribute
#' of \code{opiTemporalStimulus}
#' @usage "See details"
#' @details The list should be of class opiTemporalStimulus and contain the
#' following elements.
#' \itemize{
#'   \item\code{x} coordinate of the center of stimulus in degrees relative to fixation
#'   \item\code{y} coordinate of the center of stimulus in degrees relative to fixation
#'   \item\code{image} an image to display in a machine specific format \item\code{lut}
#'     if \code{is.na(image)} then this is a lookup table (vector) for stimulus level at
#'     each step of rate Hz in cd/\eqn{\mbox{m}^2}{m^2}. If image is specified, then
#'     this is a list of images, in the same format as image, that is stepped through at
#'     rate Hz.
#'   \item\code{size} diameter of target in degrees, or scaling factor for
#'     image if specified 
#'   \item\code{color} machine specific stimulus color settings (ignored if
#'     \code{!is.na(image)}) 
#'   \item\code{rate} frequency with which lut is processed in Hz
#'   \item\code{duration}total length of stimulus flash in milliseconds. There is no
#'     guarantee that \code{duration \%\% length(lut)/rate == 0}. That is, the
#'     onus is on the user to ensure the duration is a multiple of the period of the
#'     stimuli.
#'   \item\code{responseWindow} maximum time (>= 0) in milliseconds to wait for a
#'     response from the onset of the stimulus presentation
#'   \item\code{...} machine specific parameters
#' }
#' \subsection{Octopus 900}{
#'   \code{x} and \code{y} are in degrees, with precision to one decimal place
#'     recognised.
#'   
#'   \code{image} is not possible on an Octopus 900.
#'   
#'   \code{lut} is not possible on an Octopus 900. Stimulus is at 0 dB.
#'   
#'   \code{rate} is in Hz, with precision to one decimal place recognised.
#'   
#'   \code{color} is ignored. Use \code{opiSetBackground()} to alter stimulus color.
#' }
#' \subsection{Kowa AP-7000}{Not supported.}
#' \subsection{Compass}{Not implemented.}
#' @seealso \code{\link{opiSetBackground}}, \code{\link{opiStaticStimulus}},
#' \code{\link{opiKineticStimulus}}
#' @examples
#' # A Size III flickering with a 10Hz square wave at location (7,7) with luminance
#' # 10 dB (HFA) 
#' stim <- list(x=7, y=7, size=0.43, color="white",
#'              rate=20,        # one lut step per 50 ms
#'              lut=c(0,318),   # so one full lut per 100 ms == 10Hz
#'              duration=400,   # and 4 cycles per stimulus
#'              responseWindow=1500)
#' class(stim) <- "opiTemporalStimulus"
opiTemporalStimulus <- function() NULL