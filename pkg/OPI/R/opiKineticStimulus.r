#' @rdname opiKineticStimulus
#' @title Stimulus parameter list
#' @description List containing stimulus parameters with an S3 class attribute
#' of \code{opiKineticStimulus}
#' @usage "See details"
#' @details The list should be of class opiKineticStimulus and contain the
#' following elements.
#'   * \code{path} list of (x,y) coordinates in degrees that is usable by \code{xy.coords()}
#'   * \code{image} \code{image[i]} is the image to display (in a machine specific format) in the section of the path specified by \code{path[i]..path[i+1]}. 
#'   * \code{levels} if \code{is.na(image)} then \code{levels[i]} is the stimulus level in cd/\eqn{\mbox{m}^2}{m^2} in the section of the path specified by \code{path[i]..path[i+1]}
#'   * \code{sizes} \code{sizes[i]} is the size of stimulus (diameter in degrees) to use for the section of path specified by \code{path[i]..path[i+1]}, or a scaling factor for \code{images[i]}.
#'   * \code{colors} \code{colors[i]} is the color to use for the stimulus in the section of path specified by \code{path[i]..path[i+1]}. Ignored if !is.na(image). 
#'   * \code{speeds}\code{speeds[i]} is the speed (degrees per second) for the stimulus to traverse the path specified by \code{path[i]..path[i+1]}.
#'   * \code{...} machine specific parameters
#'
#' # Octopus 900
#'   \code{x} and \code{y} are in degrees, with precision to three decimal places
#'     recognised.
#'     
#'   \code{image} is not possible on an Octopus 900.
#'   
#'   \code{levels} are in cd/\eqn{\mbox{m}^2}{m^2}, and are rounded to the nearest
#'     one tenth of a dB for display.
#'     
#'   \code{colors} are ignored. Use \code{opiSetBackground()} to alter stimulus color.
#'   
#'   \code{sizes} are in degrees, but are rounded to the nearest Goldmann Size I..V
#'     for display.
#'
#' # Kowa AP 7000
#'   Only a simple path with a start and an end point is supported by the AP-7000.
#'   
#'   \code{x} and \code{y} are in degrees and should only be length 2. (precision?)
#'   
#'   \code{image} is not possible on an Kowa AP 7000.
#'   
#'   \code{levels} are in cd/\eqn{\mbox{m}^2}{m^2} in the range 0.03 to 3183, and are
#'     rounded to the nearest one tenth of a dB for display. (precision?)
#'   
#'   \code{colors} one of \code{.OpiEnv$KowaAP7000$COLOR_WHITE},
#'   \code{.OpiEnv$KowaAP7000$COLOR_GREEN}, \code{.OpiEnv$KowaAP7000$COLOR_BLUE}, and
#'   \code{.OpiEnv$KowaAP7000$COLOR_RED}.
#'   
#'   \code{sizes} are in degrees, but are rounded to the nearest Goldmann Size I..V
#'     for display.
#'   
#'   \code{speeds} are in degrees per second in the range 3 to 5.
#'
#' # Compass
#'   Not implemented.
#' @seealso \code{\link{opiSetBackground}}, \code{\link{opiStaticStimulus}},
#' \code{\link{opiTemporalStimulus}}
#' @examples
#' # A Size III white kinetic stimuli on a bilinear path {(27,27), (15,20), (0,0)}
#' stim <- list(path=list(x=c(27,15,0), y=c(27,20,0)),
#'              izes=rep(0.43,2),
#'              colors=rep("white",2),
#'              levels=rep(318,2),
#'              speeds=c(4,3))
#' class(stim) <- "opiKineticStimulus"
opiKineticStimulus <- function() NULL