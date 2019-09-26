#' Convert pixels to degrees for machine 'machine'
#'
#' @param xy a 2 element vector c(x,y) where x and y are in pixels
#' @param machine "compass" or ...?
#'
#' @return xy converted to degrees of visual field with the usual conventions or \code{NA} if machine is unknown
#'
#' @examples
#' pixToDeg(c(1000, 200), machine="compass")  # c(1.290323, 24.516129) degrees
#' pixToDeg(c(1920/2, 1920/2)) # c(0,0) degrees
#'
pixToDeg <- function(xy, machine="compass") { 
    if (machine == "compass")
        return(c((xy[1] - 1920/2), 1920/2 - xy[2]) / 31) 

    return(NA)
}

#' Convert degrees to pixels for machine 'machine'
#'
#' @param xy a 2 element vector c(x,y) where x and y are in degrees on a visual field with the usual conventions
#' @param machine "compass" or ...?
#'
#' @return xy converted to pixels (top-left is (0,0)) for the machine or \code{NA} if machine is unknown
#'
#' @examples
#' degToPix(c(0, 0), machine="compass")  # c(960, 960) pixels
#' degToPix(c(-15, 2)) # c(495, 898) pixels
#'
degToPix <- function(xy, machine="compass") { 
    if (machine == "compass")
        return(c(xy[1] * 31 + 1920/2, 1920/2 - xy[2]*31)) 

    return(NA)
}
