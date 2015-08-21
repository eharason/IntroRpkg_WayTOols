#' meanZoom {WayTOols}
#' @aliases countZoom
#' @description Averages number of Zooms per user
#' @slot object A hard-coded argument for User ID and its average ID. See example for details
#' @slot User class specifies the ID and interaction parameters
#' @slot data the logfile data read as a data frame.
#' @slot ID Each logfile has a unique ID corresponding to a participant in the experiment
#' @slot Zoom is the value of zoom level (e.g. 14.0) measured at each map interaction
#' @slot Zoomed returns a TRUE/FALSE value for each interaction. It enables the user to know whether the interaction was a Zoom into the map or not.
#' @section S4 Method
#' @usage meanZoom(object)
#' @details The meanZoom method and the countZoom method are able to calculate the average and total zoom for a specific participant. The participant's data must first be loaded into the workspace.
#' @note A User.ID must first be hard coded before the meanZoom or countZoom methods can return the average zoom level.
#' @seealso countZoom
#' @examples
#' ## Set User ID
#' #User.9w <- new("User",
#' #ID = "ID",
#' #Zoom = logfile_data$Zoom[logfile_data$ID == "9w"] ,
#' #Zoomed = logfile_data$Zoomed[logfile_data$ID == "9w"] )
#'
#' ## meanZoom(User.9w)
#'
#' @export

setClass("User", representation(ID = "character", data = "data.frame",
                                Zoom = "numeric", Zoomed = "logical"))
setClass("meanZoom", representation(mean = "numeric" ))
meanZoom <- function(object) {
  meanZoom <- mean(object@Zoom)
  meanZoom
}
setMethod("meanZoom", signature(object = "User"), function(object) {
  object@meanZoom
})
setGeneric("meanZoom", function(object) {
  standardGeneric("meanZoom")
  cat(object@ID, "\n")
  cat("meanZoom:", mean(object@Zoom))
})
# User.9w <- new("User",
#                 ID = "ID",
#                 data = logfile_data,
#                 Zoom = logfile_data$Zoom[logfile_data$ID == "9w"] ,
#                 Zoomed = logfile_data$Zoomed[logfile_data$ID == "9w"] )

