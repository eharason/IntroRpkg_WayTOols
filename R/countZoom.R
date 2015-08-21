#' countZoom {WayTOols}
#' @aliases meanZoom
#' @description Counts number of Zooms per user
#' @usage countZoom(object)
#' @details The sum of the number of zoom interactions per user.
#' @note see utils
#' @seealso utils
#' @examples
#' ## Set User ID
#' #User.13w <- new("User",
#' #ID = "ID",
#' #Zoom = logfile_data$Zoom[logfile_data$ID == "13w"] ,
#' #Zoomed = logfile_data$Zoomed[logfile_data$ID == "13w"] )
#'
#' ## countZoom(User.13w)
#'
#' @export
setClass("User", representation(ID = "character", data = "data.frame",
                                Zoom = "numeric", Zoomed = "logical"))
setClass("countZoom", representation(mean = "numeric" ))
countZoom <- function(object) {
  countZoom <- sum(object@Zoomed, na.rm=TRUE)
  countZoom
}
setMethod("countZoom", signature(object = "User"), function(object) {
  object@countZoom
})
setGeneric("countZoom", function(object) {
  standardGeneric("countZoom")
  cat(object@ID, "\n")
  cat("countZoom:", sum(object@Zoomed, na.rm=TRUE))
})
# User.13w <- new("User",
#                 ID = "ID",
#                 data = logfile_data,
#                 Zoom = logfile_data$Zoom[logfile_data$ID == "13w"] ,
#                 Zoomed = logfile_data$Zoomed[logfile_data$ID == "13w"] )
