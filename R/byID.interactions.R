#' byID.interactions {WayTOols}
#' @description Take a sequence of logfile data frame arguments and combine
#' by "ID", "Zoomed", "Panned" and "WithOffScreenMarkers" by columns.
#' @usage byID.interactions(logfile_data, ID = "", ...)
#' @param logfile_data the data from the object's data frame. WayTOols is built
#' specifically to deal with logfile_data generated from the WayTO mobile application.
#' @param ID Each logfile has a unique ID corresponding to a participant in the experiment
#' @param ... Arguments to be passed to methods, such as na.rm=TRUE
#' @details This function cleans up the original data frame "logfile_data" to include
#' only the events corresponding to user interaction with the digital map. This function
#' enables the choice of which ID should be displayed in the condensed data frame.
#' @seealso byTime.entries, mean.interactions, plot.interactions, sum.interactions
#' @examples
#' ## when na.rm = TRUE
#' data(logfile_data)
#' byID.interactions(logfile_data, ID="13w")

#' @export
byID.interactions <- function(logfile_data, ID = "", ...)
byID.interactions.default <- function(logfile_data, ID = "", ...)
{
  logfile_data_byID_interactions <- logfile_data[logfile_data$ID == ID, ]
  byID <- cbind(logfile_data_byID_interactions)[, c("ID", "Zoomed",
                                                    "Panned", "WithOffScreenMarkers")]
  byID
}
