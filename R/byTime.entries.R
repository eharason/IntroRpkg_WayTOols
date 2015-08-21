#' byTime.entries {WayTOols}
#' @description Counts the number of entries by a factor of time in the logfile.
#' @usage byTime.entries(logfile_data, ID = "", ...)
#' @param logfile_data the data from the object's data frame. WayTOols is built
#' specifically to deal with logfile_data generated from the WayTO mobile application.
#' @param ID Each logfile has a unique ID corresponding to a participant in the experiment
#' @param ... Arguments to be passed to methods, such as na.rm=TRUE
#' @details This function provides users with the opportunity to count the number of
#' interactions by the user ID. This is important because the Time variable is the only
#' constant variable over the entire dataset.
#' @note test
#' @seealso byID.interactions, mean.interactions, plot.interactions, sum.interactions
#' @examples
#' ## count the number of entries by time in ID=4a
#' data(logfile_data)
#' byTime.entries(logfile_data, ID="4a")
#'
#' @export
byTime.entries <- function(logfile_data, ID = "", ...)
byTime.entries.default <- function(logfile_data, ID = "", ...) {
  byID <- logfile_data[logfile_data$ID == ID, ]
  TimebyID <- byID$Time
  col.ID <- list(attr(TimebyID, "Time"), TimebyID)
  cat("Summary of time interactions by ID\n")
  # print(TimebyID)
  print(paste("There are", length(byID$Time), "entries in ID:", ID)) # length returns number of entries
  invisible(byID)
}
