#' sum.interactions {WayTOols}
#' @aliases sum
#' @description Provides the sum of interactions per user by Zoom and Pan factor
#' @usage sum.interactions(logfile_data, ID = "", ...)
#' @param logfile_data the data from the object's data frame. WayTOols is built
#' specifically to deal with logfile_data generated from the WayTO mobile application.
#' @param ID Each logfile has a unique ID corresponding to a participant in the experiment
#' @param ... Arguments to be passed to methods, such as na.rm=TRUE
#' @details When the ID and logfile data are provided, the total amount of Zoom and Pan interactions are returned by this function.
#' @seealso byTime.entries, byID.interactions, plot.interactions, mean.interactions
#' @examples
#' require(dplyr)
#' data(logfile_data)
#' sum.interactions(logfile_data, ID="6wo")

#' @export
sum.interactions <- function(logfile_data, ID = "", ...){
  require(dplyr)
  logfile_data_sum <- logfile_data[logfile_data$ID == ID, ]
  countZP <- logfile_data_sum %>%
    summarise(sumZoomed = sum(Zoomed, na.rm=TRUE),
              sumPanned = sum(Panned, na.rm=TRUE))
  print(paste("ID:", ID))
  countZP
}
