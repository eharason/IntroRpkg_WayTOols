#' mean.interactions {WayTOols}
#' @aliases mean
#' @description Returns the average amount of Zoom and Pan interactions by user
#' @usage mean.interactions(logfile_data, ID = "", ...)
#' @param logfile_data the data from the object's data frame. WayTOols is built
#' specifically to deal with logfile_data generated from the WayTO mobile application.
#' @param ID Each logfile has a unique ID corresponding to a participant in the experiment
#' @param ... Arguments to be passed to methods, such as na.rm=TRUE
#' @details For a given ID, the average zoom level and average percentage of panning for the digital map are returned. This information is useful in that we can see the degree of panning (is it a lot? is the participant interacting a lot with the map?) and zooming (what is the average zoom level? Are they mostly zoomed in close to map elements, or far away?)
#' @seealso byTime.entries, byID.interactions, plot.interactions, sum.interactions
#' @examples
#' data(logfile_data)
#' mean.interactions(logfile_data, ID="6wo")

#' @export
mean.interactions <- function(logfile_data, ID = "", ...){
  data <- data.frame(logfile_data)
  logfile_mean_function <- data[data$ID == ID, ]
  Zoom.Mean <- logfile_mean_function$Zoom
  Panned.MeanPercent <- ifelse(logfile_mean_function$Panned, 100,0)
  print(paste("ID:", ID))
  apply(cbind(Zoom.Mean, Panned.MeanPercent), 2, mean, na.rm = TRUE)
}

