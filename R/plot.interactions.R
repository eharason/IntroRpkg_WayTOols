#' plot.interactions {WayTOols}
#' @aliases plot
#' @description Plots the locations where a specific user zoomed in or out of the digital map, and visualizes the zoom by size of zoom.
#' @usage plot.interactions(logfile_data, ID = "", ...)
#' @param logfile_data the data from the object's data frame. WayTOols is built
#' specifically to deal with logfile_data generated from the WayTO mobile application.
#' @param ID Each logfile has a unique ID corresponding to a participant in the experiment
#' @param ... Arguments to be passed to methods, such as na.rm=TRUE
#' @details Plots are generated to show the location-based zoom level interactions by user. This is interesting when analyzing the logfile data to see if users are constantly zooming throughout their experiment navigation
#' @note requires ggplot2 and ggmap
#' @seealso byTime.entries, byID.interactions, mean.interactions, sum.interactions
#' @examples
#' ## Plot interactions on a map and show boxplot
#' library(ggplot2)
#' library(ggmap)
#'
#' plot.interactions(logfile_data, ID="4a")
#'
#' ggplot(logfile_data, aes(factor(ID), Zoom), na.rm=TRUE) +
#' geom_boxplot() +
#' xlab("User ID")+
#' ggtitle("Zoom factor across different Users")

#' @export
plot.interactions <- function(logfile_data, ID = "", ...){
  require(ggmap)
  require(ggplot2)
  logfile_data_plot_Zoom_latlon <- logfile_data[logfile_data$ID == ID, ]
  map <- get_map(location = c(mean(logfile_data_plot_Zoom_latlon$Lon, na.rm = TRUE),
                              mean(logfile_data_plot_Zoom_latlon$Lat, na.rm = TRUE)), zoom=14, color = "bw")
  plot_Zoom_latlon <- ggmap(map) +
    geom_point(aes(x = Lon, y = Lat, colour = ID, size=Zoom),
               data = logfile_data_plot_Zoom_latlon, alpha = .5)+
    geom_line(aes(x = Lon, y = Lat, colour = ID),
              data = logfile_data_plot_Zoom_latlon, alpha = .5) +
    ggtitle(paste("Locations of zoom interactions for User", ID))
  print(plot_Zoom_latlon)
}
