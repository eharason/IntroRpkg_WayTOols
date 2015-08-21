## ------------------------------------------------------------------------
require(WayTOols)
data("User.9w")
library(stringr)
library(dplyr)
library(ggplot2)
library(ggmap)

## ------------------------------------------------------------------------
# call functions
# The function readClickedMap takes
# a string that has the tag "Clicked on map"
# a row from the data set to be filled with the information available
# in the string (information available: time)

# Example: Clicked on map at time: Wed May 20 15:03:14 GMT+02:00 2015

readClickedMap <- function(Line_String, logfile_data){

  # Time
  position_Time_Start <- str_locate(pattern ="at time:", Line_String)
  position_Time_Stop <- nchar(Line_String)

  # Move the cut out indexes to the appropriate spots
  position_Time_Start <- position_Time_Start[,2]+2

  # Fill data with Information about Time, using the positions of this data defined earlier
  Time_String <- substr(Line_String, start = position_Time_Start,
                        stop = position_Time_Stop)

  logfile_data$Time <- Time_String

  logfile_data
}

# The function readClickedMarker takes
# a string that has the Action tag "Clicked on marker"
# a row from the data set to be filled with the information available
# in the string (information available: landmark, zoom, and time)

# Example: Clicked on marker: Netto at zoom level: 14.256604 at time: Wed May 20 15:03:06 GMT+02:00 2015

readClickedMarker <- function(Line_String, logfile_data){

  # Landmark
  # Locate landmark tags in data
  position_ClickedMarker_Start <- str_locate(pattern ="marker:", Line_String)
  position_ClickedMarker_Stop <- str_locate(pattern ="at zoom level:", Line_String)

  # Move the cut out indexes to the appropriate spots
  position_ClickedMarker_Start <- position_ClickedMarker_Start[,2]+2
  position_ClickedMarker_Stop <- position_ClickedMarker_Stop[,1]-2

  # Fill data with Information about Clicked Marker, using the positions of this data defined earlier
  ClickedMarker_String <- substr(Line_String, start = position_ClickedMarker_Start,
                                 stop = position_ClickedMarker_Stop)

  logfile_data$Landmark <- ClickedMarker_String

  # Zoom
  # Example: Clicked on marker: Netto at zoom level: 14.256604 at time: Wed May 20 15:03:06 GMT+02:00 2015
  position_Zoom_Start <- str_locate(pattern ="zoom level:", Line_String)
  position_Zoom_Stop <- str_locate(pattern ="at time:", Line_String)

  # Move the cut out indexes to the appropriate spots
  position_Zoom_Start <- position_Zoom_Start[,2]+2
  position_Zoom_Stop <- position_Zoom_Stop[,1]-1

  # Fill logfile_data with information about Zoom, using the positions of this data defined earlier
  Zoom_String <- substr(Line_String, start = position_Zoom_Start,
                        stop = position_Zoom_Stop)
  logfile_data$Zoom <- as.numeric(Zoom_String)

  # Time
  # Example: Clicked on marker: Netto at zoom level: 14.256604 at time: Wed May 20 15:03:06 GMT+02:00 2015
  position_Time_Start <- str_locate(pattern ="at time:", Line_String)
  position_Time_Stop <- nchar(Line_String) # stop at the end of the string

  # Move the cut out indexes to the appropriate spots
  position_Time_Start <- position_Time_Start[,2]+2

  # Fill data with Information about Time, using the positions of this data defined earlier
  Time_String <- substr(Line_String, start = position_Time_Start,
                        stop = position_Time_Stop)

  logfile_data$Time <- Time_String
  logfile_data
}

# The function readMapMoved takes
# a string that has the tag "Map moved to position"
# a row from the data set to be filled with the information available
# in the string (information available: latitude, longitude, zoom, and time)

# Example: Map moved to position: lat/lng: (51.94720574654645,7.6224299892783165)
# at zoom level: 14.0 at time: Wed May 20 15:02:19 GMT+02:00 2015

### TEST ###
# Line_String <- "Map moved to position: lat/lng: (51.94720574654645,7.6224299892783165)
# at zoom level: 14.0 at time: Wed May 20 15:02:19 GMT+02:00 2015"

readMapMoved <- function(Line_String, logfile_data){

  # Lat long
  # Locate lat/lng tags in data
  # Example: lat/lng: (51.94720574654645,7.6224299892783165) at zoom level: 14.0
  position_LatLong_Start <- str_locate(pattern ="lat/lng:", Line_String)
  position_LatLong_Stop <- str_locate(pattern ="at zoom level:", Line_String)

  # Move the cut out indexes to the appropriate spots, brackets in data are removed
  position_LatLong_Start <- position_LatLong_Start[,2]+3
  position_LatLong_Stop <- position_LatLong_Stop[,1]-3

  # Fill data with information about LatLong, using the positions of this data defined earlier
  LatLon_String <- substr(Line_String, start = position_LatLong_Start,
                          stop = position_LatLong_Stop)

  logfile_data$Lat <- as.numeric(unlist(strsplit(LatLon_String, ",")))[1]
  logfile_data$Lon <- as.numeric(unlist(strsplit(LatLon_String, ",")))[2]

  # Zoom
  # Example: lat/lng: (51.94720574654645,7.6224299892783165) at zoom level: 14.0
  position_Zoom_Start <- str_locate(pattern ="zoom level:", Line_String)
  position_Zoom_Stop <- str_locate(pattern ="at time:", Line_String)

  # Move the cut out indexes to the appropriate spots
  position_Zoom_Start <- position_Zoom_Start[,2]+2
  position_Zoom_Stop <- position_Zoom_Stop[,1]-1

  # Fill data with Information about Zoom, using the positions of this data defined earlier
  Zoom_String <- substr(Line_String, start = position_Zoom_Start,
                        stop = position_Zoom_Stop)
  logfile_data$Zoom <- as.numeric(Zoom_String)

  # Time
  # Example: at time: Wed May 20 15:02:19 GMT+02:00 2015
  position_Time_Start <- str_locate(pattern ="at time:", Line_String)
  position_Time_Stop <- nchar(Line_String)

  # Move the cut out indexes to the appropriate spots
  position_Time_Start <- position_Time_Start[,2]+2

  # Fill data with Information about Time, using the positions of this data defined earlier
  Time_String <- substr(Line_String, start = position_Time_Start,
                        stop = position_Time_Stop)

  logfile_data$Time <- Time_String

  logfile_data
}

##### Function ########

readLogfiles <- function(filename){

  # Read the entire txt file as one string
  # Export format was not consitent, therefore necessary to identify certain
  # tags within the data to extract the valuable information

  string <- readChar(filename, file.info(filename)$size)

  position_Lines_Tag <- str_locate_all(pattern =" 2015", string)
  position_Lines_Stop <- position_Lines_Tag[[1]][,2]

  position_Lines_Start <- position_Lines_Stop + 1
  position_Lines_Start[2:(length(position_Lines_Start))] <- position_Lines_Start[1:(length(position_Lines_Start)-1)]
  position_Lines_Start[1] <- 1

  position_Lines <- data.frame(start = position_Lines_Start, stop = position_Lines_Stop)

  # Prepare Data set with Columns for ID, Lat, Long
  logfile_data <- data.frame(ID = rep(strsplit(filename, ".txt")[[1]], nrow(position_Lines)),
                             Action = rep(NA,nrow(position_Lines)),
                             Landmark = rep(NA, nrow(position_Lines)),
                             Time = rep(NA,nrow(position_Lines)),
                             Zoom = rep(NA, nrow(position_Lines)),
                             Lat = rep(NA, nrow(position_Lines)),
                             Lon = rep(NA, nrow(position_Lines)))

  for (i in 1:nrow(position_Lines)){
    Line_String <- substr(string, start = position_Lines$start[i],
                          stop = position_Lines$stop[i])

    Action_MapMoved <- !is.na(str_locate(pattern = "Map moved to position", string = Line_String)[1])
    Action_ClickedMarker <- !is.na(str_locate(pattern = "Clicked on marker", string = Line_String)[1])
    Action_ClickedMap <- !is.na(str_locate(pattern = "Clicked on map", string = Line_String)[1])

    if (Action_MapMoved){
      logfile_data$Action[i] <- "Map moved to position"
      logfile_data[i,] <- readMapMoved(Line_String, logfile_data[i,])
    }
    if (Action_ClickedMarker){
      logfile_data$Action[i] <- "Clicked on marker"
      logfile_data[i,] <- readClickedMarker(Line_String, logfile_data[i,])
    }
    if (Action_ClickedMap){
      logfile_data$Action[i] <- "Clicked on map"
      logfile_data[i,] <- readClickedMap(Line_String, logfile_data[i,])
    }
  }

  logfile_data
}

##### Read Data ########

files <- list.files(pattern = "\\.txt$")
logfile_data <- NULL
for (filename in files){
  logfile_data <- rbind(logfile_data, readLogfiles(filename))
}


# # Add Lat/Lon information to clicked on marker and clicked on map
# Assumption: Lat/Lon information is not included with clicked on map/marker, therefore we will
# assume that the lat/lon will be the same as the lat/lon information from the previous "moved to position" string
index_Clicked <- c(grep("Clicked on marker", logfile_data$Action), grep("Clicked on map", logfile_data$Action))
index_Clicked <- index_Clicked[order(index_Clicked)]

for (index in index_Clicked){
  logfile_data[index, ]$Lat <- logfile_data[index - 1, ]$Lat
  logfile_data[index, ]$Lon <- logfile_data[index - 1, ]$Lon
}

# Fill in zoom information for "clicked on map"
index_ClickedMapZoom <- grep("Clicked on map", logfile_data$Action)

for (index in index_ClickedMapZoom){
  logfile_data[index, ]$Zoom <- logfile_data[index - 1, ]$Zoom
}

logfile_data$Zoomed <- NA
logfile_data$Panned <- NA

# For loop over the IDs
for (ID in unique(logfile_data$ID)){

  # Identify if user moved by evaluating if lat or lon changed
  LonDif <- c(NA, diff(logfile_data[logfile_data$ID == ID, ]$Lon))
  LatDif <- c(NA, diff(logfile_data[logfile_data$ID == ID, ]$Lat))

  logfile_data[logfile_data$ID == ID, ]$Panned <- LonDif !=0 | LatDif !=0

  # Identify if user zoomed by evaluating if zoom changed
  ZoomDif <- c(NA, diff(logfile_data[logfile_data$ID == ID, ]$Zoom))
  logfile_data[logfile_data$ID == ID, ]$Zoomed <- ZoomDif!=0

}

# define WithOffScreenMarkers
logfile_data$WithOffScreenMarkers <- NA

# It is TRUE if the .txt file represents a participant with off-screen markers visible if there is an "a"
# or a "w" in the file ID. If there is a "b" or a "wo" in the file ID, then the off-screen markers were
# not visible, and FALSE will be displayed. grep is used to search for patterns
logfile_data$WithOffScreenMarkers[c(grep("a", logfile_data$ID), grep("w", logfile_data$ID))] <- TRUE
logfile_data$WithOffScreenMarkers[c(grep("b", logfile_data$ID), grep("wo", logfile_data$ID))] <- FALSE


