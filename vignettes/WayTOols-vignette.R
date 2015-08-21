## ----message=FALSE-------------------------------------------------------
require(dplyr)
require(WayTOols)
data(logfile_data)
head(logfile_data, 5)

## ----message=FALSE-------------------------------------------------------
colnames(logfile_data)

## ----message=FALSE-------------------------------------------------------
logfile.interactions <- cbind(logfile_data)[, c("ID", "Zoomed", "Panned", "WithOffScreenMarkers")]

require(dplyr)
countZP.dataframe <- logfile.interactions %>% group_by(ID) %>% 
  summarise(sumZoomed = sum(Zoomed, na.rm=TRUE), 
            sumPanned = sum(Panned, na.rm=TRUE)) 

