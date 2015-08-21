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

## ----message=FALSE-------------------------------------------------------
summary(logfile_data$ID)
head(byID.interactions(logfile_data, ID = "11a"), 10)
byTime.entries(logfile_data, ID="9w")
sum.interactions(logfile_data, ID = "10wo")
mean.interactions(logfile_data, ID = "15w")

x <- as.numeric(logfile_data$Zoom, ID="3w")
x <- head(x, 50)
mean(x)

## ----plot, dpi=100, fig.width=7, message=FALSE---------------------------

require(ggmap)
require(ggplot2)

ggplot(logfile_data, aes(factor(ID), Zoom)) + 
  geom_boxplot() + 
  xlab("User")+
  ggtitle("Zoom factor across different Users")

plot.interactions(logfile_data, ID="4a")

