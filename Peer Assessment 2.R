##read in data
data <- read.csv("repdata-data-StormData.csv.bz2", stringsAsFactors = FALSE)
library(ggplot2)
library(gridExtra)
library(data.table)
colnames(data)
dim(data)

##creates vector of all event types
eventTypes <- data$EVTYPE

##finds the number of each unique value in eventTypes
no_unique <- aggregate(data.frame(count = eventTypes), list(value = eventTypes),
                       length)

##subsets all rows that have an event type that appears less than 10 times
subset_events <- no_unique[(no_unique$count > 10),]

##creates vectors of the event names to be used in analysis:
events <- subset_events$value

##creates subset of original data usinig only event names occuring more than 10
##times resulting in only 00.19% loss of data
subset_data <- data[data$EVTYPE %in% events,]
(nrow(data) - nrow(subset_data))/nrow(data)

##renaming event names to combine similar events together
eventType <- subset_data$EVTYPE
subset_data$EVTYPE[grep("FLOOD", eventType, ignore.case = TRUE)] <- "FLOOD"
subset_data$EVTYPE[grep("FLD", eventType, ignore.case = TRUE)] <- "FLOOD"
subset_data$EVTYPE[grep("THUNDERSTORM", eventType, ignore.case = TRUE)] <- "THUNDERSTORM"
subset_data$EVTYPE[grep("TSTM", eventType, ignore.case = TRUE)] <- "THUNDERSTORM"
subset_data$EVTYPE[grep("SNOW", eventType, ignore.case = TRUE)] <- "WINTER WEATHER"
subset_data$EVTYPE[grep("BLIZZARD", eventType, ignore.case = TRUE)] <- "WINTER STORM"
subset_data$EVTYPE[grep("WINTER STORM", eventType, ignore.case = TRUE)] <- "WINTER STORM"
subset_data$EVTYPE[grep("WINTER WEATHER", eventType, ignore.case = TRUE)] <- "WINTER WEATHER"
subset_data$EVTYPE[grep("WINTRY", eventType, ignore.case = TRUE)] <- "WINTER WEATHER"
subset_data$EVTYPE[grep("SLEET", eventType, ignore.case = TRUE)] <- "SLEET"
subset_data$EVTYPE[grep("AVALANCHE", eventType, ignore.case = TRUE)] <- "AVALANCHE"
subset_data$EVTYPE[grep("RAIN", eventType, ignore.case = TRUE)] <- "RAIN"
subset_data$EVTYPE[grep("WET", eventType, ignore.case = TRUE)] <- "RAIN"
subset_data$EVTYPE[grep("WINDCHILL", eventType, ignore.case = TRUE)] <- "EXTREME COLD"
subset_data$EVTYPE[grep("WIND CHILL", eventType, ignore.case = TRUE)] <- "EXTREME COLD"
subset_data$EVTYPE[grep("WIND", eventType, ignore.case = TRUE)] <- "WIND"
subset_data$EVTYPE[grep("HAIL", eventType, ignore.case = TRUE)] <- "HAIL"
subset_data$EVTYPE[grep("FREEZ", eventType, ignore.case = TRUE)] <- "ICY CONDITIONS"
subset_data$EVTYPE[grep("ICE", eventType, ignore.case = TRUE)] <- "ICY CONDITIONS"
subset_data$EVTYPE[grep("ICY", eventType, ignore.case = TRUE)] <- "ICY CONDITIONS"
subset_data$EVTYPE[grep("HEAT", eventType, ignore.case = TRUE)] <- "HOT WEATHER"
subset_data$EVTYPE[grep("WARM", eventType, ignore.case = TRUE)] <- "HOT WEATHER"
subset_data$EVTYPE[grep("COLD", eventType, ignore.case = TRUE)] <- "COLD WEATHER"
subset_data$EVTYPE[grep("COOL", eventType, ignore.case = TRUE)] <- "COLD WEATHER"
subset_data$EVTYPE[grep("FUNNEL", eventType, ignore.case = TRUE)] <- "FUNNEL CLOUD"
subset_data$EVTYPE[grep("TORNADO", eventType, ignore.case = TRUE)] <- "TORNADO"
subset_data$EVTYPE[grep("TIDE", eventType, ignore.case = TRUE)] <- "TIDE LEVELS"
subset_data$EVTYPE[grep("HURRICANE", eventType, ignore.case = TRUE)] <- "HURRICANE(TYPHOON)"
subset_data$EVTYPE[grep("SURF", eventType, ignore.case = TRUE)] <- "HIGH SURF"
subset_data$EVTYPE[grep("CURRENT", eventType, ignore.case = TRUE)] <- "RIP CURRENT"
subset_data$EVTYPE[grep("SURGE", eventType, ignore.case = TRUE)] <- "STORM SURGE"
subset_data$EVTYPE[grep("TSUNAMI", eventType, ignore.case = TRUE)] <- "TSUNAMI"
subset_data$EVTYPE[grep("TYPHOON", eventType, ignore.case = TRUE)] <- "HURRICANE(TYPHOON"
subset_data$EVTYPE[grep("WATERSPOUT", eventType, ignore.case = TRUE)] <- "WATERSPOUT"
subset_data$EVTYPE[grep("FIRE", eventType, ignore.case = TRUE)] <- "WILDFIRE"
subset_data$EVTYPE[grep("DRY", eventType, ignore.case = TRUE)] <- "DRY CONDITIONS"
subset_data$EVTYPE[grep("DROUGHT", eventType, ignore.case = TRUE)] <- "DRY CONDITIONS"
subset_data$EVTYPE[grep("DUST", eventType, ignore.case = TRUE)] <- "DRY CONDITIONS"
subset_data$EVTYPE[grep("FOG", eventType, ignore.case = TRUE)] <- "FOG"
subset_data$EVTYPE[grep("GLAZE", eventType, ignore.case = TRUE)] <- "GLAZE"
subset_data$EVTYPE[grep("TEMPERATURE", eventType, ignore.case = TRUE)] <- "RECORD TEMPERATURE"

##This leaves us with 45 unique event types
unique(data$EVTYPE)

##Total fatalities and injuries incurred for each event type
total_fatalities <- aggregate(FATALITIES ~ EVTYPE, data = subset_data, sum)
total_injuries <- aggregate(INJURIES ~ EVTYPE, data = subset_data, sum)
mean_fatalities <- aggregate(FATALITIES ~ EVTYPE, data = subset_data, mean)
mean_injuries <- aggregate(INJURIES ~ EVTYPE, data = subset_data, mean)

##Create bar plots of fatalities and injuries

p1 <- ggplot(total_fatalities, aes(x = reorder(EVTYPE, -FATALITIES),
                                      y = FATALITIES)) +
    geom_bar(stat = "identity") + theme(text = element_text(size = 8),
                                        axis.text.x = element_text(angle = 90)) +
    xlab("Event Type") + ggtitle("TOTAL FATALITIES PER EVENT TYPE") +
    ylab("Total Number of Fatalities")

p2 <- ggplot(total_injuries, aes(x = reorder(EVTYPE, -INJURIES),
                                      y = INJURIES)) +
    geom_bar(stat = "identity") + theme(text = element_text(size = 8),
                                        axis.text.x = element_text(angle = 90)) +
    xlab("Event Type") + ggtitle("TOTAL INJURIES PER EVENT TYPE") +
    ylab("Total Number of Injuries")

p3 <- ggplot(mean_fatalities, aes(x = reorder(EVTYPE, -FATALITIES),
                                      y = FATALITIES)) +
    geom_bar(stat = "identity") + theme(text = element_text(size = 8),
                                        axis.text.x = element_text(angle = 90)) +
    xlab("Event Type") + ggtitle("MEAN FATALITIES PER EVENT TYPE") + 
    ylab("Mean Number of Fatalities")

p4 <- ggplot(mean_injuries, aes(x = reorder(EVTYPE, -INJURIES),
                                      y = INJURIES)) +
    geom_bar(stat = "identity") + theme(text = element_text(size = 8),
                                        axis.text.x = element_text(angle = 90)) +
    xlab("Event Type") + ggtitle("MEAN INJURIES PER EVENT TYPE") +
    ylab("Mean Number of Injuries")

grid.arrange(p1, p2, main = "TOTAL FATALITIES AND INJURIES")
grid.arrange(p3, p4, main = "MEAN FATALITIES AND INJURIES")

##find the totals of injuries and fatalities for both sums and means
total_health <- as.data.table(cbind(total_fatalities,total_injuries))
mean_health <- as.data.table(cbind(mean_fatalities, mean_injuries))

total_health[,HEALTH_TOTALS:=FATALITIES + INJURIES]
mean_health[,HEALTH_MEANS:=FATALITIES + INJURIES]

#Order table from greatest to least based on the totals
setorder(total_health, -HEALTH_TOTALS)
setorder(mean_health, -HEALTH_MEANS)

##Tidying up the data tables:
total_health[, EVTYPE:=NULL]
total_health[, FATALITIES:=NULL]
total_health[, INJURIES:=NULL]

mean_health[, EVTYPE:=NULL]
mean_health[, FATALITIES:=NULL]
mean_health[, INJURIES:=NULL]

total_health[1:10,]
mean_health[1:10,]

