library(googleVis)
library(plyr)
library(ggplot2)
library(shiny)

load("data/serves.RData")


top_servers <- ddply(serves, "FirstPlayer", summarize,
	Player = FirstPlayer[1],
	"First" = round(mean(AvgFirst, na.rm = T), 1),
	"Difference" = round(mean(Difference, na.rm = T),1),
	"Recorded Serves" = sum(!is.na(AvgFirst))
)

top_servers <- top_servers[top_servers[,"Recorded Serves"]>=10,]