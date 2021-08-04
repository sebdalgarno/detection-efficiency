library(tidyverse)
library(magrittr)
library(jmbr)
library(extras)
library(mbr)

rm(list = ls())
graphics.off()
set_analysis_mode("report")

sentinel <- read_csv("data/brownscombe/sentinel.csv")

sentinel <- sentinel %>%
  transmute(Station = as.factor(ID),
            Timestep = hour,
            Lat = lat,
            Lon = lon,
         Rugosity = factor(rugosity, levels = c(1, 2, 3)),
         DielPeriod = Diel,
         TideM = tidemeters,
         TideState = factor(tidestate, levels = c("incoming", "high", "outgoing", "low")),
         Benthos = benthos,
         Habitat = habitat,
         Depth = depth,
         ## should be around 11 pings per hour, Detects should max at 11
         Pings = 11,
         Detects = if_else(Det > 11, 11, Det))

ggplot(sentinel, aes(x = Timestep, y = Detects/Pings)) +
  geom_point() + 
  facet_wrap(~Station)


  