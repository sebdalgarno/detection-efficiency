library(tidyverse)
library(magrittr)
library(jmbr)
library(extras)
library(mbr)

rm(list = ls())
graphics.off()
set_analysis_mode("report")

detrange <- read_csv("data/brownscombe/detrange.csv")
detrange <- detrange %>%
  transmute(Habitat = factor(habitat, levels=c("bank","channel","basin")),
         Station = factor(station, levels=c("BTT37", "BTT27", "BTT17","BTT49","BTT18","BTT54","BTT7","BTT13","BTT46")),
         Pings = 12,
         Detects = if_else(det > Pings, Pings, det),
         Distance = distm)

ggplot(detrange, aes(x = Distance, y = Detects/Pings, col = Habitat)) + 
  geom_point() + 
  facet_wrap( ~ Station) +
  ylim(c(0,1)) +
  xlab("Distance (m)") + 
  ylab("Detection efficiency")

data <- detrange %>% filter(Station == "BTT17")

model_slope <- model("model {
  bDistance ~ dnorm(0, 2^-2)
  for(i in 1:nObs) {
    logit(eDetects[i]) <- 10 + bDistance * Distance[i]
    Detects[i] ~ dbin(eDetects[i], Pings[i])
  }
}", new_expr = "
 for (i in 1:nObs) {
    logit(eDetects[i]) <- 10 + bDistance * Distance[i]
    midpoint[i] <- -b0/bDistance
   prediction[i] <- eDetects[i]
 }
", nthin = 10L)

model_midpoint <- model("model {
  bMidpoint ~ dunif(0, 1000)
  for(i in 1:nObs) {
    logit(eDetects[i]) <- 10 +  -10/bMidpoint * Distance[i]
    Detects[i] ~ dbin(eDetects[i], Pings[i])
  }
}", new_expr = "
 for (i in 1:nObs) {
    logit(eDetects[i]) <- 10 + -10/bMidpoint * Distance[i]
   prediction[i] <- eDetects[i]
 }
", nthin = 10L)

analysis <- analyse(model_slope, data = data)
coef(analysis, simplify = TRUE)
plot(analysis)

prediction_midpoint <- predict(analysis, new_data = character(0), 
                               term = "midpoint")
prediction <- predict(jags_analysis, new_data = "Distance")

ggplot(data = prediction, aes(x = Distance, y = estimate)) +
  geom_point(data = data, aes(y = Detects/Pings)) +
  geom_line() +
  geom_line(aes(y = lower), linetype = "dotted") +
  geom_line(aes(y = upper), linetype = "dotted") +
  NULL



