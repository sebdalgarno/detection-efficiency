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

data <- detrange 
model_distance <- model("model {

  muint ~ dnorm(5, 2^-2)
  sigmaint ~ dnorm(0, 5^-2) T(0,)

  mubdist ~ dnorm(0, 0.1^-2)
  sigmabdist ~ dnorm(0, 0.1^-2) T(0,)

  for(i in 1:nStation) {
    b0[i] ~ dnorm(muint, sigmaint^-2)
    bDistance[i] ~ dnorm(mubdist, sigmabdist^-2)
  }

  for(i in 1:nObs) {
    logit(eDetects[i]) <- b0[Station[i]] + bDistance[Station[i]] * Distance[i]
    Detects[i] ~ dbin(eDetects[i], Pings[i])
  }
}", new_expr = "
  for(i in 1:nObs) {
    logit(eDetects[i]) <- b0[Station[i]] + bDistance[Station[i]] * Distance[i]
    prediction[i] <- eDetects[i]
  }
", nthin = 20L)

analysis <- analyse(model_distance, data = data)
print(coef(analysis, simplify = TRUE), n = 100)
plot(analysis)

prediction <- predict(analysis, new_data = c("Distance", "Station"))

### next steps - try model with y-intercept at 100m
# try model with habitat as fixed effect
# compare model fit
data$Detects %<>% as.integer()
data$Pings %<>% as.integer()
model_habitat <- model("model {

  muint ~ dnorm(5, 2^-2)
  sigmaint ~ dnorm(0, 5^-2) T(0,)

  mubdist ~ dnorm(0, 0.1^-2)
  sigmabdist ~ dnorm(0, 0.1^-2) T(0,)

  for(i in 1:nStation) {
    b0[i] ~ dnorm(muint, sigmaint^-2)
    bDistance[i] ~ dnorm(mubdist, sigmabdist^-2)
  }
  
  bHabitat[1] <- 0
  for(i in 2:nHabitat) {
    bHabitat[i] ~ dnorm(0, 10^-2)
  }

  for(i in 1:nObs) {
    logit(eDetects[i]) <- b0[Station[i]] + bDistance[Station[i]] * Distance[i] + bHabitat[Habitat[i]] 
    Detects[i] ~ dbin(eDetects[i], Pings[i])
  }
}", new_expr = "
  for(i in 1:nObs) {
    logit(eDetects[i]) <- b0[Station[i]] + bDistance[Station[i]] * Distance[i] + bHabitat[Habitat[i]] 
    prediction[i] <- eDetects[i]
  }
", nthin = 50L, select_data = list(Detects = c(0L, 10000L), 
                                   `Distance+` = c(0, 1000),
                                   Habitat = factor(),
                                   Station = factor(),
                                   Pings = c(1L, 10000L)), random_effects = list(bDistance = "Station",
                                                                                 b0 = "Station"))

analysis_habitat <- analyse(model_habitat, data = data)
print(coef(analysis_habitat, simplify = TRUE), n = 100)
plot(analysis_habitat)

habitat <- data %>% select(Station, Habitat)
prediction_habitat <- predict(analysis_habitat, new_data = c("Distance", "Station", "Habitat")) %>%
  left_join(habitat, "Station") %>%
  filter(Habitat.x == Habitat.y) %>%
  rename(Habitat = Habitat.x)

# how do I sample from the posterior to generate a distribution from each distance point not just estimate and upper/lower?
# i.e. as in mcelreath link function where each column is a distance value and each row is a sample of mu
# can we recreate plot similar to figure 4.10 in mcelreath? - parameter CI vs simulated outcome (predicted interval) CI - or same in this case since there is no sigma param in dbinom
ggplot(data = prediction_habitat, aes(x = Distance, y = estimate, colour = Habitat)) +
  # geom_point(data = data, aes(y = Detects/Pings)) +
  geom_line() +
  geom_line(aes(y = lower), linetype = "dotted") +
  geom_line(aes(y = upper), linetype = "dotted") +
  facet_wrap(~Station) +
  theme(legend.position = "none") +
  scale_y_continuous(
    trans = scales::logit_trans(),
  ) +
  NULL

ggplot(data = prediction, aes(x = Distance + 100, y = estimate)) +
  geom_point(data = data, aes(y = Detects/Pings)) +
  geom_line() +
  geom_line(aes(y = lower), linetype = "dotted") +
  geom_line(aes(y = upper), linetype = "dotted") +
  facet_wrap(~Station) +
  NULL

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

prediction_midpoint <- predict(analysis, new_data = character(0), 
                               term = "midpoint")



