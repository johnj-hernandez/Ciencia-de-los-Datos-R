#data<-readRDS()

load("Bikes.Rdata")

vars <- c("hr","holiday","workingday","weathersit","temp","atemp","hum","windspeed")

mean(bikesJuly$cnt)
var(bikesJuly$cnt)

fmla <- cnt ~ hr + holiday + workingday+weathersit + temp + atemp + hum + windspeed

model <- glm(fmla, data = bikesJuly,family = quasipoisson)

install.packages("broom")
library("broom")
perf<-glance(model)    
library("dplyr")
#Que tanto es mejor mi modelo a un modelo basico como el de la media
perf%>%summarize(pseudoR2 = 1 - deviance/null.deviance)

#ahora predecimos para el mes de agosto con base al mdoelo que encontramos
bikesAugust$pred<- predict(model, newdata = bikesAugust,type = "response")
sum(bikesAugust$pred >0 )#todos sobre 0 

library(tidyr)
library(ggplot2)
xg_plot<-bikesAugust %>%
  # set start to 0, convert unit to days
  mutate(instant = (instant - min(instant))/24) %>%
  gather(key = valuetype, value = value, cnt, pred) %>%
  filter(instant < 14) %>% # first two weeks
  ggplot(aes(x = instant, y = value, color = valuetype,
             linetype = valuetype)) +
  geom_point() +
  geom_line() +
  scale_x_continuous("Day", breaks = 0:14, labels = 0:14) +
  scale_color_brewer(palette = "Dark2") +
  ggtitle("Predicted August bike rentals, QuasiPoisson")

xg_plot

set.seed(14)
install.packages("ranger")
library(ranger)

fmla<- cnt ~ hr + holiday + workingday +
  + weathersit + temp + atemp + hum + windspeed

model_rf2 <- ranger(fmla, bikesAugust, num.trees = 500,
                    respect.unordered.factors = "order")
model_rf2$r.squared


#ahora predecimos para el mes de agosto con base al mdoelo que encontramos
predictionaugust<- predict(object = model_rf2,data = bikesAugust)
predictionaugust$predictions
bikesAugust$cnt

sum(bikesAugust$pred >0 )#todos sobre 0 

install.packages("Metrics")
library(Metrics)
Metrics::rmse(bikesAugust$cnt,predictionaugust$predictions)

library(tidyr)
library(ggplot2)
xg_plot<-bikesAugust %>%
  # set start to 0, convert unit to days
  mutate(instant = (instant - min(instant))/24) %>%
  gather(key = valuetype, value = value, cnt, pred) %>%
  filter(instant < 14) %>% # first two weeks
  ggplot(aes(x = instant, y = value, color = valuetype,
             linetype = valuetype)) +
  geom_point() +
  geom_line() +
  scale_x_continuous("Day", breaks = 0:14, labels = 0:14) +
  scale_color_brewer(palette = "Dark2") +
  ggtitle("Predicted August bike rentals, QuasiPoisson")

xg_plot
