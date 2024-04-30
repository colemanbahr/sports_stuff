library (tidyverse)
library(ggthemes)
library(ranger)
library(vip)
library(caret)
library(xgboost)
library(randomForest)
library(DiagrammeR)
options(scipen = 9999)
library(dplyr)
library(stringr)

test_rpi <- read.csv("/Users/colemanbahr/Downloads/ncaa2.csv") # Read Data
data_24 <- read.csv("/Users/colemanbahr/Downloads/ncaa3.csv") # Read Data

sample_data <- test_rpi[sample(nrow(test_rpi), 0.7 * nrow(test_rpi)), ]
tourney_rf <- randomForest::randomForest(as.factor(Made.Tournament) ~ Adj.RPI+Org.RPI+Record+Non.Conf.RPI+Conference.RPI+Conference.Record+X1.25.Wins+X1.25.Win..+X26.50.Win..+X51.100.Win.., data = data_24)
vip(tourney_rf)
tourn_preds <- predict(tourney_rf, type = "prob")
tourn_preds_join <- cbind(data_24, tourn_preds)

write.csv(tourn_preds_join, "tourney.csv")

