library(tidyverse)
library(readr)

set.seed(12345)

data_exp1_brm <- read_csv("data/data.exp1.new.brm.csv")
data_exp2_brm <- read_csv("data/data.exp2.new.brm.csv")

data_exp1_brm %>%
    group_by(Clip,LearnedPos1,LearnedPos2) %>%
    summarize(total= sum(as.numeric(ChosenPosition),na.rm=T),
              count= n(),
              mean= mean(as.numeric(ChosenPosition),na.rm=T),
              sd= sd(as.numeric(ChosenPosition),na.rm=T))

data_exp2_brm %>%
    group_by(Clip,LearnedPos1,LearnedPos2) %>%
    summarize(total= sum(as.numeric(ChosenPosition),na.rm=T),
              count= n(),
              mean= mean(as.numeric(ChosenPosition),na.rm=T),
              sd= sd(as.numeric(ChosenPosition),na.rm=T))


#exp1
data_exp1_new$ChosenPosition <- as.factor(data_exp1_new$ChosenPosition)
data_exp1_new$LearnedPos1 <- as.factor(data_exp1_new$LearnedPos1)
data_exp1_new$LearnedPos2 <- as.factor(data_exp1_new$LearnedPos2)
data_exp1_new$id <- as.factor(data_exp1_new$id)
data_exp1_new$BrandPos1 <- as.factor(data_exp1_new$BrandPos1)
data_exp1_new$BrandPos2 <- as.factor(data_exp1_new$BrandPos2)
data_exp1_new$SongPos1 <- as.factor(data_exp1_new$SongPos1)
data_exp1_new$SongPos2 <- as.factor(data_exp1_new$SongPos2)
library(brms)
BRM.Exp1c_RF_b <-  brm(ChosenPosition ~ 0+LearnedPos1 : LearnedPos2 + (1 | id)+(1|BrandPos1) + (1|BrandPos2) +
                           (1|SongPos1) + (1|SongPos2) , data=data_exp1_new, cores= 4,
                       iter= 8000, control= list(max_treedepth = 10, adapt_delta=0.99),family = bernoulli()) #09:34

summary(BRM.Exp1c_RF_b)
plot(BRM.Exp1c_RF_b, pars = c("LearnedPos1", "LearnedPos2")) 
plot(conditional_effects(BRM.Exp1c_RF_b, effects = "LearnedPos1:LearnedPos2"))

#exp2
data_exp2_new$ChosenPosition <- as.factor(data_exp2_new$ChosenPosition)
data_exp2_new$LearnedPos1 <- as.factor(data_exp2_new$LearnedPos1)
data_exp2_new$LearnedPos2 <- as.factor(data_exp2_new$LearnedPos2)
data_exp2_new$id <- as.factor(data_exp2_new$id)
data_exp2_new$BrandPos1 <- as.factor(data_exp2_new$BrandPos1)
data_exp2_new$BrandPos2 <- as.factor(data_exp2_new$BrandPos2)
data_exp2_new$SongPos1 <- as.factor(data_exp2_new$SongPos1)
data_exp2_new$SongPos2 <- as.factor(data_exp2_new$SongPos2)
library(brms)
BRM.Exp2c_RF_b <-  brm(ChosenPosition ~ 0+LearnedPos1 : LearnedPos2 + (1 | id)+(1|BrandPos1) + (1|BrandPos2) +
                           (1|SongPos1) + (1|SongPos2) , data=data_exp2_new, cores= 4,
                       iter= 8000, control= list(max_treedepth = 10, adapt_delta=0.99),family = bernoulli())

summary(BRM.Exp2c_RF_b)
plot(BRM.Exp2c_RF_b, pars = c("LearnedPos1", "LearnedPos2")) 
plot(conditional_effects(BRM.Exp2c_RF_b, effects = "LearnedPos1:LearnedPos2"))
