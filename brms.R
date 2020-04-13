# brm analysis

library(tidyverse)
library(readxl)
library(car)
library(lme4)
library(lmerTest)
library(multcomp)

#solving the issue
Data.Exp1.glmer <- read_excel("data/Data.Exp1.glmer.xlsx") #or load manually
Data.Exp2.glmer <- read_excel("data/Data.Exp2.glmer.xlsx") #or load manually

names.exp1 <- c("Recognition","id","Song","Brand","Position","Task","Clip","BrandCategory")
Data.Exp1.glmer[,names.exp1] <- lapply(Data.Exp1.glmer[,names.exp1] , factor)
Data.Exp1.glmer$Choice <- as.numeric(Data.Exp1.glmer$Choice )
Data.Exp1.glmer

Data.Exp1.glmer$IV <- paste(Data.Exp1.glmer$Clip,Data.Exp1.glmer$Recognition,sep="-")
table(Data.Exp1.glmer$IV,useNA="ifany")
Data.Exp1.glmer$IV <- as.factor(Data.Exp1.glmer$IV)
Data.Exp1.glmer$IV <- relevel(Data.Exp1.glmer$IV, ref= "NCL-Learned")

names.exp2 <- c("Recognition","id","Song","Brand","Position","Task","Clip","BrandCategory")
Data.Exp2.glmer[,names.exp2] <- lapply(Data.Exp2.glmer[,names.exp2] , factor)
Data.Exp2.glmer$Choice <- as.numeric(Data.Exp2.glmer$Choice )
Data.Exp2.glmer

Data.Exp2.glmer$IV <- paste(Data.Exp2.glmer$Clip,Data.Exp2.glmer$Recognition,sep="-")
table(Data.Exp2.glmer$IV,useNA="ifany")
Data.Exp2.glmer$IV <- as.factor(Data.Exp2.glmer$IV)
Data.Exp2.glmer$IV <- relevel(Data.Exp2.glmer$IV, ref= "NCL-Learned")

Data.Exp2.glmer %>%
    group_by(IV,Position) %>%
    summarize(total= sum(as.numeric(Choice),na.rm=T),
              count= n(),
              mean= mean(as.numeric(Choice),na.rm=T),
              sd= sd(as.numeric(Choice),na.rm=T))

## brm
set.seed(1234)
library(brms)
library(sjstats)
library(sjmisc)
library("loo")
library(performance)
## exp1
General.brm.Exp1<- brm(Choice ~ 0+IV+Position + (1 | id)+(1|Brand) + (1|Song),
                       data=Data.Exp1.glmer, cores= 4,
                       iter= 8000, control= list(max_treedepth = 10, adapt_delta=0.99),
                       family = bernoulli()) #there is sth wrong!
summary(General.brm.Exp1)
plot(General.brm.Exp1, pars = c("IV", "Position")) 
plot(conditional_effects(General.brm.Exp1, effects = "IV:Position"))
plot(marginal_effects(General.brm.Exp1), points = TRUE, rug = TRUE)

#effect size?
tidy_stan(General.brm.Exp1) 
r2(General.brm.Exp1)  #R2c= 0.1; R2m= 0.048
r2_bayes(General.brm.Exp1) #R2c= 0.1; R2m= 0.048

# plotting
data_plot_exp1 <- tibble(
    ChoiceCondition = as.factor(c("CRL","CRN","NCL", "NCN")),
    BRM_estimate = as.numeric(c("0.06","-0.67","-0.34", "-0.32")),
    lower_ci = as.numeric(c("-0.47 ","-1.19 ","-0.87 ", "-0.84")),
    higher_ci = as.numeric(c("0.61","0.19","0.19", "-0.14 ")))

ggplot(data_plot_exp1, aes(x= ChoiceCondition, y= BRM_estimate)) +
    geom_bar(stat="identity",color="black",position=position_dodge()) +
    geom_errorbar(aes(ymin= lower_ci, ymax= higher_ci), width= .2,
                  position=position_dodge(.9)) +
    ggtitle("BRM results exp1") +
    theme_bw()


## exp 2
General.brm.Exp2<- brm(Choice ~ 0+IV+Position + (1 | id)+(1|Brand) + (1|Song),
                       data=Data.Exp2.glmer, cores= 4,
                       iter= 8000, control= list(max_treedepth = 10, adapt_delta=0.99),
                       family = bernoulli()) #there is sth wrong!
summary(General.brm.Exp2)
plot(General.brm.Exp2, pars = c("IV", "Position")) 
plot(conditional_effects(General.brm.Exp2, effects = "IV:Position"))
plot(marginal_effects(General.brm.Exp2), points = TRUE, rug = TRUE)

#effect size?
equi_test(General.brm.Exp2)
equi_test(General.brm.Exp2, out = "plot")

tidy_stan(General.brm.Exp2) 
r2(General.brm.Exp2)  #R2c= 0.1; R2m= 0.016

r2_bayes(General.brm.Exp2) #R2c= 0.1; R2m= 0.016

# plotting
data_plot_exp2 <- tibble(
    ChoiceCondition = as.factor(c("CRL","CRN","NCL", "NCN")),
    BRM_estimate = as.numeric(c("0.41","-0.37","0", "0")),
    lower_ci = as.numeric(c("-0.14 ","-0.93 ","-0.38 ", "-0.39")),
    higher_ci = as.numeric(c("0.96 ","0.16","0.40", "0.39 ")))

ggplot(data_plot_exp2, aes(x= ChoiceCondition, y= BRM_estimate)) +
    geom_bar(stat="identity",color="black",position=position_dodge()) +
    geom_errorbar(aes(ymin= lower_ci, ymax= higher_ci), width= .2,
                  position=position_dodge(.9)) +
    ggtitle("BRM results exp2") +
    theme_bw()


