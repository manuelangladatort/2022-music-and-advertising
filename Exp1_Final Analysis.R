#####
# ANALYSIS EXPERIMENT 1
options(digits= 3)
options(scipen=999)
set.seed(12345)

# Set working directory
# Install/ load packages
library(readxl)
library(car)
library(lme4)
library(lmerTest)
library(tidyverse)
library(multcomp)
library(brms)
library(sjstats)
library(sjmisc)
library(loo)
library(performance)

# Datasets: Data.Exp1.ttest + Data.Exp1.glmer
Data.Exp1.ttest <- read_excel("data/Data.Exp1.ttest.xlsx") #or load manually
Data.Exp1.brm <- read_excel("data/Data.Exp1.brm.xlsx") #or load manually

# Analysis 1: Critical pairs only - t-test
Data.Exp1.ttest$Participant= as.factor(Data.Exp1.ttest$id)
Data.Exp1.ttest$Recognition <- as.factor(Data.Exp1.ttest$Recognition)
Data.Exp1.ttest$Mean <- as.numeric(Data.Exp1.ttest$Mean)

Data.Exp1.ttest %>%
    group_by(Recognition) %>%
    summarize(total= sum(as.numeric(Mean),na.rm=T),
              count= n(),
              mean= mean(as.numeric(Mean),na.rm=T),
              sd= sd(as.numeric(Mean),na.rm=T))
# Absolute Percentages: 59% and 41%
# Absolute difference: 9%
# the % relative increase is 18% (59/ 50= 1.18)
# odds ratio 1.44 ((59/41)/(50/50))

t.test.Exp1 <- t.test(Mean ~ Recognition, data = Data.Exp1.ttest, paired = TRUE)
t.test.Exp1

t.Exp1<- t.test.Exp1$statistic[[1]]
t.Exp1
df.Exp1<- t.test.Exp1$parameter[[1]]
df.Exp1
r.Exp1 <- sqrt(t.Exp1^2/(t.Exp1^2+df.Exp1))
round(r.Exp1, 3)

# Analysis 2: General model - brms
names.exp1 <- c("Recognition","id","Song","Brand","Position","Task","Clip",
                "Combination","BrandCategory","Trial","IV")
Data.Exp1.brm[,names.exp1] <- lapply(Data.Exp1.brm[,names.exp1] , factor)
table(Data.Exp1.brm$IV,useNA="ifany")
Data.Exp1.brm$IV <- relevel(Data.Exp1.brm$IV, ref= "NCL-Learned")
Data.Exp1.brm

General.brm.Exp1<- brm(Choice ~ 0+IV+Position + (1 |id) + (1|Brand) + (1|Song),
                       data=Data.Exp1.brm, cores= 4,
                       iter= 8000, control= list(max_treedepth = 10, adapt_delta=0.99),
                       family = bernoulli()) 
summary(General.brm.Exp1)
plot(General.brm.Exp1, pars = c("IV", "Position")) 
plot(marginal_effects(General.brm.Exp1), points = TRUE, rug = TRUE)
plot(conditional_effects(General.brm.Exp1, effects = "IV:Position"))
#effect size?
tidy_stan(General.brm.Exp1) 
r2_bayes(General.brm.Exp1) #R2c= 0.1; R2m= 0.047

# plotting
data_plot_exp1 <- tibble(
    ChoiceCondition = as.factor(c("CRL","CRN","NCL", "NCN")),
    BRM_estimate = as.numeric(c("0.06","-0.67","-0.33", "-0.32")),
    lower_ci = as.numeric(c("-0.49 ","-1.21 ","-0.85 ", "-0.84")),
    higher_ci = as.numeric(c("0.60","-.14","0.20", ".21 ")))

ggplot(data_plot_exp1, aes(x= ChoiceCondition, y= BRM_estimate)) +
    geom_bar(stat="identity",color="black",position=position_dodge()) +
    geom_errorbar(aes(ymin= lower_ci, ymax= higher_ci), width= .2,
                  position=position_dodge(.9)) +
    ggtitle("BRM results exp1") +
    theme_bw()

#########
#Figures - Figure 1
source("summarySE.R")
#Figure 1
Exp1.Figure1.Songs.Data<- summarySE(Data.Exp1.glmer, measurevar="Choice",groupvars=c("Song","Position"))
Exp1.Figure1.Songs.Data <- na.omit(Exp1.Figure1.Songs.Data)

plot.Exp1.Figure1.Songs<- ggplot(data=Exp1.Figure1.Songs.Data, aes(x=reorder(Song, Choice), y=Choice, fill= Position)) +
    geom_bar(stat="identity", color="black", position=position_dodge())+
    geom_errorbar(aes(ymin=Choice-ci, ymax=Choice+ci), width=.2,
                  position=position_dodge(.9)) +
    scale_fill_manual(values=c("#E69F00", "#56B4E9")) +
    ylim(0,1) +
    theme_bw()

plot.Exp1.Figure1.Songs + theme(axis.text=element_text(size=14), axis.title=element_text(size=12),
                                axis.title.x = element_blank()) + labs(fill = "Presentation\nPosition") 

ggsave("plot.Exp1.Figure1.Songs.pdf", width=25, height=18, units = c("cm"),
       dpi=300, device = "pdf")

#########
#########