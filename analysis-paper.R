################################################################################
# File:     analysis-paper.R
# Purpose:  Analysis script to reproduce results reported in paper
#
# Author:   Manuel Anglada-Tort
################################################################################
options(digits= 4)
options(scipen=999)
set.seed(12345)

# load
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
library(ggpubr)
library(lsmeans)
source("summarySE.R")

# load data
## Exp 1:
Data.Exp1.ttest <- read_excel("data/Data.Exp1.ttest.xlsx") 
## Exp 2: 
Data.Exp2.ttest <- read_excel("data/Data.Exp2.ttest.xlsx") 
Data.Exp2.brm <- read_excel("data/Data.Exp2.brm.xlsx")
Data.Exp2.Liking <- read_excel("data/Data.Exp2.Liking.xlsx") 


# Analysis 1 (Experiment 1): Critical pairs only - t-test
Data.Exp1.ttest$Participant<- as.factor(Data.Exp1.ttest$id)
Data.Exp1.ttest$Recognition <- as.factor(Data.Exp1.ttest$Recognition)
Data.Exp1.ttest$Mean <- as.numeric(Data.Exp1.ttest$Mean)

Data.Exp1.ttest %>%
    group_by(Recognition) %>%
    summarize(
              mean = mean(as.numeric(Mean),na.rm=T),
              sd= sd(as.numeric(Mean),na.rm=T),
              count= n()
              )
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


# Analysis 2 (Experiment 2): Critical pairs only - t-test
Data.Exp2.ttest$Participant= as.factor(Data.Exp2.ttest$id)
Data.Exp2.ttest$Recognition <- as.factor(Data.Exp2.ttest$Recognition)
Data.Exp2.ttest$Mean <- as.numeric(Data.Exp2.ttest$Mean)

# detach(package:plyr) #if you have problems because loading summarySE 
Data.Exp2.ttest %>%
    group_by(Recognition) %>%
    summarize(
              mean= mean(as.numeric(Mean),na.rm=T),
              sd= sd(as.numeric(Mean),na.rm=T),
              count= n()
              )

# Absolute Percentages: 56% and 44%
# Absolute difference: 6%
# the % relative increase is 12% (56/ 50= 1.12)
# odds ratio 1.27 ((56/44)/(50/50))

t.test.Exp2 <- t.test(Mean ~ Recognition, data = Data.Exp2.ttest, paired = TRUE)
t.test.Exp2

t.Exp2<- t.test.Exp2$statistic[[1]]
t.Exp2
df.Exp2<- t.test.Exp2$parameter[[1]]
df.Exp2
r.Exp2 <- sqrt(t.Exp2^2/(t.Exp2^2+df.Exp2))
round(r.Exp2, 3)

# Analysis 3 (Experiment 2): General model: brms
names.exp2 <- c("Recognition","id","Song","Brand","Position","Task","Clip",
                "Combination","BrandCategory","Trial","IV")
Data.Exp2.brm[,names.exp2] <- lapply(Data.Exp2.brm[,names.exp2] , factor)
table(Data.Exp2.brm$IV,useNA="ifany")
Data.Exp2.brm$IV <- relevel(Data.Exp2.brm$IV, ref= "NCL-Learned")
Data.Exp2.brm

General.brm.Exp2<- brm(Choice ~ 0+IV+Position + (1 |id) + (1|Brand) + (1|Song),
                       data=Data.Exp2.brm, cores= 4,
                       iter= 8000, control= list(max_treedepth = 10, adapt_delta=0.99),
                       family = bernoulli()) 
summary(General.brm.Exp2)
# visualize
plot(General.brm.Exp2, pars = c("IV", "Position")) 
plot(marginal_effects(General.brm.Exp2), points = TRUE, rug = TRUE)
plot(conditional_effects(General.brm.Exp2, effects = "IV:Position"))

#effect size
tidy_stan(General.brm.Exp2) 
r2_bayes(General.brm.Exp2) #R2c= 0.1; R2m= 0.047

# plotting
data_plot_exp2 <- tibble(
    ChoiceCondition = as.factor(c("critical-learned","critical-novel","noncritical-learned", "noncritical-novel", "particiipant", "music", "brand")),
    BRM_estimate = as.numeric(c(".42","-.36","0", "0", ".03", ".71", ".11")),
    lower_ci = as.numeric(c("-0.12 ","-.9","-.39", "-.40", "0", ".51", ".01")),
    higher_ci = as.numeric(c(".96",".18",".40", ".40", ".09", ".99", ".24")))

data_plot_exp2$ChoiceCondition = factor(data_plot_exp2$ChoiceCondition, levels=c("critical-learned","critical-novel","noncritical-learned", "noncritical-novel", "particiipant", "music", "brand")) 

plot_brm_exp2 <- ggplot(data_plot_exp2, aes(x= ChoiceCondition, y= BRM_estimate,  fill = ChoiceCondition)) +
    geom_bar(stat="identity",color="black",position=position_dodge()) +
    geom_errorbar(aes(ymin= lower_ci, ymax= higher_ci), width= .2,
                  position=position_dodge(.9)) +
    scale_fill_manual(values=c("#56B4E9", "#E69F00","#56B4E9", "#E69F00", "grey","grey","grey")) +
    ylab("Model estimates") +
    xlab("Choice condition") +
    ylim(-1.22,1) +
    theme_bw()

plot_brm_exp2 <- plot_brm_exp2 + theme(legend.position = "none",
                      axis.text=element_text(size=12),
                      axis.title=element_text(size=12), plot.title = element_text(color = "black", size = 12,hjust = 0.5)) 

#  to save
ggsave("Figure2_brms_output.png", plot_brm_exp2,
       width=27, height=15, units = c("cm"),
       dpi=300, device = "png")


# Analysis 4 (Experiment 2): Music liking
data_sum_liking = Data.Exp2.Liking %>%
    filter(Clip == "CR") %>%
    group_by(Recognition, Response) %>%
    summarise(
        n=n(),
        mean_choice = mean(Choice, na.rm=T),
        sd_choice = sd(Choice, na.rm=T),
        se_choice = sd_choice/sqrt(n),
    )

plot_liking = ggplot(data_sum_liking, 
                     aes(x=as.factor(Response), 
                         y=mean_choice, 
                         group=Recognition, 
                         color=Recognition)
                     ) + 
    geom_errorbar(aes(ymin=mean_choice-se_choice, ymax=mean_choice+se_choice), 
                  width=.1,
                  size = 0.7) +
    geom_line(aes(color=Recognition), size = 0.8) +
    ylab("Mean brand choice") +
    xlab("Liking Scale") +
    scale_color_manual(values=c("#56B4E9", "#E69F00")) +
    geom_point(aes(color=Recognition)) +
    theme_bw() + 
    theme(
        axis.text=element_text(size=12),
        axis.title=element_text(size=12), 
        legend.text=element_text(size=12)
        ) 

# to save
ggsave("Figure3_like.png", plot_liking,
       width=15, height=15, units = c("cm"),
       dpi=300, device = "png")


# linear model
Data.Exp2.Liking_CR <- Data.Exp2.Liking %>% filter(Clip == "CR")

Data.Exp2.Liking_CR$Recognition <- relevel(Data.Exp2.Liking_CR$Recognition, ref= "Novel")
Data.Exp2.Liking_CR$Choice <- as.numeric(as.character(Data.Exp2.Liking_CR$Choice))
Data.Exp2.Liking_CR$Recognition <- as.factor(Data.Exp2.Liking_CR$Recognition)

lm.Exp2.Liking <- lm(Choice ~ Response*Recognition, data = Data.Exp2.Liking_CR)
Anova(lm.Exp2.Liking, Type="III")
anova_stats(lm.Exp2.Liking)
