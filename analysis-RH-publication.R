# Analysis code

#####
# ANALYSIS EXPERIMENT 1
options(digits= 4)
options(scipen=999)
set.seed(12345)


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
library(ggpubr)
library(lsmeans)

# Datasets exp 1:
Data.Exp1.ttest <- read_excel("data/Data.Exp1.ttest.xlsx") 
Data.Exp1.brm <- read_excel("data/Data.Exp1.brm.xlsx") 
# Datasets exp 2: 
Data.Exp2.ttest <- read_excel("data/Data.Exp2.ttest.xlsx") 
Data.Exp2.brm <- read_excel("data/Data.Exp2.brm.xlsx")
Data.Exp2.Liking <- read_excel("data/Data.Exp2.Liking.xlsx") 

# Analysis Experiment 1 - Exp1: Critical pairs only - t-test
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

# Analysis 2 - Exp1: General model - brms
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
#effect size
tidy_stan(General.brm.Exp1) 
r2_bayes(General.brm.Exp1) #R2c= 0.1; R2m= 0.047

# plotting
data_plot_exp1 <- tibble(
    ChoiceCondition = as.factor(c("critical-learned","critical-novel","noncritical-learned", "noncritical-novel")),
    BRM_estimate = as.numeric(c("0.06","-0.67","-0.33", "-0.32")),
    lower_ci = as.numeric(c("-0.49 ","-1.21 ","-0.85 ", "-0.84")),
    higher_ci = as.numeric(c("0.60","-.14","0.20", ".21 ")))

plot_brm_exp1 <- ggplot(data_plot_exp1, aes(x= ChoiceCondition, y= BRM_estimate,  fill = ChoiceCondition)) +
    geom_bar(stat="identity",color="black",position=position_dodge()) +
    geom_errorbar(aes(ymin= lower_ci, ymax= higher_ci), width= .2,
                  position=position_dodge(.9)) +
    scale_fill_manual(values=c("#56B4E9", "#E69F00","#56B4E9", "#E69F00")) +
    ylab("Model estimates") +
    xlab("Choice condition") +
    ylim(-1.22,1) +
    theme_bw()

plot_brm_exp1 <- plot_brm_exp1 + theme(legend.position = "none",
                                       axis.text=element_text(size=12),
                                       axis.title=element_text(size=12), plot.title = element_text(color = "black", size = 12,hjust = 0.5)) +
    ggtitle("Experiment 1")
<<<<<<< HEAD
plot_brm_exp1
#########
#########
=======

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


#####
# ANALYSIS EXPERIMENT 2
>>>>>>> d5caeba07866d30369312a6606fc9ffd050b0a7a
# Analysis 1 - Exp2: Critical pairs only - t-test
Data.Exp2.ttest$Participant= as.factor(Data.Exp2.ttest$id)
Data.Exp2.ttest$Recognition <- as.factor(Data.Exp2.ttest$Recognition)
Data.Exp2.ttest$Mean <- as.numeric(Data.Exp2.ttest$Mean)

detach(package:plyr) #if you have problems because loading summarySE 
Data.Exp2.ttest %>%
    group_by(Recognition) %>%
    summarize(total= sum(as.numeric(Mean),na.rm=T),
              count= n(),
              mean= mean(as.numeric(Mean),na.rm=T),
              sd= sd(as.numeric(Mean),na.rm=T))

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

# Analysis 2 -  Exp2: General model: brms
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

plots_brm_exp1and2 <- ggarrange(plot_brm_exp1,plot_brm_exp2, ncol=1, nrow=2)
ggsave("plots_brm_exp1and2.pdf", width=20, height=15, units = c("cm"),
       dpi=300, device = "pdf")

# Analysis 3 - Exp2: Adittional music infomration
names.exp2 <- c("Recognition","id","Song","Brand","Position","Task","Clip",
                "Combination","BrandCategory","Trial","IV")
Data.Exp2.Liking[,names.exp2] <- lapply(Data.Exp2.Liking[,names.exp2] , factor)
Data.Exp2.Liking

Data.Exp2.Liking <- Data.Exp2.Liking %>%
    mutate(Preference = ifelse(Response %in% 1:3, "Dislike", "Like"))
Data.Exp2.Liking$Preference <- factor(Data.Exp2.Liking$Preference, levels = c("Dislike", "Like"))
Data.Exp2.Liking_CR <- Data.Exp2.Liking %>% filter(Clip == "CR")
Data.Exp2.Liking_CR$Choice <- as.numeric(as.character(Data.Exp2.Liking_CR$Choice))

ANOVA.Exp2.Liking <- lm(Choice ~ Preference*Recognition, data = Data.Exp2.Liking_CR)
Anova(ANOVA.Exp2.Liking, Type="III")
summary.lm(ANOVA.Exp2.Liking)
anova_stats(ANOVA.Exp2.Liking)

detach(package:plyr) #if I have problems because loadfed summarySE first
Data.Exp2.Liking_CR %>%
    group_by(Preference) %>%
    summarize(total= sum(as.numeric(Choice),na.rm=T),
              mean= mean(as.numeric(Choice),na.rm=T),
              sd= sd(as.numeric(Choice),na.rm=T),
              count=n())

Data.Exp2.Liking_CR %>%
    group_by(Preference, Recognition) %>%
    summarize(total= sum(as.numeric(Choice),na.rm=T),
              mean= mean(as.numeric(Choice),na.rm=T),
              sd= sd(as.numeric(Choice),na.rm=T),
              count=n())

#Figure 3
## import summarySE
Figure3_MusicInformation_Data<- summarySE(Data.Exp2.Liking_CR,measurevar="Choice",groupvars=c("Preference","Recognition"))

plot.Exp2.Figure3 <- ggplot(data=Figure4_MusicInformation_Data, aes(x=factor(Preference), y=Choice, fill= Recognition)) +
    geom_bar(stat="identity",color="black",position=position_dodge())+
    scale_fill_manual(values=c("#56B4E9","#E69F00")) +
    geom_errorbar(aes(ymin=Choice-ci, ymax=Choice+ci), width=.2,
                  position=position_dodge(.9)) +
    ylim(0,1) +
    labs(x="", y = "Mean choice proportion")+
    theme_bw()

plot.Exp2.Figure3 <- plot.Exp2.Figure3 + 
    theme(axis.text=element_text(size=12),legend.text=element_text(size=12))
   
ggsave("plot.Exp2.Figure3.pdf", width=20, height=15, units = c("cm"),
       dpi=300, device = "pdf")

# Analysis 4 - Exp2: individual participant level
Data.Exp2.Liking_CR_like <- Data.Exp2.Liking_CR %>% filter(Preference == "Like", Recognition == "Learned")
Data.Exp2.Liking_CR_dislike <- Data.Exp2.Liking_CR %>% filter(Preference == "Dislike",Recognition == "Learned")

###Like
Data.Exp2.Liking_CR_like_plot <- Data.Exp2.Liking_CR_like %>% 
    group_by(id) %>%
    summarize(meanChoice= mean(Choice, na.rm= TRUE))
Data.Exp2.Liking_CR_like_plot_yesno <- Data.Exp2.Liking_CR_like_plot %>%
    mutate(  RH = ifelse(meanChoice <= 0.5, "No", "Yes"))
summary(factor(Data.Exp2.Liking_CR_like_plot_yesno$RH))

Data.Exp2.Liking_CR_like_plot_yesno$meanChoice <- ifelse(Data.Exp2.Liking_CR_like_plot_yesno$meanChoice==0, 0.01,
                                                         Data.Exp2.Liking_CR_like_plot_yesno$meanChoice)

Figure4.plot.MusicInformation_Liking_Like_yesno <- ggplot(Data.Exp2.Liking_CR_like_plot_yesno, aes(reorder(id, -meanChoice),y= meanChoice, fill=RH)) +
    geom_bar(stat="identity", width=0.2) +
    scale_fill_manual(values=c("#56B4E9","#E69F00")) +
    theme_bw() +
    ylim(0,1) +
    ylab("Proportion recognised chosen") + 
    xlab("Participants")

Figure4.plot.MusicInformation_Liking_Like_yesno + 
    theme(axis.text=element_text(size=14),legend.text=element_text(size=14),
            axis.line = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())

ggsave("Figure4.plot.MusicInformation_Liking_yesno.pdf", width=28, height=10, units = c("cm"),
       dpi=300, device = "pdf")

summary(factor(Data.Exp2.Liking_CR_like_plot_yesno$RH)) #No= 38/ Yes= 89
binom.test(38,127) # 84%

###Dislike
Data.Exp2.Liking_CR_dislike_plot <- Data.Exp2.Liking_CR_dislike %>% 
    group_by(id) %>%
    summarize(meanChoice= mean(Choice, na.rm= TRUE))
Data.Exp2.Liking_CR_dislike_plot_yesno <- Data.Exp2.Liking_CR_dislike_plot %>%
    mutate(  RH = ifelse(meanChoice <= 0.5, "No", "Yes"))
summary(factor(Data.Exp2.Liking_CR_dislike_plot_yesno$RH))

Data.Exp2.Liking_CR_dislike_plot_yesno$meanChoice <- ifelse(Data.Exp2.Liking_CR_dislike_plot_yesno$meanChoice==0, 0.01,
                                                         Data.Exp2.Liking_CR_dislike_plot_yesno$meanChoice)

Figure4.plot.MusicInformation_disLike_yesno <- ggplot(Data.Exp2.Liking_CR_dislike_plot_yesno, aes(reorder(id, -meanChoice),y= meanChoice, fill=RH)) +
    geom_bar(stat="identity", width=0.2) +
    scale_fill_manual(values=c("#56B4E9","#E69F00")) +
    theme_bw() +
    ylim(0,1) +
    ylab("Proportion recognised chosen") + 
    xlab("Participants")

Figure4.plot.MusicInformation_disLike_yesno + 
    theme(axis.text=element_text(size=14),legend.text=element_text(size=14),
          axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())

ggsave("Figure4.plot.MusicInformation_dislike_yesno.pdf", width=28, height=10, units = c("cm"),
       dpi=300, device = "pdf")

summary(factor(Data.Exp2.Liking_CR_dislike_plot_yesno$RH)) #No= 63/ Yes= 17
binom.test(63,80) # 79
