#####
# ANALYSIS EXPERIMENT 2
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
# Datasets: Data.Exp1.ttest + Data.Exp1.glmer
Data.Exp2.ttest <- read_excel("data/Data.Exp2.ttest.xlsx") #or load manually
Data.Exp2.glmer <- read_excel("data/Data.Exp2.glmer.xlsx") #or load manually
Data.Exp2.Liking <- read_excel("data/Data.Exp2.Liking.xlsx") #or load manually
Data.Exp2.Correlations <- read_excel("data/Data.Exp2.Correlations.xlsx") #or load manually

# Analysis 1: Critical pairs only - t-test
Data.Exp2.ttest$Participant= as.factor(Data.Exp2.ttest$id)
Data.Exp2.ttest$Recognition <- as.factor(Data.Exp2.ttest$Recognition)
Data.Exp2.ttest$Mean <- as.numeric(Data.Exp2.ttest$Mean)
contrasts(Data.Exp2.ttest$Recognition)<-contr.sum

detach(package:plyr) #if I have problems because loadfed summarySE first
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

# Analysis 2: General model - GLMER
Data.Exp2.glmer$Choice <- as.numeric(Data.Exp2.glmer$Choice )
Data.Exp2.glmer$Recognition <- as.factor(Data.Exp2.glmer$Recognition)
Data.Exp2.glmer$id <- as.factor(Data.Exp2.glmer$id)
Data.Exp2.glmer$Song <- as.factor(Data.Exp2.glmer$Song)
Data.Exp2.glmer$Brand <- as.factor(Data.Exp2.glmer$Brand)
Data.Exp2.glmer$Position <- as.factor(Data.Exp2.glmer$Position)
Data.Exp2.glmer$BrandCategory <- as.factor(Data.Exp2.glmer$BrandCategory)

Data.Exp2.glmer$IV <- paste(Data.Exp2.glmer$Clip,Data.Exp2.glmer$Recognition,sep="-")
table(Data.Exp2.glmer$IV,useNA="ifany")
Data.Exp2.glmer$IV <- as.factor(Data.Exp2.glmer$IV)
contrasts(Data.Exp2.glmer$IV)<-contr.sum

General.glmer.Exp2<- glmer(Choice ~ IV+ Position + Brand + Song + 
                               (1|id),data=Data.Exp2.glmer, family= binomial)
Anova(General.glmer.Exp2,Type="III")
summary(General.glmer.Exp2)

model2.E2.CR.predictions= round(predict(General.glmer.Exp2,type="response")) #calculate model's classification accuracy
acc.table.model2.E2.CR= table(na.omit(Data.Exp2.glmer$Choice), model2.E2.CR.predictions) #compare the predictions agaisnt the actual data
accuracy.model2.E2.CR= sum(diag(acc.table.model2.E2.CR))/sum(acc.table.model2.E2.CR) #compute the accuracy of this table
accuracy.model2.E2.CR #.647

#psothoc using HOLM
summary(glht(General.glmer.Exp2, linfct = mcp(IV = "Tukey")), test = adjusted("holm")) #holm correction
summary(glht(General.glmer.Exp2, linfct = mcp(Song = "Tukey")), test = adjusted("holm")) #holm correction 
summary(glht(General.glmer.Exp2, linfct = mcp(Brand = "Tukey")), test = adjusted("holm")) #holm correction
summary(glht(General.glmer.Exp2, linfct = mcp(Position = "Tukey")), test = adjusted("holm")) #holm correction

detach(package:plyr) #if I have problems because loadfed summarySE first
Data.Exp2.glmer %>%
    group_by(IV) %>%
    summarize(total= sum(as.numeric(Choice),na.rm=T),
              count= n(),
              mean= mean(as.numeric(Choice),na.rm=T),
              sd= sd(as.numeric(Choice),na.rm=T))

Data.Exp2.glmer %>%
    group_by(Position) %>%
    summarize(total= sum(as.numeric(Choice),na.rm=T),
              count= n(),
              mean= mean(as.numeric(Choice),na.rm=T),
              sd= sd(as.numeric(Choice),na.rm=T))
#######
#Figure 3
source("summarySE.R")

#Figure 3a position 1
DataExp2_LearnedSongs_P1 <- Data.Exp2.glmer %>% filter( Position == "First")
DataExp2_LearnedSongs_P2 <- Data.Exp2.glmer %>% filter( Position == "Second")

Exp2.Figure1.P1.Data<- summarySE(DataExp2_LearnedSongs_P1, measurevar="Choice",groupvars=c("IV"))
plot.Exp2.Figure3.Position1 <- ggplot(data=Exp2.Figure1.P1.Data, aes(x=IV, y=Choice, fill= IV)) +
    geom_bar(stat="identity",color="black",position=position_dodge())+
    scale_fill_manual(values=c("#56B4E9","#56B4E9", "#E69F00", "#E69F00")) +
    geom_errorbar(aes(ymin=Choice-ci, ymax=Choice+ci), width=.2,
                  position=position_dodge(.9)) +
    ylim(0,1) +
    labs(x="Condition", y = "Mean choice proportion (Position 1)")+
    theme_bw()

plot.Exp2.Figure3.Position1 + theme(axis.text=element_text(size=14), axis.title=element_blank(), legend.position="none")

ggsave("plot.Exp2.Figure3.Position1.pdf", width=16, height=19, units = c("cm"),
       dpi=300, device = "pdf")

#Figure 3b position 2
Exp2.Figure1.P2.Data<- summarySE(DataExp2_LearnedSongs_P2, measurevar="Choice",groupvars=c("IV"))
plot.Exp2.Figure3.Position2 <- ggplot(data=Exp2.Figure1.P2.Data, aes(x=IV, y=Choice, fill= IV)) +
    geom_bar(stat="identity",color="black",position=position_dodge())+
    scale_fill_manual(values=c("#56B4E9","#56B4E9", "#E69F00", "#E69F00")) +
    geom_errorbar(aes(ymin=Choice-ci, ymax=Choice+ci), width=.2,
                  position=position_dodge(.9)) +
    ylim(0,1) +
    labs(x="Condition", y = "Mean choice proportion (Position 2)")+
    theme_bw()

plot.Exp2.Figure3.Position2 + theme(axis.text=element_text(size=14), axis.title=element_blank(), legend.position="none")

ggsave("plot.Exp2.Figure3.Position2.pdf", width=16, height=19, units = c("cm"),
       dpi=300, device = "pdf")
#######

# Analysis 3: Adittional music infomration
Data.Exp2.Liking <- Data.Exp2.Liking %>%
    mutate(Preference = ifelse(Response %in% 1:2, "Low",
                               ifelse(Response %in% 2:3, "Medium", "High")))
summary(factor(Data.Exp2.Liking$Preference))
Data.Exp2.Liking$Preference <- factor(Data.Exp2.Liking$Preference, levels = c("Low", "Medium", "High"))

Data.Exp2.Liking_CR <- Data.Exp2.Liking %>% filter(Clip == "CR")
Data.Exp2.Liking_CR$Choice <- as.numeric(as.character(Data.Exp2.Liking_CR$Choice))

ANOVA.Exp2.Liking <- lm(Choice ~ as.numeric(Preference)*as.factor(Recognition), data = Data.Exp2.Liking_CR) #note Prefernece as NUMERIC
Anova(ANOVA.Exp2.Liking, Type="III")
summary.lm(ANOVA.Exp2.Liking)
library(sjstats)
anova_stats(ANOVA.Exp2.Liking)

#Repeat using HOLM
library(lsmeans)
post.ANOVA.Exp2.MusicInfo.Pref <- glht(ANOVA.Exp2.Liking, lsm(pairwise ~ Preference))
post.ANOVA.Exp2.MusicInfo.Pref.Holm = summary(post.ANOVA.Exp2.MusicInfo.Pref, test=adjusted("holm"))
post.ANOVA.Exp2.MusicInfo.Pref.Holm

post.ANOVA.Exp2.MusicInfo.Reco <- glht(ANOVA.Exp2.Liking, lsm(pairwise ~ Recognition))
post.ANOVA.Exp2.MusicInfo.Reco.Holm = summary(post.ANOVA.Exp2.MusicInfo.Reco, test=adjusted("holm"))
post.ANOVA.Exp2.MusicInfo.Reco.Holm

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

Data.Exp2.Liking_CR %>%
    group_by(Recognition) %>%
    summarize(total= sum(as.numeric(Choice),na.rm=T),
              mean= mean(as.numeric(Choice),na.rm=T),
              sd= sd(as.numeric(Choice),na.rm=T),
              count=n())

#Figure 4
Figure4_MusicInformation_Data<- summarySE(Data.Exp2.Liking_CR,measurevar="Choice",groupvars=c("Preference","Recognition"))

plot.Exp2.Figure4 <- ggplot(data=Figure4_MusicInformation_Data, aes(x=factor(Preference), y=Choice, fill= Recognition)) +
    geom_bar(stat="identity",color="black",position=position_dodge())+
    scale_fill_manual(values=c("#56B4E9","#E69F00")) +
    geom_errorbar(aes(ymin=Choice-ci, ymax=Choice+ci), width=.2,
                  position=position_dodge(.9)) +
    ylim(0,1) +
    labs(x="Condition", y = "Liking")+
    theme_bw()

plot.Exp2.Figure4 + 
    theme(axis.text=element_text(size=14),legend.text=element_text(size=14),
          axis.title=element_blank())

ggsave("plot.Exp2.Figure4.pdf", width=28, height=19, units = c("cm"),
       dpi=300, device = "pdf")

# non-compensatory: individual participant level
Data.Exp2.Liking_CR_low <- Data.Exp2.Liking_CR %>% filter(Preference == "Low", Recognition == "Learned")
Data.Exp2.Liking_CR_Med <- Data.Exp2.Liking_CR %>% filter(Preference == "Medium",Recognition == "Learned")
Data.Exp2.Liking_CR_High <- Data.Exp2.Liking_CR %>% filter(Preference == "High",Recognition == "Learned")

detach(package:plyr) #if I have problems because loadfed summarySE first
Data.Exp2.Liking_CR_low %>% group_by(Recognition) %>%
    summarize(mean = mean(as.numeric(Choice), na.rm= TRUE))
Data.Exp2.Liking_CR_Med %>% group_by(Recognition) %>%
    summarize(mean = mean(as.numeric(Choice), na.rm= TRUE))
Data.Exp2.Liking_CR_High %>% group_by(Recognition) %>%
    summarize(mean = mean(as.numeric(Choice), na.rm= TRUE))

###LOW
Data.Exp2.Liking_CR_low_plot <- Data.Exp2.Liking_CR_low %>% 
    group_by(id) %>%
    summarize(meanChoice= mean(Choice, na.rm= TRUE))
Data.Exp2.Liking_CR_low_plot_yesno <- Data.Exp2.Liking_CR_low_plot %>%
    mutate(  RH = ifelse(meanChoice <= 0.5, "No", "Yes"))
summary(factor(Data.Exp2.Liking_CR_low_plot_yesno$RH))

Data.Exp2.Liking_CR_low_plot_yesno$meanChoice <- ifelse(Data.Exp2.Liking_CR_low_plot_yesno$meanChoice==0, 0.01,
                                                 Data.Exp2.Liking_CR_low_plot_yesno$meanChoice)

Figure5.plot.MusicInformation_Liking_LOW_yesno <- ggplot(Data.Exp2.Liking_CR_low_plot_yesno, aes(reorder(id, -meanChoice),y= meanChoice, fill=RH)) +
    geom_bar(stat="identity", width=0.2) +
    scale_fill_manual(values=c("#56B4E9","#E69F00")) +
    theme_bw() +
    ylim(0,1) +
    ylab("Proportion recognised chosen") + 
    xlab("Participants")

Figure5.plot.MusicInformation_Liking_LOW_yesno + 
    theme(axis.text=element_text(size=14),legend.text=element_text(size=14),
          axis.title=element_blank(), axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())

ggsave("Figure5.plot.MusicInformation_Liking_LOW_yesno.pdf", width=28, height=6, units = c("cm"),
       dpi=300, device = "pdf")

summary(factor(Data.Exp2.Liking_CR_low_plot_yesno$RH)) #No= 43/ Yes= 8
binom.test(8,43) # 84%

###Medium
Data.Exp2.Liking_CR_Med_plot <- Data.Exp2.Liking_CR_Med %>% 
    group_by(id) %>%
    summarize(meanChoice= mean(Choice, na.rm= TRUE))
Data.Exp2.Liking_CR_Med_plot_yesno <- Data.Exp2.Liking_CR_Med_plot %>%
    mutate(  RH = ifelse(meanChoice <= 0.5, "No", "Yes"))
summary(factor(Data.Exp2.Liking_CR_Med_plot_yesno$RH)) # No= 37 (67%) vs. Yes= 18 (33%)

Data.Exp2.Liking_CR_Med_plot_yesno$meanChoice <- ifelse(Data.Exp2.Liking_CR_Med_plot_yesno$meanChoice==0, 0.01,
                                                        Data.Exp2.Liking_CR_Med_plot_yesno$meanChoice)

Figure5.plot.MusicInformation_Liking_MED_yesno <- ggplot(Data.Exp2.Liking_CR_Med_plot_yesno, aes(reorder(id, -meanChoice),y= meanChoice, fill=RH)) +
    geom_bar(stat="identity", width=0.2) +
    scale_fill_manual(values=c("#56B4E9","#E69F00")) +
    theme_bw() +
    ylim(0,1) +
    ylab("Proportion recognised chosen") + 
    xlab("Participants")

Figure5.plot.MusicInformation_Liking_MED_yesno + 
    theme(axis.text=element_text(size=14),legend.text=element_text(size=14),
          axis.title=element_blank(), axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())

ggsave("Figure5.plot.MusicInformation_Liking_MED_yesno.pdf", width=28, height=6, units = c("cm"),
       dpi=300, device = "pdf")

###HIGH
Data.Exp2.Liking_CR_High_plot <- Data.Exp2.Liking_CR_High%>% 
    group_by(id) %>%
    summarize(meanChoice= mean(Choice, na.rm= TRUE))
Data.Exp2.Liking_CR_High_plot_yesno <- Data.Exp2.Liking_CR_High_plot %>%
    mutate(  RH = ifelse(meanChoice <= 0.5, "No", "Yes"))
summary(factor(Data.Exp2.Liking_CR_High_plot_yesno$RH)) #No= 38 (30%)/ Yes= 89 (70%)

Data.Exp2.Liking_CR_High_plot_yesno$meanChoice <- ifelse(Data.Exp2.Liking_CR_High_plot_yesno$meanChoice==0, 0.01,
                                                        Data.Exp2.Liking_CR_High_plot_yesno$meanChoice)

Figure5.plot.MusicInformation_Liking_HIGH_yesno <- ggplot(Data.Exp2.Liking_CR_High_plot_yesno, aes(reorder(id, -meanChoice),y= meanChoice, fill=RH)) +
    geom_bar(stat="identity", width=0.2) +
    scale_fill_manual(values=c("#56B4E9","#E69F00")) +
    theme_bw() +
    ylim(0,1) +
    ylab("Proportion recognised chosen") + 
    xlab("Participants")

Figure5.plot.MusicInformation_Liking_HIGH_yesno + 
    theme(axis.text=element_text(size=14),legend.text=element_text(size=14),
          axis.title=element_blank(), axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())

ggsave("Figure5.plot.MusicInformation_Liking_HIGH_yesno.pdf", width=28, height=6, units = c("cm"),
       dpi=300, device = "pdf")

binom.test(8,43) #low
binom.test(18,37) #Medium
binom.test(23,89) #High

# Analysis 4: Correlations
### Choice and Recognition have to be 0/1
####Choice: 0= not chosen vs. 1= chosen
####Recognition: 0= novel vs. 1= learned
Data.Exp2.Correlations$Liking <- as.numeric(Data.Exp2.Correlations$Liking)
Data.Exp2.Correlations$Choice <- as.numeric(Data.Exp2.Correlations$Choice)
Data.Exp2.Correlations$Recognition <- as.numeric(Data.Exp2.Correlations$Recognition)

library(corrplot)
res.Exp2<- cor(Data.Exp2.Correlations, use="complete.obs")
res1.Exp2 <- cor.mtest(Data.Exp2.Correlations, conf.level = .95)
res1.Exp2
corrplot(res.Exp2, type = "upper", order = "hclust", p.mat=res1.Exp2$p,addCoef.col = "black",
         tl.col = "black", tl.srt = 45)

library(ppcor)
pcor.test(Data.Exp2.Correlations$Choice, Data.Exp2.Correlations$Recognition, Data.Exp2.Correlations$Liking,  method = "pearson") #controlling for liking
pcor.test(Data.Exp2.Correlations$Choice, Data.Exp2.Correlations$Liking, Data.Exp2.Correlations$Recognition,  method = "pearson") #controlling for recognition

pcor.test(Data.Exp2.Correlations$Recognition, Data.Exp2.Correlations$Choice, Data.Exp2.Correlations$Liking,  method = "pearson") #controlling for liking
pcor.test(Data.Exp2.Correlations$Recognition, Data.Exp2.Correlations$Liking, Data.Exp2.Correlations$Choice,  method = "pearson") #controlling for Choice

pcor.test(Data.Exp2.Correlations$Liking, Data.Exp2.Correlations$Choice, Data.Exp2.Correlations$Recognition,  method = "pearson") #controlling for recognition
pcor.test(Data.Exp2.Correlations$Liking, Data.Exp2.Correlations$Recognition, Data.Exp2.Correlations$Choice,  method = "pearson") #controlling for choice
