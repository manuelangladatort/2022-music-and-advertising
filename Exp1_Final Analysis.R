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
# Datasets: Data.Exp1.ttest + Data.Exp1.glmer
Data.Exp1.ttest <- read_excel("data/Data.Exp1.ttest.xlsx") #or load manually
Data.Exp1.glmer <- read_excel("data/Data.Exp1.glmer.xlsx") #or load manually

# Analysis 1: Critical pairs only - t-test
Data.Exp1.ttest$Participant= as.factor(Data.Exp1.ttest$id)
Data.Exp1.ttest$Recognition <- as.factor(Data.Exp1.ttest$Recognition)
Data.Exp1.ttest$Mean <- as.numeric(Data.Exp1.ttest$Mean)
contrasts(Data.Exp1.ttest$Recognition)<-contr.sum

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

# Analysis 2: General model - GLMER
Data.Exp1.glmer$Choice <- as.numeric(Data.Exp1.glmer$Choice )
Data.Exp1.glmer$Recognition <- as.factor(Data.Exp1.glmer$Recognition)
Data.Exp1.glmer$id <- as.factor(Data.Exp1.glmer$id)
Data.Exp1.glmer$Song <- as.factor(Data.Exp1.glmer$Song)
Data.Exp1.glmer$Brand <- as.factor(Data.Exp1.glmer$Brand)
Data.Exp1.glmer$Position <- as.factor(Data.Exp1.glmer$Position)
Data.Exp1.glmer$BrandCategory <- as.factor(Data.Exp1.glmer$BrandCategory)

Data.Exp1.glmer$IV <- paste(Data.Exp1.glmer$Clip,Data.Exp1.glmer$Recognition,sep="-")
table(Data.Exp1.glmer$IV,useNA="ifany")
Data.Exp1.glmer$IV <- as.factor(Data.Exp1.glmer$IV)
contrasts(Data.Exp1.glmer$IV)<-contr.sum

General.glmer.Exp1<- glmer(Choice ~ IV+ Position + Brand + Song + 
                               (1|id),data=Data.Exp1.glmer, family= binomial)
Anova(General.glmer.Exp1,Type="III")
summary(General.glmer.Exp1)

model2.E1.CR.predictions= round(predict(General.glmer.Exp1,type="response")) #calculate model's classification accuracy
acc.table.model2.E1.CR= table(na.omit(Data.Exp1.glmer$Choice), model2.E1.CR.predictions) #compare the predictions agaisnt the actual data
accuracy.model2.E1.CR= sum(diag(acc.table.model2.E1.CR))/sum(acc.table.model2.E1.CR) #compute the accuracy of this table
accuracy.model2.E1.CR #.652

#psothoc using HOLM
summary(glht(General.glmer.Exp1, linfct = mcp(IV = "Tukey")), test = adjusted("holm")) #holm correction
summary(glht(General.glmer.Exp1, linfct = mcp(Song = "Tukey")), test = adjusted("holm")) #holm correction 
summary(glht(General.glmer.Exp1, linfct = mcp(Brand = "Tukey")), test = adjusted("holm")) #holm correction
summary(glht(General.glmer.Exp1, linfct = mcp(Position = "Tukey")), test = adjusted("holm")) #holm correction

Data.Exp1.glmer %>%
    group_by(IV) %>%
    summarize(total= sum(as.numeric(Choice),na.rm=T),
              count= n(),
              mean= mean(as.numeric(Choice),na.rm=T),
              sd= sd(as.numeric(Choice),na.rm=T))

Data.Exp1.glmer %>%
    group_by(Position) %>%
    summarize(total= sum(as.numeric(Choice),na.rm=T),
              count= n(),
              mean= mean(as.numeric(Choice),na.rm=T),
              sd= sd(as.numeric(Choice),na.rm=T))

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