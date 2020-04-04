#changin data again
library(tidyverse)

#solving the issue
Data.Exp2.glmer %>%
    group_by(IV,Position) %>%
    summarize(total= sum(as.numeric(Choice),na.rm=T),
              count= n(),
              mean= mean(as.numeric(Choice),na.rm=T),
              sd= sd(as.numeric(Choice),na.rm=T))

Data.Exp2.glmer$Choice <- as.numeric(Data.Exp2.glmer$Choice )
Data.Exp2.glmer$Recognition <- as.factor(Data.Exp2.glmer$Recognition)
Data.Exp2.glmer$id <- as.factor(Data.Exp2.glmer$id)
Data.Exp2.glmer$Song <- as.factor(Data.Exp2.glmer$Song)
Data.Exp2.glmer$Brand <- as.factor(Data.Exp2.glmer$Brand)
Data.Exp2.glmer$Position <- as.factor(Data.Exp2.glmer$Position)
Data.Exp2.glmer$Task <- as.factor(Data.Exp2.glmer$Task)
Data.Exp2.glmer$Clip <- as.factor(Data.Exp2.glmer$Clip)
Data.Exp2.glmer$BrandCategory <- as.factor(Data.Exp2.glmer$BrandCategory)

data.original <- Data.Exp2.glmer %>% filter(Task == "Choice") %>% 
    drop_na(Choice) %>% 
    dplyr::select(-Age,-Gender,-IV)
data.pos1 <- data.original %>% filter(Position == "First")  %>%  
    dplyr::select(-Task, -Response)   %>%  rename(Choice.Pos1 = Choice) %>%
    rename(Song.Pos1 = Song) %>% rename(Brand.Pos1 = Brand) %>%
    mutate(LearnPos1 = as.factor(ifelse(Recognition == "Learned", 1, 0))) %>%
    mutate(NovelPos1 = as.factor(ifelse(Recognition == "Novel", 1, 0)))
data.pos2 <- data.original %>% filter(Position == "Second")  %>%  
    dplyr::select(-Task, -Response)  %>%  rename(Choice.Pos2 = Choice) %>%
    rename(Song.Pos2 = Song) %>% rename(Brand.Pos2 = Brand) %>%
    mutate(LearnPos2 = as.factor(ifelse(Recognition == "Learned", 1, 0))) %>%
    mutate(NovelPos2 = as.factor(ifelse(Recognition == "Novel", 1, 0)))
    
data2.final <- merge(data.pos1,data.pos2,by=c("id","Trial","BrandCategory","Combination"))
data2.final <- as_tibble(data2.final) # 129 participants
data2.final.corrected <- data2.final %>% mutate(ChosenPosition = as.numeric(ifelse(Choice.Pos1 == 1, 0,
                                        ifelse(Choice.Pos2 == 1, 1, "NA")))) %>%
    rename(Clip = Clip.x) 

is.na(data2.final.corrected$ChosenPosition)
table(data.pos1$Clip)

data2.final.corrected %>%
    group_by(Clip,LearnPos1,LearnPos2) %>%
    summarize(total= sum(as.numeric(ChosenPosition),na.rm=T),
              count= n(),
              mean= mean(as.numeric(ChosenPosition),na.rm=T),
              sd= sd(as.numeric(ChosenPosition),na.rm=T))

data2.final.corrected %>%
    group_by(Clip,NovelPos1,NovelPos2) %>%
    summarize(total= sum(as.numeric(ChosenPosition),na.rm=T),
              count= n(),
              mean= mean(as.numeric(ChosenPosition),na.rm=T),
              sd= sd(as.numeric(ChosenPosition),na.rm=T))

library(brms)
BRM.Exp2.Learn <-  brm(ChosenPosition ~ 0+LearnPos1 : LearnPos2 + (1 | id)+(1|Brand.Pos1) + (1|Brand.Pos2) +
                           (1|Song.Pos1) + (1|Song.Pos2) , data=data2.final.corrected, cores= 4,
                       iter= 8000, control= list(max_treedepth = 10, adapt_delta=0.99),family = bernoulli()) #09:34

summary(BRM.Exp2.Learn)
plot(BRM.Exp2.Learn, pars = c("LearnPos1", "LearnPos2")) 
plot(conditional_effects(BRM.Exp2.Learn, effects = "LearnPos1:LearnPos2"))

BRM.Exp2.Novel <-  brm(ChosenPosition ~ 0+NovelPos1 : NovelPos2 + (1 | id)+(1|Brand.Pos1) + (1|Brand.Pos2) +
                           (1|Song.Pos1) + (1|Song.Pos2) , data=data2.final.corrected, cores= 4,
                       iter= 8000, control= list(max_treedepth = 10, adapt_delta=0.99),family = bernoulli()) #09:34

summary(BRM.Exp2.Novel)
plot(BRM.Exp2.Novel, pars = c("NovelPos1", "NovelPos2")) 
plot(conditional_effects(BRM.Exp2.Novel, effects = "NovelPos1:NovelPos2"))


##

General.brm.Exp2<- brm(Choice ~ 0+IV+Position + (1 | id)+(1|Brand) + (1|Song),
                         data=Data.Exp2.glmer, cores= 4,
                         iter= 8000, control= list(max_treedepth = 10, adapt_delta=0.99),
                         family = bernoulli()) #there is sth wrong!
summary(General.brm.Exp2)
plot(General.brm.Exp2, pars = c("IV", "Position")) 
plot(conditional_effects(General.brm.Exp2, effects = "IV:Position"))


General.glmer.Exp2b<- glmer(Choice ~ IV + Position + (1 | id)+(1|Brand) + (1|Song),
                              data=Data.Exp2.glmer, family= binomial,nAGQ=0 )
Anova(General.glmer.Exp2b,Type="III")
summary(General.glmer.Exp2b)

General.glmer.Exp1b<- glmer(Choice ~ IV + Position + (1 | id)+(1|Brand) + (1|Song),
                            data=Data.Exp1.glmer, family= binomial,nAGQ=0 )
Anova(General.glmer.Exp1b,Type="III")
summary(General.glmer.Exp1b)


library(emmeans)
emmip(General.glmer.Exp2b, Position ~ IV)
emmeans(General.glmer.Exp2b, pairwise ~ Position : IV)

model2.E2.CR.predictions= round(predict(General.glmer.Exp2b,type="response")) #calculate model's classification accuracy
acc.table.model2.E2.CR= table(na.omit(Data.Exp2.glmer$Choice), model2.E2.CR.predictions) #compare the predictions agaisnt the actual data
accuracy.model2.E2.CR= sum(diag(acc.table.model2.E2.CR))/sum(acc.table.model2.E2.CR) #compute the accuracy of this table
accuracy.model2.E2.CR #.647

#psothoc using HOLM
summary(glht(General.glmer.Exp2b, linfct = mcp(IV = "Tukey")), test = adjusted("holm")) #h




General.glmer.Exp2c<- glmer(Choice ~ IV * Position + (1 | id)+(1|Brand) + (1|Song),
                            data=Data.Exp2.glmer, family= binomial,nAGQ=0 )
Anova(General.glmer.Exp2c,Type="III")
summary(General.glmer.Exp2c)

emmip(General.glmer.Exp2c, Position ~ IV)

emmeans(General.glmer.Exp2c, pairwise ~ Position : IV)

