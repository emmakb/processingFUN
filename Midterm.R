###MIDTERM EXERCISE###- BIOS13- Emma Bradshaw
rm(list=ls())

#load necessary packages
library(ggplot2)
library(ggfortify)
library(dplyr)
library(readxl)

#Going to be working with the butterfly data set - import data
butterflies <- read_excel("butterflies.xlsx")
View(butterflies)
data<-butterflies

#Research questions: Does the larval and maternal host effect the adult weight of butterflies? 
#Is this effect the same for both sexes? 
#STEP 1: SUBSET THE DATA BY SEX
males<-data[data$Sex=="M",]
females<-data[data$Sex=="F",]

#STEP 2: FIT THE MODELS
#Run two two-way ANOVAs
#First model tests the interaction between hosts in males
butterfly_model1<- lm(AdultWeight~LarvalHost*MaternalHost, data=males) 
par(mfrow=c(2,2))
autoplot(butterfly_model1, smooth.colour=NA) #residuals are fairly well distributed
#Second model tests the interaction between hosts in females
butterfly_model2<- lm(AdultWeight~LarvalHost*MaternalHost, data=females) 
par(mfrow=c(2,2))
autoplot(butterfly_model2, smooth.colour=NA) #residuals are fairly well distributed


#View and interpret model outputs
anova(butterfly_model1)
summary(butterfly_model1)

anova(butterfly_model2)
summary(butterfly_model2)

#STEP 3: PLOT THE MODELS
#Basic plot to visualize data
ggplot(butterfly_model1, aes(x=MaternalHost, y=AdultWeight, colour=MaternalHost, group=MaternalHost)) +
  geom_point() +
  theme_classic()

#Too many points to see, so find the mean and sd for each combination
sum_male_butter<- males%>%
    group_by(LarvalHost, MaternalHost) %>%
    summarise(meanweight=mean(AdultWeight),
              seweight=sd(AdultWeight/sqrt(n())))

sum_female_butter<- females%>%
  group_by(LarvalHost, MaternalHost) %>%
  summarise(meanweight=mean(AdultWeight),
            seweight=sd(AdultWeight/sqrt(n())))
  
#Updated plot for males
ggplot(sum_male_butter, aes(x=MaternalHost, y=meanweight, colour=LarvalHost, group=LarvalHost)) + 
    geom_point() +
    geom_line() +
    geom_errorbar(aes(ymin=meanweight-seweight,
                      ymax=meanweight+seweight), width=0.1) +
    labs(x="Maternal Host", y="Mean adult weight (mg)",colour="Larval Host") +
  theme(axis.line = element_line(colour="black"),
        axis.text = element_text(colour="black"), 
        aspect.ratio = 1) +
  theme_classic()

#Updated plot for females
ggplot(sum_female_butter, aes(x=MaternalHost, y=meanweight, colour=LarvalHost, group=LarvalHost)) + 
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin=meanweight-seweight,
                    ymax=meanweight+seweight), width=0.1) +
  scale_color_manual(values=c("orchid3","seagreen3")) +
  labs(x="Maternal Host", y="Mean adult weight (mg)", colour="Larval Host") +
  theme(axis.line = element_line(colour="black"),
        axis.text = element_text(colour="black"), 
        aspect.ratio = 1) +
  theme_classic()



