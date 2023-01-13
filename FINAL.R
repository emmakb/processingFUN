#FINAL EXAM- BIOS14#
#Part 1
rm(list=ls())
#Research question: What is the effect of the species type, treatment type and their interaction on ASD
library(ggplot2)
library(ggfortify)
library(dplyr)
#Step 1: Import the data
plant_dat = read.csv("exam2022_part1.csv")
plant_data<-na.omit(plant_dat) #remove NA data

#Step 2: Create the model
model_plant<-lm(GAD~ treat* sp, data=plant_data)
#check the assumptions
autoplot(model_plant, smooth.colour=NA)

#Step 3: Generate tables
anova(model_plant)
summary(model_plant)

#Step 4: Calculate means and SDs
sum_plant<- plant_data %>%
  group_by(treat,sp) %>%
  summarise(mean_area= mean(GAD),
            se_area=sd(GAD/sqrt(n()))
            )

#Step 4: Plot the two-way ANOVA
ggplot(sum_plant, aes(x=treat, y=mean_area, colour=sp, group=sp)) + 
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin=mean_area-se_area,
                    ymax=mean_area+se_area), width=0.1) +
  labs(x="Treatment", y="Gland-area distance(mm)",colour="Species") +
  theme(axis.line = element_line(colour="black"),
        axis.text = element_text(colour="black"), 
        aspect.ratio = 1) +
  theme_classic()

#Part 2____________________________________________________________________
#Research question: Is the variance in body mass most influenced by the length of the right or left horn?
#And is this true for both male and female mountain goats?
#Step 1: Import the data
goat_dat = read.table("exam2022_part2.txt", header=T) 
goat_data<-na.omit(goat_dat) #remove NA data

#Step 2: Subset the data
males<-goat_data[goat_data$sex=="M",]
females<-goat_data[goat_data$sex=="F",]

cor(males$hornL, males$hornR)
#[1] 0.9909308
cor(females$hornL, females$hornR)
#[1] 0.9911645
#****TOO MUCH COLLINEARITY- MUST COMPLETE THE ANALYSIS USING OTHER VARIABLES THAT ARE NOT AS STRONGLY CORRELATED TO ONE ANOTHER

#Step 3: Create the models
model_goat_males<-lm(hornR~mass+age, data=males)
model_goat_females<-lm(hornR~mass+age, data=females)
coefs1= summary(model_goat_males)$coef
coefs2= summary(model_goat_females)$coef
#view summary tables
summary(model_goat_males)
#R2 is 0.55 so 55% of the variance in mass is explained by the model
summary(model_goat_females)
#R2 is 0.60 so 60% of the variance in mass is explained by the model

#Step 4: Find the variance explained by each of the predictors in each model
#MALES
y_hat<-coefs1[1,1] + coefs1[2,1]*males$mass + coefs1[3,1]*males$age
var(y_hat)
var(y_hat)/var(males$hornR) #r2

y_hat1<-coefs1[1,1] + coefs1[2,1]*males$mass + coefs1[3,1]*mean(males$age)
var(y_hat1) 
#Totally variance of mass
#[1] 527.0154
var(y_hat1)/var(males$hornR)
#Proportion of the variance in mass explained by mass
#[1] 0.2786536

y_hat2<-coefs1[1,1] + coefs1[2,1]*mean(males$mass) + coefs1[3,1]*males$age
var(y_hat2) 
#Totally variance of age
#[1] 164.2233
var(y_hat2)/var(males$hornR)
#Proportion of the variance in mass explained by mass
#[1] 0.08683127

#FEMALES
yy_hat<-coefs2[1,1] + coefs2[2,1]*females$mass + coefs2[3,1]*females$age
var(yy_hat)
var(yy_hat)/var(females$hornR) #r2

y_hat3<-coefs2[1,1] + coefs2[2,1]*females$mass + coefs2[3,1]*mean(females$age)
var(y_hat3) 
#Totally variance of mass
#[1] 362.8308
var(y_hat3)/var(females$hornR)
#Proportion of the variance in mass explained by mass
#[1] 0.2138179

y_hat4<-coefs2[1,1] + coefs2[2,1]*mean(females$mass) + coefs2[3,1]*females$age
var(y_hat4) 
#Totally variance of age
#[1] 320.4032
var(y_hat4)/var(females$hornR)
#Proportion of the variance in mass explained by mass
#[1] 0.1888152

#Step 5: Check for collinearity
cor(males$mass, males$age)
#[1] 0.602958
cor(females$mass, females$age)
#[1] 0.4948573

#Step 6: Plot the meaningful regressions 
ggplot(data = males, aes(x=mass, y=hornR)) +
  geom_point() +
  geom_smooth(method="lm") +
  labs(x="Mass (kg)", y="Right horn length (mm)") +
  theme_classic()

ggplot(data = females, aes(x=mass, y=hornR)) +
  geom_point() +
  geom_smooth(method="lm", color="red") +
  labs(x="Mass (kg)", y="Right horn length (mm)") +
  theme_classic()


