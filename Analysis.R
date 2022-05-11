#------- Libraries -------
library(tidyverse)
library(readbulk)
library(magrittr)
library(ggpubr)

# https://www.datanovia.com/en/lessons/wilcoxon-test-in-r/#:~:text=The%20Wilcoxon%20test%20is%20a,%2Dsample%20and%20two%2Dsamples.

#------- Load ------
df = readbulk::read_bulk('Data', sep=';', na.strings = 'none', stringsAsFactors=FALSE)

#------- Clean Data ------
df$Condition <- as.factor(df$Condition)

df %<>% rename(ParticipantID = ï..ParticipantID)
df$ParticipantID <- as.factor(df$ParticipantID)

#------- Overview ------

dfOverview <- df %>%
  group_by(Condition) %>%
  #filter(ParticipantID != 3) %>%
  summarise(avgCorrectItem = mean(CorrectItem),
            sdCorrectItem = sd(CorrectItem),
            avgFoodFighters = mean(FoodFighters),
            sdFoodFighters = sd(FoodFighters),
            avgNonFoodFightersPer = mean(NonFoodFightersPer),
            sdNonFoodFightersPer = sd(NonFoodFightersPer))

ggplot(df, aes(x=Condition, y=CorrectItem, color=Condition)) +
  geom_violin() +
  theme_classic()

#------- Analysis condition ------

ggplot(df, aes(x=CorrectItem, color=Condition)) + 
  geom_density(alpha=.2) +
  theme_classic()

ggqqplot(df$CorrectItem)
shapiro.test(df$CorrectItem)


summary(glm(CorrectItem ~ Condition, data = df)) 

#------- Analysis FoodFighters ------

ggplot(df, aes(x=Condition, y=FoodFighters, color=Condition)) +
  geom_boxplot() +
  theme_classic()

# only food fighters elements
summary(glm(FoodFighters ~ Condition, data = df)) 


ggplot(df, aes(x=Condition, y=NonFoodFightersPer, color=Condition)) +
  geom_violin() +
  geom_point() +
  theme_classic()
xcxc
summary(glm(NonFoodFightersPer ~ Condition, data = df)) 

library(reshape2)
df.m <- melt(df,id.vars='Condition', measure.vars=c('FoodFightersPer','NonFoodFightersPer'))

ggplot(df.m) +
  geom_boxplot(aes(x=Condition, y=value, color=variable))+
  theme_classic()

ggplot(df, aes(x=Condition)) +
  geom_boxplot(aes(y=FoodFightersPer),color="green") +
  geom_boxplot(aes(y=NonFoodFightersPer),color="blue") +
  theme_classic()



