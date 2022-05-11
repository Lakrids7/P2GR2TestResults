#------- Libraries -------
library(tidyverse)
library(readbulk)
library(magrittr)
library(ggpubr)
library(reshape2)
library(Hmisc)
library(pastecs)

# https://www.datanovia.com/en/lessons/wilcoxon-test-in-r/#:~:text=The%20Wilcoxon%20test%20is%20a,%2Dsample%20and%20two%2Dsamples.

#------- Load ------
df = readbulk::read_bulk('Data', sep=';', na.strings = 'none', stringsAsFactors=FALSE)
GEQ = read.table("GEQResult.csv", header = TRUE, sep=",")

#------- Clean Data ------
df$Condition <- as.factor(df$Condition)

df %<>% rename(ParticipantID = ï..ParticipantID)
df$ParticipantID <- as.factor(df$ParticipantID)

GEQ %<>% rename(ParticipantID = ï..ParticipantID)
GEQ$ParticipantID <- as.factor(GEQ$ParticipantID)

df.m <- melt(df,id.vars='Condition', measure.vars=c('FoodFightersPer','NonFoodFightersPer'))

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

GEQ_QUESI_List <- GEQ %<>%
  rename(
    ParticipantID = Participant_ID,
    List_Question1 = "1. I felt annoyed",
    List_Question2 = "2. I enjoyed it",
    List_Question3 = "3. I felt irritable",
    List_Question4 = "4. I lost track of time",
    List_Question5 = "5. I felt competent",
    List_Question6 = "6. I was deeply concentrated in the game",
    List_Question7 = "7. I felt frustrated",
    List_Question8 = "8. I lost connection with the outside world",
    List_Question9 = "9. I felt content",
    List_Question10 = "10. I felt skilful",
    List_Question11 = "11. I thought it was fun",
    List_Question12 = "12. I was fully occupied with the game",
    List_Question13 = "13. I felt happy",
    List_Question14 = "14. It gave me a bad mood",
    List_Question15 = "15. I thought about other things",
    List_Question16 = "16. I found it tiresome",
    List_Question17 = "17. I felt good at it ",
    List_Question18 = "18. I felt successful",
    List_Question19 = "19. I forgot everything around me",
    List_Question20 = "20. I felt good",
    List_Question21 = "21. I felt bored",
    List_Question22 = "22. I was fast at reaching the game's targets",
  )



ggplot(df, aes(x=Condition, y=CorrectItem, color=Condition)) +
  stat_ydensity() +
  theme_classic()

#------- Analysis condition ------

ggplot(df, aes(x=CorrectItem, color=Condition)) + 
  geom_density(alpha=.2) +
  theme_classic()

ggqqplot(df$CorrectItem)+ylab("Total items remembered")+theme(
  axis.text.x = element_text(size = 14),
  axis.text.y = element_text(size = 14),
  axis.title = element_text(size = 14),
  legend.title = element_text(size = 14),
  legend.text = element_text(size = 14),
  panel.border = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.line = element_line(colour = "black")
)
ggqqplot(df$FoodFightersPer)
ggqqplot(df$NonFoodFightersPer)
shapiro.test(df$CorrectItem)
shapiro.test(df$FoodFightersPer)
shapiro.test(df$NonFoodFightersPer)

stat.desc(df)
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

summary(glm(NonFoodFightersPer ~ Condition, data = df)) 



#------- Boxplot of foodfighters and non-foodfighters with both conditions -------
ggplot(df.m) +
  geom_boxplot(aes(x=Condition, y=value, color=variable))+
  theme_classic()



