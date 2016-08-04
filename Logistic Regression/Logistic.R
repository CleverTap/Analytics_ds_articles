
##Please install the below libraries, if already not installed
##Importing Libraries in R Env
library(ggplot2)
library(dplyr)

##Importing Input dataset

dat1 <- read.csv(".../data.csv", stringsAsFactors = F)

dat1$Age <- as.numeric(dat1$Age)
dat1$Churn <- as.numeric(dat1$Churn)

##Scatterplot of Input dataset

ggplot(dat1, aes(x = Age, y = Churn)) + geom_point(size = 1.75, colour = "brown")+
  scale_colour_hue(l=50)+
  scale_y_continuous(breaks = seq(0,1, by = 0.25)) +
  theme_bw()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),title=element_text(face = "bold",size=10,color = "darkgreen"))+
  labs(title="Scatter Plot of Age & Churn")

##Scatterplot of Input dataset with Trend line 

ggplot(dat1, aes(x = Age, y = Churn)) + geom_point(size = 1.75, colour = "brown")+
  scale_colour_hue(l=50)+
  scale_y_continuous(breaks = seq(0,1, by = 0.25)) +
  geom_smooth(method = lm,se=FALSE,fullrange=TRUE)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),title=element_text(face = "bold",size=10,color = "darkgreen"))+
  labs(title="Scatter Plot of Age & Churn with Trend Line")


##Creating new variable - AgeGroup for each user

dat1$AgeGroup <- NA

##Grouping the people based on age
dat1$AgeGroup[dat1$Age <=16] <- "<= 16"
dat1$AgeGroup[dat1$Age >= 17 & dat1$Age < 30] <- "17-29"
dat1$AgeGroup[dat1$Age >= 30 & dat1$Age < 42] <- "30-41"
dat1$AgeGroup[dat1$Age >= 42 & dat1$Age < 54] <- "42-53"
dat1$AgeGroup[dat1$Age >= 54 & dat1$Age < 67] <- "54-66"
dat1$AgeGroup[dat1$Age >= 67] <- ">= 67"

##Ordering levels of AgeGroup
dat1$AgeGroup <- ordered(dat1$AgeGroup,levels=c("<= 16","17-29","30-41","42-53","54-66",">= 67"))


##Grouping to summarise and compute new variables
df <- dat1 %>%
      group_by(AgeGroup) %>%
      summarise(total.count=n(),count=sum(Churn == 1)) 

df1 <- df %>%
      mutate(Percentage_Churn = as.numeric(count / total.count))

df2 <- df1 %>%
      mutate(Odds = Percentage_Churn/(1-Percentage_Churn), log_odds = log(Odds))

##Scatterplot of % Churn resulting into S-Shaped Curve 
ggplot(df1, aes(x = AgeGroup, y = Percentage_Churn)) + geom_point(size = 1.75, colour = "brown")+
  scale_colour_hue(l=50)+
  ylim(c(0,1)) + geom_line(aes(x = 1:6,y = Percentage_Churn))+
  geom_smooth(method = "auto",se=TRUE,fullrange=TRUE)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),title=element_text(face = "bold",size=10,color = "darkgreen"))+
  labs(title="Scatter Plot of AgeGroup & Percentage Churn")

##Log odds Scatterplot resulting in S-Shaped Curve

ggplot(df2, aes(x = AgeGroup, y = log_odds)) + geom_point(size = 1.75, colour = "brown")+
    scale_colour_hue(l=50)+
    ylim(c(-2,3.3)) + geom_line(aes(x = 1:6,y = log_odds))+
    geom_smooth(method = "auto",se=TRUE,fullrange=TRUE)+
    theme_bw()+
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),title=element_text(face = "bold",size=10,color = "darkgreen"))+
    labs(title="Scatter Plot of Log Odds")


