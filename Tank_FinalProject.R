#Importing necessary libraries and modules for the dataset
install.packages("dplyr")
library(dplyr)
install.packages("imputeTS")
library(imputeTS)
library(tidyverse)
install.packages("cowplot")
library(cowplot)
library(dplyr)
install.packages("ggplot2")
install.packages("ggplot2")
install.packages("maps")
library(ggplot2)
library(maps)
install.packages("highcharter")

#setting up the working directory to access the dataset
setwd("C:/Users/baps/OneDrive - Northeastern University/Desktop/Data")

#importing dataset
football=read.csv("Foot.csv")
#viewing the datset
football

#geting the nams of the colums so we can remove the data
a=names(football)
a #names of the columns

#attaching the dataset
attach(football)

#ewmoving unnecessary columns
football<- football[,! names(football) 
        %in% c("Unnamed..0","Photo",
        "GKDiving","GKHandling","GKPositioning","GKReflexes","GKKicking",
        "Flag","Club.Logo","X","International.Reputation","Overall",
        "Overall.1", "Special","Wage","Skill.Moves","X.1","X")]
football

#getting the sum of null values
sum(is.na(football))

#mean of the null values
mean(is.na(football))

#checking that which columns has how many null values
nul <- sapply(football, function(football) sum(is.na(football)))

#checking whether attributes have any null values
nul

#describing summary of the dataset
summary(football)

#ommiting null values
football=na.omit(football)
football

#sorting and arranging the data
Top earners =top_earning[order(-top_10earner),]

#top 10 earner by slicing and dicing
h=football$Sal_mill
top_10earner=h[1:15]
top_10earner

#the name of the top 10 earner in the footbll industries
top_names=football$Name
top_names=top_names[1:15]
top_names

attach(football)

names(football)

#naming new attribute salary in million
football$salary_million= football$Value * 1000000

#naming the new attribute rating
football$Overall_Ratings=Crossing+Finishing+HeadingAccuracy+ShortPassing+
Dribbling+Curve+BallControl+Acceleration+Jumping+LongShots/10

#naming the new attribute salary weekly
football$weekly_salary=football$salary_million/ 52

#naming the new attribute monthly salary
football$monthly_salary=football$salary_million/12

#top players according to wage and sallry
top_player= football %>%
  select(football, Name,Nationality,Club,Sal_mill,overall_rat) %>%
  group_by(Sal_mill) %>% arrange(desc(Sal_mill)) %>% head(30)
top_player

top_player= football %>%
  select(football, Name,Nationality,Club,Sal_mill,overall_rat) %>%
  group_by(Sal_mill) %>% arrange(desc(Sal_mill)) %>% head(30)

#getting the summary of the newly generated dataset
summary(football$Sal_mill)
summary(football$weekly_sal)
summary(football$overall_rating)

#plotting the work rate of footballers by age
Quents=table(football$Work.Rate, football$Age)
Quents
workrate=as.data.frame(Quents)

#criteria for the plot
barplot(Quents, main = "Work rate distribution by age of the footballer",
        xlab = "Age",ylab = "Frequnecy")

#getting the group by count by table command
rat=table(football$oveall_rating)
rat

#plotting the histogram 
barplot(rat, main= "Histogram of Players ratings",
        xlab= "Ratings", Ylab="Frequency")

#plotting the bar graph of the footballers who earn more
football %>%
  arrange(desc(football$salary_million))%>% 
  head(10) %>%
  ggplot()+ geom_col(mapping=aes(x=football$Name,y=football$salary_million
  ,fill=football$Name))

#getting the group by count of preferred foot to the numbers of scored adn skills
skills=table(football$Preferred.Foot,football$Weak.Foot)
skills

#plotting the graph
barplot(skills, main = "Distribution of goal scored by both the foot",
        xlab ="Number of skills",ylab = "Frequency",col = c("darkblue","red"),
        legend=rownames(skills))
#getting the row names
rownames(skills)

#getting the group by count of prefereed foot
pre_foot=table(football$Preferred.Foot)
pre_foot

#plotting the graph of footballers' preference on foot
barplot(pre_foot, main = "preferred foot",xlab = "Right foot or left",
        ylab = "Frequency",col="red")