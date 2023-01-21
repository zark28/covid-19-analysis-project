rm(list=ls()) #removes all variables stored previously
install.packages("Hmisc")
library(Hmisc)
data <- read.csv("C:/Users/raswe/Downloads/Compressed/archive_1/COVID19_line_list_data.csv")
describe(data)
#clean up data on death column
data$death_dummy<- as.integer(data$death !=0)
#death rate
sum(data$death_dummy)/ nrow(data)
#AGe
#claim: people who die are older than people who survive 

#find the stats for the various groups
dead=subset(data,death_dummy==1)
alive=subset(data,death_dummy==0)

#find the mean ages of the groups ("na.rm=true" ignores all unknown entries)
mean(dead$age, na.rm = TRUE)
mean(alive$age, na.rm = TRUE)

# is the results statistically significant?
t.test(alive$age,dead$age, alternative = "two.sided", conf.level = 0.95)
#we can say with 95% confidence:people who die are much older than those alive
#if p-value< 0.5 , we reject the null hypothesis.
#this means our results are statistically significant since p-value ~ 0


#Gender
#claim: gender has no effect 

#find the stats for the various groups
men=subset(data,gender=="male")
women=subset(data,gender=="female")

#find the mean death rate of the groups ("na.rm=true" ignores all unknown entries)
mean(men$death_dummy, na.rm = TRUE)
mean(women$death_dummy, na.rm = TRUE)

# is the results statistically significant?
t.test(men$death_dummy,women$death_dummy, alternative = "two.sided", conf.level = 0.99)
#99% confidence :men have from 0.8% to 8.8% chance higher of dying
#p-value = 0.002 < 0.5 , so results are statistically significant 
#Means men have a higher death rate that women in this population sample and it is a true
#representation of the population
l