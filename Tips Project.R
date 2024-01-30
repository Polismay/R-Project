##Installing package and loading data
install.packages("regclass")
library(regclass)
data("TIPS")

##reading the first few rows and last few rows of data
head(TIPS)
tail(TIPS)

##levels of the categorical data
levels(TIPS$Gender)
levels(TIPS$Weekday)<- c('Thursday', 'Friday', 'Saturday', 'Sunday')
levels(TIPS$Weekday)
levels(TIPS$Time)
levels(TIPS$Smoker)

##loading ggplot2
remove.packages("rlang")
remove.packages("dplyr")

install.packages("rlang")
install.packages("dplyr")

library(rlang)
library(dplyr)

install.packages('ggplot2')
library(ggplot2)

summary(TIPS)

##histogram of variables
#Gender
ggplot(TIPS, aes(x=Gender))+
  geom_bar()+
  labs(y="Observation Count")
#Bill
ggplot(TIPS, aes(x=Bill))+
  geom_bar()+
  labs(y="Observation Count")
#Weekday
ggplot(TIPS, aes(x=Weekday, xlab='Days'))+
  geom_bar()+
  labs(y="Observation Count")
#Smoker
ggplot(TIPS, aes(x=Smoker, xlab='Smoker'))+
  geom_bar()+
  labs(y="Observation Count")
#Time
ggplot(TIPS, aes(x=Time, xlab='Time of Day'))+
  geom_bar()+
  labs(y="Observation Count")
#PartySize
ggplot(TIPS, aes(x=PartySize, xlab='Time of Day'))+
  geom_bar()+
  labs(y="Observation Count")

##Boxplots 
#ggplot Gender 
ggplot(data=TIPS, aes(x=Gender, y=TipPercentage))+
  geom_boxplot()
ggplot(data=TIPS, aes(x=Gender, y=Bill))+
  geom_boxplot()
##ggplot Weekday
ggplot(data=TIPS, aes(x=Weekday, y=TipPercentage))+
  geom_boxplot()
ggplot(data=TIPS, aes(x=Weekday, y=Bill))+
  geom_boxplot()
##ggplot Time
ggplot(data=TIPS, aes(x=Time, y=TipPercentage))+
  geom_boxplot()
ggplot(data=TIPS, aes(x=Time, y=Bill))+
  geom_boxplot()
##ggplot Smoker
ggplot(data=TIPS, aes(x=Smoker, y=TipPercentage))+
  geom_boxplot()
ggplot(data=TIPS, aes(x=Smoker, y=Bill))+
  geom_boxplot()
##ggplot PartySize
ggplot(data=TIPS, aes(x=PartySize, y=TipPercentage))+
  geom_point()
ggplot(data=TIPS, aes(x=PartySize, y=Bill))+
  geom_point()

#Association between categorical variables
associate(Smoker~Gender, TIPS) ##no association based on mosaic plot and pvalue is not statistically significant 
associate(Time~Weekday, TIPS) ## big difference in shading, this association is statistically significant 
associate(Smoker~Time, TIPS) ##no association based on mosaic plot and pvalue is not statistically significant 
associate(Smoker~Weekday, TIPS) ##no association based on mosaic plot and pvalue is not statistically significant 

#Association between categorical and numeric variable
associate(Bill~Gender, data=TIPS, seed=9750)
associate(Bill~Weekday, data=TIPS, seed=9750)
associate(Bill~Weekday, data=TIPS, seed=9750, permutations = 2000)
associate(Bill~Smoker, data=TIPS, seed=9750)
associate(Bill~Time, data=TIPS, seed=9750)


#Association between numeric variables
associate(Bill~PartySize, data=TIPS, seed=9750) # yes there is statistical significance 
associate(Bill~Tip, data=TIPS, seed=9750)
associate(PartySize~Tip, data=TIPS, seed=9750)
 
##Linear Regression
A <- lm(TipPercentage~PartySize, data=TIPS)
plot(Bill~PartySize, data=TIPS, xlab="PartySize", ylab="Bill")
possible_regressions(A)
anova(A)
summary(A)


