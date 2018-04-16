setwd("D:\\Study  Materials\\R Language\\DataSet")

climate_change=read.csv("climate_change.csv")

str(climate_change)
summary(climate_change)

#Compute the correlations between all the variables in the dataset except Date & Month. 

cor(subset(climate_change, select = c(-Year,-Month)))

#Which of the following independent variables is N2O highly correlated with (absolute 
#correlation greater than 0.7)? 
#CO2,CH4,CFC.12,Temp

#Find out which variables are highly correlated with the Dependent Variable? 
#CO2,N2O,CFC.12,Temp,CH4

#What will be the prediction of a baseline Model? 
pred=mean(climate_change$Temp)

#SST/TSS

sum((climate_change$Temp--pred)^2)


