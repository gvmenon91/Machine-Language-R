setwd("D:\\Study  Materials\\R Language\\DataSet")

wine_train=read.csv("wine edx.csv")

str(wine_train)
dim(wine_train)
View(wine_train)

is.na(wine_train$Age)

summary(wine_train)

Model_1=lm(Price~AGST,data=wine_train)
Model_1

summary(Model_1)

Model_1$residuals

SSE=sum(Model_1$residuals^2)
SSE

Model_2=lm(Price~AGST+HarvestRain,data=wine_train)
summary(Model_2)

SSE=sum(Model_2$residuals^2)
SSE

Model_3=lm(Price~.,data=wine_train)
summary(Model_3)

#use the dataset wine.csv to create a linear regression model to predict Price using 
#HarvestRain and WinterRain as independent variables. Using the summary output of this model,
#What is the "Multiple R-squared" value of your model?

Model_4=lm(Price~WinterRain+HarvestRain,data=wine_train)
summary(Model_4)
SSE=sum(Model_4$residuals^2)
SSE

#What is the coefficient for HarvestRain?
#-4.971e-03

#What is the intercept coefficient?
#7.865e+00

Model_5=lm(Price~AGST+HarvestRain+WinterRain+Age,data=wine_train)

summary(Model_5)
summary(Model_3)

#Before, Age was not significant at all in our model.
#But now, Age has two stars, meaning
#that it's very significant in this new model.
#This is due to something called multicollinearity.
#Age and FrancePopulation are what we call highly correlated.

#create a linear regression model to predict Price using HarvestRain and WinterRain as 
#independent variables

Model_6=lm(Price~HarvestRain+WinterRain,data=wine_train)
summary(Model_6)

#Is the coefficient for HarvestRain significant?
#Yes

#Is the coefficient for WinterRain significant?
#No

cor(wine_train$Price,wine_train$WinterRain)
cor(wine_train$FrancePop,wine_train$Age)

#To find corelation between all column 
cor(wine_train)

#Since the corelarion between Age and francepop is High we remove the Vraiable and Build Model
Model_7=lm(Price~AGST+WinterRain+HarvestRain,data=wine_train)
summary(Model_7)

Model_7=lm(Price~AGST+WinterRain+HarvestRain+Age,data=wine_train)
summary(Model_7)

#R^2 dropped significantly since Age removed Model5=0.8286 but Model7 we got only 0.7537

#Using the data set wine.csv, what is the correlation between HarvestRain and WinterRain?
#-0.275440854

#Making Predictions

wine_Test=read.csv("wine_test.csv")

predictions=predict(Model_7,newdata=wine_Test)
predictions

wine_Test$Price


#SSE
SSE=sum((wine_Test$Price-predictions)^2)

#SST Check Baseline Model Accuracy on Train Data
SST=sum((wine_Test$Price-mean(wine_train$Price))^2)

#Adjusted R-squared
1-(SSE/SST)

#R² = 1 - SSE/SST,
#where SST is calculated using the average value of the dependent variable on the training set.
#Since SSE and SST are the sums of squared terms, we know that both will be positive.
#Thus SSE/SST must be greater than or equal to zero. 
#This means it is not possible to have an out-of-sample R² value of 2.4.
#However, all other values are valid (even the negative ones!), since SSE can be more or less than SST, due to the fact that this is an out-of-sample R², not a model R².