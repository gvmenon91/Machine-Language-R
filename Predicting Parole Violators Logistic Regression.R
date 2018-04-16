setwd("D:\\Study  Materials\\R Language\\DataSet")
Parole=read.csv("parole.csv")
View(Parole)

#How many parolees are contained in the dataset?
nrow(Parole)

#How many of the parolees in the dataset violated the terms of their parole?
nrow(subset(Parole,Parole$violator==1))

#You should be familiar with unordered factors (if not, review the Week 2 homework problem "Reading Test Scores").
#Which variables in this dataset are unordered factors with at least three levels?

Parole$male=as.factor(Parole$male)
Parole$race=as.factor(Parole$race)
Parole$state=as.factor(Parole$state)
Parole$multiple.offenses=as.factor(Parole$multiple.offenses)
Parole$violator=as.factor(Parole$violator)
Parole$crime=as.factor(Parole$crime)
str(Parole)

#state,crime

#How does the output of summary() change for a factor variable as compared to a numerical variable?
#The output becomes similar to that of the table() function applied to that variable 
Parole$state = as.factor(Parole$state)
Parole$crime = as.factor(Parole$crime)
#The output of summary(parole$state) or summary(parole$crime) now shows a breakdown of the number of parolees with each 
#level of the factor, which is most similar to the output of the table() function.

#To ensure consistent training/testing set splits, run the following 5 lines of code (do not include the line numbers at the beginning):
set.seed(144)
library(caTools)
split = sample.split(Parole$violator, SplitRatio = 0.7)
train_Parole = subset(Parole, split == TRUE)
test_Parole = subset(Parole, split == FALSE)

#Roughly what proportion of parolees have been allocated to the training and testing sets?
#70% to the training set, 30% to the testing set

#Now, suppose you re-ran lines [1]-[5] of Problem 3.1. What would you expect?
#The exact same training/testing set split as the first execution of [1]-[5]

#If you instead ONLY re-ran lines [3]-[5], what would you expect?
#A different training/testing set split from the first execution of [1]-[5]

#If you instead called set.seed() with a different number and then re-ran lines [3]-[5] of Problem 3.1, 
#what would you expect?
#A different training/testing set split from the first execution of [1]-[5] correct

#Check if any NA Present
sum(is.na(Parole))

#Building a Logistic Regression Model
Model_1=glm(violator~.,data=train_Parole,family=binomial)
summary(Model_1)

Model_2=glm(violator~race+state+max.sentence+multiple.offenses,data=train_Parole,family=binomial)
summary(Model_2)

test_Parole$pred_test=predict(Model_2,newdata=test_Parole,type="response")
table(test_Parole$violator,test_Parole$pred_test > 0.5)

#race,state4,multiple.offenses

#What can we say based on the coefficient of the multiple.offenses variable?
#If we have a coefficient c for a variable, then that means the log odds (or Logit) are increased by c for a unit increase
#in the variable.
#If we have a coefficient c for a variable, then that means the odds are multiplied by e^c for a unit increase in the
#variable.

#For parolees A and B who are identical other than A having committed multiple offenses, the predicted log odds of A is 
#1.61 more than the predicted log odds of B. Then we have:
  
ln(odds of A) = ln(odds of B) + 1.61
exp(ln(odds of A)) = exp(ln(odds of B) + 1.61)
exp(ln(odds of A)) = exp(ln(odds of B)) * exp(1.61)
odds of A = exp(1.61) * odds of B
odds of A= 5.01 * odds of B
#In the second step we raised e to the power of both sides. In the third step we used the exponentiation rule 
#that e^(a+b) = e^a * e^b. In the fourth step we used the rule that e^(ln(x)) = x.

#Consider a parolee who is male, of white race, aged 50 years at prison release, from the state of Maryland, served 3 months,
#had a maximum sentence of 12 months, did not commit multiple offenses, and committed a larceny. 
#male=1, race=1, age=50, state2=0, state3=0, state4=0, time.served=3, max.sentence=12, multiple.offenses=0, crime2=1, 
#crime3=0, crime4=0.
#According to the model, what are the odds this individual is a violator?
-3.57755 + (0.3869904*1) + (0.8867192*1) +(- 0.0001756*50) + (0.4433007*0) + (0.8349797*0)+( - 3.3967878*0) +
(- 0.1238867*3) + (0.0802954*12) + (1.6119919*0) + (0.6837143*1)+( - 0.2781054*0)+(- 0.0117627*0)

#According to the model, what is the probability this individual is a violator?
1/(1+exp(1.037021))

#What is the maximum predicted probability of a violation?
max(test_Parole$pred_test)
summary(test_Parole)

#What is the model's accuracy?
(169+10)/nrow(test_Parole)

#What is the model's sensitivity?
10/(10+13)

#What is the model's specificity?
169/(169+10)

#What is the accuracy of a simple model that predicts that every parolee is a non-violator?
179/202

#Consider a parole board using the model to predict whether parolees will be violators or not. The job of a parole board is 
#to make sure that a prisoner is ready to be released into free society, and therefore parole boards tend to be particularily 
#concerned about releasing prisoners who will violate their parole. Which of the following most likely describes their 
#preferences and best course of action?

#If the board used the model for parole decisions, a negative prediction would lead to a prisoner being granted parole, 
#while a positive prediction would lead to a prisoner being denied parole. The parole board would experience more regret 
#for releasing a prisoner who then violates parole (a negative prediction that is actually positive, or false negative) 
#than it would experience for denying parole to a prisoner who would not have violated parole (a positive prediction that 
#is actually negative, or false positive).
#Decreasing the cutoff leads to more positive predictions, which increases false positives and decreases false negatives.
#Meanwhile, increasing the cutoff leads to more negative predictions, which increases false negatives and decreases false
#positives. The parole board assigns high cost to false negatives, and therefore should decrease the cutoff.

#Which of the following is the most accurate assessment of the value of the logistic regression model with a cutoff 0.5 to 
#a parole board, based on the model's accuracy as compared to the simple baseline model?
#The model at cutoff 0.5 has 12 false positives and 11 false negatives, while the baseline model has 0 false positives and 
#23 false negatives. Because a parole board is likely to assign more cost to a false negative, the model at cutoff 0.5 is 
#likely of value to the board.
#From the previous question, the parole board would likely benefit from decreasing the logistic regression cutoffs, 
#which decreases the false negative rate while increasing the false positive rate.

#Using the ROCR package, what is the AUC value for the model?
library(ROCR)
pred = prediction(test_Parole$pred_test,test_Parole$violator)
as.numeric(performance(pred, "auc")@y.values)

#Describe the meaning of AUC in this context.
#The AUC deals with differentiating between a randomly selected positive and negative example. 
#It is independent of the regression cutoff selected.