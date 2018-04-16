#read the USDA.csv file into R

USDA=read.csv("D:\\Study  Materials\\R Language\\DataSet\\USDA.csv")

#get the datatype of each column

str(USDA)
dim(USDA)

#get the high level statistical information of the columns
#study the Sodium variables particulary

summary(USDA)

#get the number of missing values in each column

sapply(USDA,function(y) sum(length(which(is.na(y)))))
sapply(USDA,function(x){sum(is.na(x))})

#which food (description column gives the food) corresponds to the maximum level of Sodium

which.max(USDA$Sodium)
USDA$Description[265]

#createa  new dataframe called High_sodium that has >10000 Sodium content

High_sodium=subset(USDA,USDA$Sodium>10000)

#how many food have High Sodium. print all their descriptions

High_sodium$Description

#CAVIAR has very high sodium content. fidn which row has the food CAVIAR.
#Also find how much sodium does it have

USDA$Description[grep("CAVIAR",USDA$Description)] 
USDA$Sodium[grep("CAVIAR",USDA$Description)] 

#find mean of Sodium and SD of Sodium in original df

mean(USDA$Sodium,na.rm=T)
sd(USDA$Sodium,na.rm=T)

#plot a scatter of Protein vs Totalfat . add red color to the points

plot(USDA$Protein,USDA$TotalFat,col="red")

#plot a histogram of Vitamon C levels.

hist(USDA$VitaminC)

#zoom into the histogram from 0 till 100. hint xlim=c(0,100)

hist(USDA$VitaminC,xlim=c(0,100))

#play witht the breaks parameter to make a good histomgram

hist(USDA$VitaminC,xlim=c(0,100),breaks=300)

#create a normal boxplot of Sugar. What do you observe

boxplot(USDA$Sugar)

#add a new variable called high sodium and make ot 1 of its
#> avergae Sodium and =0 if its<=mean(Sodium).
#similarly for High Fat, high Protein , High TotalFat & high carbs

USDA$HighSodium=ifelse(USDA$Sodium>mean(USDA$Sodium,na.rm=T),1,0)
USDA$HighProtein=ifelse(USDA$Protein>mean(USDA$Protein,na.rm=T),1,0)
USDA$HighCarbohydrate=ifelse(USDA$Carbohydrate>mean(USDA$Carbohydrate,na.rm=T),1,0)
USDA$HighTotalFat=ifelse(USDA$ TotalFat>mean(USDA$TotalFat,na.rm=T),1,0)

#find how many foods have high NA levels using the new variable that you createe

sum(is.na(USDA$HighSodium))
sum(is.na(USDA$HighProtein))
sum(is.na(USDA$HighCarbohydrate))
sum(is.na(USDA$HighTotalFat))

#find how mnay have both high sodium and high toatl fat (hint use table function)

table(USDA$HighSodium,USDA$HighTotalFat)

#Compute the average amount of iron for both high and low protein foods
#hint use tapply

tapply(USDA$Iron,USDA$HighProtein,mean,na.rm=T)

#Do foods with high carbs have low Vitamic C. Howwill you infer this..
#hint use tapply and summary together

tapply(USDA$VitaminC,USDA$HighCarbohydrate,mean,na.rm=T)
summary(USDA)
#stay healthy guys... eat the right food...