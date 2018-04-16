setwd("D:\\Study  Materials\\R Language\\DataSet")

baseball=read.csv("baseball.csv")

View(baseball)

str(baseball)

moneyball=subset(baseball,Year<2002)
str(moneyball)

moneyball$RD=moneyball$RS-moneyball$RA

plot(moneyball$RD,moneyball$W)

winsreg=lm(moneyball$W~moneyball$RD,data=moneyball)
summary(winsreg)

#80.881375+0.105766(RD) >=95

#If a baseball team scores 713 runs and allows 614 runs, how many games do we expect the team 
#to win?
#Using the linear regression model constructed during the lecture, enter the number of games 
#we expect the team to win:

80.881375+(0.105766*99)

#Model to build number of Run scored.

#Two Important Variable

#on-base percentage, or OBP, and slugging percentage, or SLG.
#On-base percentage is the percentage
#of time a player gets on base, including walks.
#Batting Average is the percentage
#of time a player gets on base, exclude walks.
#Slugging percentage measures how far
#a player gets around the bases on his turn,
#and measures the power of a hitter.

RSreg=lm(RS~SLG+OBP+BA,data=moneyball)
summary(RSreg)

#we can see that the coefficient for batting average is negative.
#This implies that, all else being equal,a team with a lower batting average
#will score more runs, which is a little counterintuitive.
#What's going on here is a case of multicollinearity.

cor(moneyball$SLG,moneyball$OBP)
cor(moneyball$BA,moneyball$OBP)
cor(moneyball$SLG,moneyball$BA)

cor(moneyball$RS,moneyball$BA)
cor(moneyball$RS,moneyball$SLG)
cor(moneyball$RS,moneyball$OBP)

RSreg_Mode12=lm(RS~SLG+OBP,data=moneyball)
summary(RSreg_Mode12)

#If a baseball team's OBP is 0.311 and SLG is 0.405, how many runs do we expect the team to score?

-804.63+(0.405*1584.91)+(2737.77*0.311)

#If a baseball team's opponents OBP (OOBP) is 0.297 and oppenents SLG (OSLG) is 0.370, 
#how many runs do we expect the team to allow?

-837.38+(0.370*1514.29)+(2913.60*0.297)
