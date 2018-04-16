#1--Create a vector of the values of (e^X)sin(x) at x=3,3.1,3.2,6

exp(1)# this represents e 
x=c(3,3.1,3.2,6)
exp(1)^(x)*sin(x)


#2--Execute the following lines which create two vectors of random integers
#which are chosen with replacement from the integers 0, 1, : : : ,999. 
#Both vectors have length 250
set.seed(100) # what is this??? think!!
x = sample (0:999, 250, replace=T) 
y = sample (0:999, 250, replace=T) 

#(a) Identify out the values in y which are > 500

y[y>500]     #retunr Values

#(b) Identify the index positions in y of the values which are > 700? 

which(y>700) #return INDEX position

#(c) What are the values in x which are in same index position to 
# the values in y which are > 400?  

which(y>400)
which(x>400)

x[which(which(y>400) %in% which(x>400))] 


#(d) How many numbers in x are divisible by 2? 

sum(x%%2==0)

#(e) Sort the numbers in the vector x in the order of increasing values in y. 

sort(x,decreasing = F)

#(f) create the following vector-
#("Label 1", "Label 2", ....., "Label 30")
#*Note that there is a single space between label and the number

vlabel=c(1:30)
vlabel
paste("label",vlabel,sep=" ")

#(g)Generate the following matrix
#it should have 5 rows and 4 cols
# values in r1- 1to 5, r2- 101 to 105, r3-201-205 , r4-301-305

r1=1:5
r2=101:105
r3=201:205
r4=301:305
mymatrix=matrix(c(r1,r2,r3,r4),nrow=5,byrow=T)
mymatrix

#(3)Create a 6 by 10 matrix of random integers chosen from
#1 to 10 by executing the following two lines of code: 
set.seed(100) 
GMAT=matrix(sample(10, size=60, replace=T), nr=6) 
GMAT
#(a) Find the number of entries in each row which are greater than 4.

#(b) Which rows contain exactly two occurrences of the number seven?


#(c) Find those pairs of columns whose total (over both columns)
#is >= 50. The answer should be a matrix with two columns. 


#(4)generate 10000 random numbers from a Uniform distribution between the values 100 and 200.
#plot a histogram for them. How shud the histogram look like??

t=runif(10000)*100+100
hist(t)

#(5)generate 10000 random numbers from a Normal distribution with mean 1000 and sd=50.
#plot a histogram for them. How shud the histogram look like??

p=rnorm(10000,1000,50)
hist(p)

#(6) Refer to the questions in the ppt.. ANswer them here--



#feel free to discuss with others in the whatsapp group.








