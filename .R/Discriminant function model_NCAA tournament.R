ncaa=read.table("ncaa.txt", header=TRUE)
x=as.matrix(ncaa[4:12])
y1 = 1:32
y1 = y1*0+1
y2 = y1*0
y = c(y1,y2)
library(MASS)
#Linear Discriminant Analysis (LDA)
#Runs LDA with response y (the group: top 32 teams = 1, bottom 32 = 0) and predictors x.

#Here:
#y → factor variable (class label: 1 or 0).

#x → predictors (matrix or data.frame of performance stats).

#Stores the result in object dm.

dm=lda(y~x)
#top32 teams are category 1(y=1)and the bottom 32 teams are category 2(y=0)
#Printing it will give:
  
#Group means

#Number of observations in each group

#Prior probabilities
lda(y~x)

#Stored in result for later analysis.
result=lda(y~x)

#Shows prior probabilities of the groups (by default proportional to group sizes).
result$prior


#Group means of predictors (average stats for group 0 vs group 1).

#Helps to see which variables differ most between groups.
result$means
result$call

#Total number of observations used
result$N

#Singular value decomposition level
result$svd

#Gives the predicted class (0 or 1) for each observation.
predict(result)$class

#Value of the predicted normalized discriminant function
predict(result)

#The cutoff is treated as being at zero.
