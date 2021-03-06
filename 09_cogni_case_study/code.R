install.packages("DMwR")
install.packages("data.table")

library(DMwR)
library(data.table)

setwd("D:/11_github/StudyMaterial/05_case_study")

data <- fread("german.data-numeric.txt", sep = " ")

final_data <- data[,c(1:20,25)]

setnames(final_data,"V25","Def")

final_data[final_data$Def==1]$Def <- 0

final_data[final_data$Def==2]$Def <- 1

creditdata <- as.data.frame(final_data)

normalization <- function(data,x){
  for(j in x){
    data[!(is.na(data[,j])),j]=(data[!(is.na(data[,j])),j]-min(data[!(is.na(data[,j])),j]))/
  (max(data[!(is.na(data[,j])),j])-min(data[!(is.na(data[,j])),j]))
    }
  return(data)
  } 


c <- c(2,5,8,11,13,16,18) 

normdata <- normalization(creditdata,c) 

boxplot(normdata[,c]) 


levels(as.factor(creditdata[,"V1"])) 

head(creditdata)

require(cluster)
distance=daisy(creditdata[,-19],stand=TRUE,metric=c("gower"), type =
                 list(interval=c(2,5,8,11,13,16,18), nominal=c(1,3,4,6,7,9,10,12,14,15,17),binary=c(19,20)))

#Output Will be like mentioned below

#> data
#A B
#1 1 5
#2 2 6
#3 3 7
#4 4 8

#> distance2
#Dissimilarities :
#  1         2         3
#2 0.3333333                    
#3 0.6666667 0.3333333          
#4 1.0000000 0.6666667 0.3333333
#
#Metric :  mixed ;  Types = I, I 
#Number of objects : 4


require(DMwR)
outlierdata=outliers.ranking(distance,test.data=NULL,method="sizeDiff",clus = list(dist="euclidean",alg = "hclust", meth="average"), power = 1, verb = F) 

#OutPut Will be like as
#> data
#A B
#1 1 5
#2 2 6
#3 3 7
#4 4 8

boxplot(outlierdata$prob.outliers[outlierdata$rank.outliers])
n=quantile(outlierdata$rank.outliers) 
n1=n[1]
n4=n[4]
filler=(outlierdata$rank.outlier > n4*1.3)
length(which(filler))
 
creditdata_noout=creditdata[!filler,]
nrow(creditdata_noout) 

require(DMwR)
creditdata_noout_noimp=knnImputation(creditdata_noout, k = 5, scale = T, meth = "weighAvg",
                                     distData = NULL)

library(DMwR)
set.seed(454) 
split<-sample(nrow(creditdata_noout_noimp), round(nrow(creditdata_noout_noimp)*0.8))
trainingdata=creditdata_noout_noimp[split,]
testdata=creditdata_noout_noimp[-split,] 

creditdata_noout_noimp_train=trainingdata
creditdata_noout_noimp_train$default <- factor(ifelse(creditdata_noout_noimp_train$Def == 1, "def",
                                                      "nondef"))

creditdata_noout_noimp_test=testdata
creditdata_noout_noimp_test$default <- factor(ifelse(creditdata_noout_noimp_test$Def == 1, "def",
                                                      "nondef"))

#form
#A formula describing the prediction problem
#data
#A data frame containing the original (unbalanced) data set
#perc.over
#A number that drives the decision of how many extra cases from the minority class are generated (known as over-sampling).
#k
#A number indicating the number of nearest neighbours that are used to generate the new examples of the minority class.
#perc.under
#A number that drives the decision of how many extra cases from the majority classes are selected for each case generated from the minority class (known as under-sampling)
#learner
#Optionally you may specify a string with the name of a function that implements a classification algorithm that will be applied to the resulting SMOTEd data set (defaults to NULL).
#...
#In case you specify a learner (parameter learner) you can indicate further arguments that will be used when calling this learner.

creditdata_noout_noimp_train_smot <- SMOTE(default ~ ., creditdata_noout_noimp_train,
                                           k=5,perc.over = 500)


table(creditdata_noout_noimp_train_smot$default)


library(cluster)
dist1=daisy(creditdata_noout_noimp_train[,-21],stand=TRUE,metric=c("gower"), type =
              list(interval=c(2,5,8,11,13,16,18), nominal=c(1,3,4,6,7,9,10,12,14,15,17),binary=c(19,20)))
dist2=daisy(creditdata_noout_noimp_train_smot[,-21],stand=TRUE,metric=c("gower"), type =
              list(interval=c(2,5,8,11,13,16,18), nominal=c(1,3,4,6,7,9,10,12,14,15,17),binary=c(19,20)))

#Multidimensional scaling takes a set of dissimilarities and returns a set of points such that the distances between the points are approximately equal to the dissimilarities. (It is a major part of what ecologists call 'ordination'.)

loc1=cmdscale(dist1,k=2)

loc2=cmdscale(dist2,k=2)


x1=loc1[,1]
y1=loc1[,2]
x2=loc2[,1]
y2=loc2[,2] 

plot(x1,y1,type="n")
text(x1,y1,labels=creditdata_noout_noimp_train[,22],
     col=as.numeric(creditdata_noout_noimp_train[,22])+4)
plot(x2,y2,type="n")
text(x2,y2,labels=creditdata_noout_noimp_train_smot[,22],
     col=as.numeric(creditdata_noout_noimp_train_smot[,22])+4) 

install.packages("ellipse")

library(package="ellipse")

c= c(2,5,8,11,13,16,18)
plotcorr(cor(creditdata_noout_noimp_train[,c]),col=cl<-c(7,6,3))

c= c(1,3,4,6,7,9,10,12,14,15,17)
plotcorr(cor(creditdata_noout_noimp_train [,c]),col=cl<-c("green","red","blue"))


library(randomForest)
set.seed(454)
data.frame(creditdata_noout_noimp_train)
randf<-randomForest(Def~ ., data=creditdata_noout_noimp_train, ntree=700, importance=TRUE,
                    proximity=TRUE)
importance(randf, type=1, scale=TRUE) 


varImpPlot(randf) 


findopt=rfcv(creditdata_noout_noimp_train[,-21],
             creditdata_noout_noimp_train[,21], cv.fold=10, scale="log", step=0.9)
opt <- which.max(findopt$error.cv)
plot( findopt$n.var, findopt$error.cv, type= "h", main = "Importance", xlab="Number of Features",
      ylab = "Classifier Error Rate")
axis(1, opt, paste("Threshold", opt, sep="\n"), col = "red", col.axis = "red")



library(rpart)
c = c(4, 8, 11, 15, 17, 18, 21)
trdata=data.frame(creditdata_noout_noimp_train[,-c])
tree=rpart(trdata$default~.,data=trdata,method="class")
printcp(tree) 

plot(tree, uniform=TRUE,main="Classification Tree")
text(tree, use.n=TRUE, all=TRUE, cex=0.7)


predicttest=data.frame(creditdata_noout_noimp_test[,c(1:20)])

pred=predict(tree,predicttest)

c=c(22)
table(predict(tree, predicttest, type="class",na.action=na.pass), creditdata_noout_noimp_test[, c])

install.packages("caret")
library(caret)

confusionMatrix(predict(tree, predicttest, type="class",na.action=na.pass), creditdata_noout_noimp_test[, c])
