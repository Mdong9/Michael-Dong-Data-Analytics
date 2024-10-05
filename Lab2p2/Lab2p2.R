library("e1071")

#insta

iris

iris.df <- iris
#?naiveBayes

classifier <- naiveBayes(iris[,1:4], iris[,5])

classifier

prediction <- predict(classifier, iris[,-5])

prediction

table(prediction, iris[,5], dnn=list('predicted','actual'))

classifier$apriori

classifier$tables$Petal.Length

plot(function(x) dnorm(x, 1.462, 0.1736640), 0, 8, col="red", main="Petal length distribution for the 3 different species") 

curve(dnorm(x, 4.260, 0.4699110), add=TRUE, col="blue") 

curve(dnorm(x, 5.552, 0.5518947 ), add=TRUE, col = "green")

library(rpart)

library(rpart.plot)

dim(iris)

s_iris <- sample(150,100)

s_iris

iris_train <-iris[s_iris,]

iris_test <-iris[-s_iris,] 

dim(iris_test)
dim(iris_train) 

?rpart

dectionTreeModel <- rpart(Species~., iris_train, method = "class") 

dectionTreeModel

rpart.plot(dectionTreeModel) 


############################
### Abelone ####

abalone <- read.csv("abalone.data", header = FALSE, sep = ",")
abalone
colnames(abalone) <- c("sex", "length", 'diameter', 'height', 'whole_weight', 'shucked_wieght', 'viscera_wieght', 'shell_weight', 'rings' )

abalone

abalone$age.group <- cut(abalone$rings, br=c(0,8,11,35), labels = c("young", 'adult', 'old')) 

abalone.norm <- abalone[,-1]


#####################
classifier <- naiveBayes(abalone[,2:4], abalone[,10])

classifier

prediction <- predict(classifier, abalone[,2:4])

prediction

table(prediction, abalone[,10], dnn=list('predicted','actual'))

classifier$apriori

classifier$tables$length
plot(function(x) dnorm(x, 0.4209915, 0.11137474), xlim=c(0, 1), ylim=c(0,5), col="red", main="Abalone length distribution [red=young, blue=adult, green=old]")
curve(dnorm(x, 0.5707182, 0.08740980), add=TRUE, col="blue") 
curve(dnorm(x, 0.5868542, 0.08100644), add=TRUE, col = "green")

classifier$tables$diameter
plot(function(x) dnorm(x, 0.3212758, 0.09029187), xlim=c(0, .8), ylim=c(0,7), col="red", main="Abalone diameter distribution [red=young, blue=adult, green=old]")
curve(dnorm(x, 0.4458591, 0.07153798), add=TRUE, col="blue") 
curve(dnorm(x, 0.4632083, 0.06699741), add=TRUE, col = "green")

classifier$tables$height
plot(function(x) dnorm(x, 0.1065956, 0.04183039), xlim=c(0, .3), ylim=c(0,15), col="red", main="Abalone diameter distribution [red=young, blue=adult, green=old]")
curve(dnorm(x, 0.1516906, 0.02984784), add=TRUE, col="blue") 
curve(dnorm(x, 0.1648125, 0.02935998), add=TRUE, col = "green")
######################
classifier2 <- naiveBayes(abalone[,4:6], abalone[,10])

classifier2

prediction2 <- predict(classifier2, abalone[,4:6])

prediction2

table(prediction2, abalone[,10], dnn=list('predicted','actual'))
######################
classifier3 <- naiveBayes(abalone[,6:8], abalone[,10])

classifier3

prediction3 <- predict(classifier3, abalone[,6:8])

prediction3

table(prediction3, abalone[,10], dnn=list('predicted','actual'))
######################
#z <- abalone
#aba <- abalone
#aba$sex <- NULL 


normalize <- function(x) {return ((x - min(x)) / (max(x) - min(x))) }

aba[1:7] <- as.data.frame(lapply(aba[1:7], normalize))

summary(aba$shucked_wieght)

ind <- sample(2, nrow(aba), replace=TRUE, prob=c(0.7, 0.3))
KNNtrain <- aba[ind==1,]
KNNtest <- aba[ind==2,]
sqrt(2918)

library(class)

KNNpred <- knn(train = KNNtrain[1:7], test = KNNtest[1:7], cl = KNNtrain$rings, k = 55)

KNNpred
table(KNNpred,KNNtest$rings)

