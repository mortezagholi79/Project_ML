#7.2
#Read Data
Customers <-read.csv("~/UniversalBank.csv",header = T)
str(Customers)

#need to exclude columns ID and Zip
Customers.df<-Customers[,c(-1,-5)]
#Remaining variables
names(Customers.df)


# as.factor
Customers.df$Personal.Loan<-as.factor(Customers.df$Personal.Loan)
Customers.df$Education<-as.factor(Customers.df$Education)
Customers.df$Family<-as.factor(Customers.df$Family)
Customers.df$Securities.Account<-as.factor(Customers.df$Securities.Account)
Customers.df$CD.Account<-as.factor(Customers.df$CD.Account)
Customers.df$CreditCard<-as.factor(Customers.df$CreditCard)
Customers.df$Online<-as.factor(Customers.df$Online)


set.seed(123)
# 60/40 split for training and validation
library(caret)
trainIndex <- createDataPartition(Customers.df$Personal.Loan, p=.6,list = FALSE, times = 1)
train.df <- Customers.df[trainIndex,] 
valid.df <- Customers.df[-trainIndex,] 



# use preProcess() from the caret package to normalize Income and Lot_Size.
norm.values <- preProcess(train.df, method=c("center", "scale"))
train.norm.df <- as.data.frame(predict(norm.values, train.df))
valid.norm.df <- as.data.frame(predict(norm.values, valid.df))
Customers.norm.df <- as.data.frame(predict(norm.values, Customers.df))





train.knn.predictors<-train.norm.df[,-8]
train.knn.target<-train.df$Personal.Loan

valid.knn.predictors<-valid.norm.df[,-8]
valid.knn.target<-valid.df$Personal.Loan





################## A #################
new.df<-data.frame(Age = 40 , Experience = 10 ,Income = 84 ,Family = 2 ,CCAvg = 2,Education = 2,Mortgage = 0 ,Securities.Account = 0
                   ,CD.Account = 0,Online = 1,CreditCard = 1)

#norm your new data
new.norm.values <- preProcess(new.df, method=c("center", "scale"))

new.norm.df <- predict(new.norm.values, newdata = new.df)

preds.k.1 <- class::knn (train=train.norm.df[,-8], test=new.norm.df, cl=train.df$Personal.Loan, k=1, prob=TRUE)
head(preds.k.1)



########################### B ################################

# Accuracy.df
# initialize a data frame with two columns: k, and accuracy.
accuracy.df <- data.frame(k = seq(1, 20, 1), accuracy = rep(0, 20))
# compute knn for different k on validation.
for(i in 1:20) {
  knn.pred<-class::knn (train=train.norm.df[,-8], test=valid.norm.df[,-8], cl=train.df$Personal.Loan, k=i, prob=TRUE)
  accuracy.df[i, 2] <- confusionMatrix(knn.pred, valid.df$Personal.Loan)$overall[1]
}

accuracy.df

which.max(accuracy.df$accuracy)

#################   C   ############################
preds.k.3 <- class::knn (train=train.norm.df[,-8], test=new.norm.df, cl=train.df$Personal.Loan, k=3, prob=TRUE)
head(preds.k.3)


############################# 7.2.5########################
Customers2 <-read.csv("~/UniversalBank.csv",header = T)
##need to exclude columns ID and Zip
Customers2<-Customers2[,c(-1,-5)]

# as.factor
Customers2$Personal.Loan<-as.factor(Customers2$Personal.Loan)
Customers2$Education<-as.factor(Customers2$Education)
Customers2$Family<-as.factor(Customers2$Family)
Customers2$Securities.Account<-as.factor(Customers2$Securities.Account)
Customers2$CD.Account<-as.factor(Customers2$CD.Account)
Customers2$CreditCard<-as.factor(Customers2$CreditCard)
Customers2$Online<-as.factor(Customers2$Online)

trainIndex <- createDataPartition(Customers2$Personal.Loan, p=.500,list = FALSE, times = 1)
Customers.test <- Customers2[trainIndex,] 
Customers1 <- Customers2[-trainIndex,] 

trainIndex1<-createDataPartition(Customers1$Personal.Loan, p=.600,list = FALSE, times = 1)
Customers.train <- Customers1[trainIndex1,] 
Customers.valid <- Customers1[-trainIndex1,] 

norm.test <- preProcess(Customers.test, method=c("center", "scale"))


Customers.trainnorm.target<-Customers.train$Personal.Loan
Customers.trainnorm<-predict(norm.test,Customers.train[, -8])

test.knn <- cbind(Customers.testnorm.z, Customers.test$Personal.Loan)
test.knn.predictors <- test.knn[, 1:11]
test.knn.target <- test.knn[,12]


#confusionMatrix
preds.k <- class::knn (train=Customers.trainnorm, test=test.knn.predictors, cl=Customers.trainnorm.target, k=1, prob=TRUE)
confusionMatrix(preds.k, test.knn.target, positive="1")
