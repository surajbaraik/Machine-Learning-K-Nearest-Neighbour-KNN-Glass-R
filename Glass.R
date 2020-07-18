

install.packages("caret")
library(caret)
install.packages("Rtools")
library(Rtools)
install.packages("lattice")
library(lattice)
install.packages("ggplot2")
library(ggplot2)
install.packages("e1071")
library("e1071")
install.packages("class")
library("class")

#load the dataset
glass <- read.csv(file.choose())
View(glass)

#table of glass type
table(glass$Type)

# table or proportation of enteries in the datasets. What % of entry is Bengin and % of entry is Malignant
round(prop.table(table(glass$Type))*100,1)
summary(glass[c("Al","Na","Mg")])

#Create a function to normalize the data
norm <- function(x){ 
  return((x-min(x))/(max(x)-min(x)))
}
#test normalization
norm(c(1,2,3,4,5))
norm(c(10,20,30,40,50))

#Apply the normalization function to glass dataset
glass_norm <- as.data.frame(lapply(glass[1:9], norm))
View(glass_norm)
anyNA(glass_norm)
#there is no NA values

glass$Type <- as.factor(glass$Type)
anyNA(glass$Type)
#there is no NA values

#create training and test datasets using sequential splitting
glass_train <- glass_norm[1:150,]
glass_test <- glass_norm[151:214,]

#Get labels for training and test datasets

glass_train_labels <- glass[1:150,10]
glass_test_labels <- glass[151:214,10]

anyNA(glass_train)
anyNA(glass_test)
anyNA(glass_train_labels)
na.omit(glass_train_labels)
dim(glass_train)
dim(glass_train)
length(glass_train_labels)

# Build a KNN model on taining dataset
# Building the KNN model on training dataset and also need labels which we are including c1. Once we build the preduction model
# we have to test on test dataset
glass_pred <- knn(train=glass_train,test=glass_test,cl=glass_train_labels,k=1)
glass_pred
glass_test_labels

#Error in prediction
error <- mean(glass_pred!=glass_test_labels)

table(glass_pred,glass_test_labels)

#Confusion Matrix
confusionMatrix(glass_pred,glass_test_labels)

#By summary of confusion matrix for k=1 we are getting very low accuracy that 0.03
#so we need to check for optimum k value

# iterations of for k values
glass_pred <- NULL
error.rate <- NULL

for (i in 1:10) {
  glass_pred <- knn(glass_train,glass_test,glass_train_labels,k=i)
  error.rate[i] <- mean(glass_pred!=glass_test_labels)
  
}

knn.error <- as.data.frame(cbind(k=1:10,error.type =error.rate))

#plotting k value vs error choose k value
ggplot(knn.error,aes(k,error.type))+ 
  geom_point()+ 
  geom_line() + 
  scale_x_continuous(breaks=1:10)+ 
  theme_bw() +
  xlab("Value of K") +
  ylab('Error')
#From plot k=1 is optimum value but we getting least accuracy as we have taken it earlier so we will check it with k=3 second optimum value

#For k=3
glass_pred <- knn(train=glass_train,test=glass_test,cl=glass_train_labels,k=3)
glass_pred
glass_test_labels

#Error in prediction
error <- mean(glass_pred!=glass_test_labels)

table(glass_pred,glass_test_labels)

#Confusion Matrix
confusionMatrix(glass_pred,glass_test_labels)

#from summary we are getting accuracy as 0 for second optimum value so we can conclude from this that sampling of data is not good
#we did sequention splitting in which there are only 1,2,3 type are there and not others
#so my model is not able to relate further types of test with which in train data
# we need to go for random sampling to get results

install.packages('caTools')   #for splitting data
library(caTools)
install.packages('dplyr')    #for Data Manipulation
library(dplyr)

#normalizing data
standard.features <- scale(glass[,1:9])

#Join the standardized data with the target column
data <- cbind(standard.features,glass[10])
#Check if there are any missing values to impute. 
anyNA(data)

#splitting data randomly by considering glass type as splitting parameter with 70:30 ratio
set.seed(101)

sample <- sample.split(data$Type,SplitRatio = 0.70)

train <- subset(data,sample==TRUE)

test <- subset(data,sample==FALSE)

#KNN model buildingg with random sampling 
glass_pred_rnd <- knn(train[1:9],test[1:9],train$Type,k=1)

#Error in prediction
error <- mean(glass_pred_rnd!=test$Type)
#Confusion Matrix
confusionMatrix(glass_pred_rnd,test$Type)
#from summary accuracy has improved to 0.70 it means this sampling is relevant for prediction
# now we have to check for accuracy with optimum value k

# iterations of for k values
glass_pred_rnd <- NULL
error.rate <- NULL

for (i in 1:10) {
  glass_pred_rnd <- knn(train[1:9],test[1:9],train$Type,k=i)
  error.rate[i] <- mean(glass_pred_rnd!=test$Type)
  
}

knn.error1 <- as.data.frame(cbind(k=1:10,error.type =error.rate))


ggplot(knn.error1,aes(k,error.type))+ 
  geom_point()+ 
  geom_line() + 
  scale_x_continuous(breaks=1:10)+ 
  theme_bw() +
  xlab("Value of K") +
  ylab('Error')

#From plot we can say that k=1 is optimum value but we will check with k=3


#KNN model buildingg with random sampling 
glass_pred_rnd <- knn(train[1:9],test[1:9],train$Type,k=3)

#Error in prediction
error <- mean(glass_pred_rnd!=test$Type)
#Confusion Matrix
confusionMatrix(glass_pred_rnd,test$Type)
# From summary accuracy is dipping to 0.6 so it good to go with k=1 with accuracy 0.7