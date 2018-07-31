#setting the directory and uploading the dataset

setwd("~/Data Science Projects/Football Team")
dataset <- read.csv('FootballTeam.csv')

#checking to str of the dataset

str(dataset)

#HT is being recognized as a factor because of the way the data was entered (i.e. "06-01"). As such, it is necessary to dissect the strings and provide a calculation to convert to inches

dataset$HT <- as.character(dataset$HT)
dataset$HTinches <- as.numeric(substr(dataset$HT,1,2))*12 + as.numeric(substr(dataset$HT, 4, 5))
dataset$HT <- dataset$HTinches
dataset <- dataset[,-9]


#previewing a summary of the data

summary(dataset)
avgbyPOS <- aggregate(.~POS, data = dataset, FUN = 'mean')


#checking to see if the necessary package for the predictive model is installed and doing so if it is not.

if(!require(randomForest)){
  install.packages("randomForest")
  library(randomForest)
}

#dividing the dataset into training and test partitions

n <- length(dataset[,1])
n1 <- floor(n*(.75))
n2 <- n - n1
train <- sample(1:n, n1)
training <- dataset[train,]
test <- dataset[-train,]

#training the model using a random forest classifier using 100 iterations

set.seed(123)
model1 <- randomForest(x = training[,-1], y = training[,1], ntree = 100)

#predicting outcomes on the test data partition

y_pred <- predict(model1, newdata = test)

#confusion matrix

cm <- table(y_pred, test[,1])

#a data frame comparing the outcomes of the predictions and actual values

compare <- as.data.frame(cbind(y_pred, test[,1]))

#a data frame that includes only the incorrect results

incorrect <- compare[which(compare[,1] != compare[,2]),]
completeincorrect <- cbind(incorrect, dataset[as.numeric(rownames(incorrect)),2:8])

#predictive accuracy of our model on the test set

accuracy <- (length(test[,1]) - length(incorrect[,1])) / length(test[,1])