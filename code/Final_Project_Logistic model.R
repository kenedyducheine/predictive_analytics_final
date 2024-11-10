#load packages
library('caret')
library('stats')
#load dataset
data <- read.csv("final_project_dataset.csv")

#inspect dataset
head(data)
summary(data)

#Drop uneeded columns
data <- data[,-c(1:6)]

#make predicted varible factor
data$status_of_system <- as.factor(data$status_of_system)

#split for training and test using 60/40 split
set.seed(1)
train.index <- sample(c(1:5369), 3221)
train.set <- data[train.index,]
valid.set <- data[-train.index,]

##Build logisitic model utilizng Caret##

#set train control for 10 K-fold validation

train.control <- trainControl(
  method = 'cv',
  number = 10, #K-Fold 10
  summaryFunction = twoClassSummary,
  classProbs = TRUE,
  verboseIter = TRUE
)

#set model
model <- train(
  status_of_system ~., #test all variables
  train.set,
  method = 'glm',
  trControl = train.control
)

#see results
summary(model)

#build confusion matrix to assess accuracy
predicted_values <- predict(model, valid.set)

#make labels align
predicted_values <- factor(predicted_values, levels = levels(valid.set$status_of_system))
true_labels <- factor(valid.set$status_of_system, levels = levels(predicted_values))

con_matrix <- confusionMatrix(predicted_values, valid.set$status_of_system)
con_matrix
