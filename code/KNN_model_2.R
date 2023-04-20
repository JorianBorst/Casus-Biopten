library(class)

# Load the dataset
data("BreastCancer")

# Remove rows with missing values
BreastCancer <- BreastCancer[complete.cases(BreastCancer),]

# Convert the Class variable to a factor
BreastCancer$Class <- as.factor(BreastCancer$Class)

#convert the class to a numeric variable
#BreastCancer$Class <- as.numeric(BreastCancer$Class)

# remove the ID column
BreastCancer <- BreastCancer[, -1]

# Convert the target variable to a factor
#BreastCancer$Class <- as.factor(BreastCancer$Class)



set.seed(123)

# Split data into k folds
k <- 5
folds <- cut(seq(1, nrow(BreastCancer)), breaks = k, labels = FALSE)

# Initialize accuracy vector
accuracy <- rep(0, k)

# Perform k-fold cross-validation
for (i in 1:k) {
  # Split data into training and testing sets
  test_indices <- which(folds == i, arr.ind = TRUE)
  test_set <- BreastCancer[test_indices, ]
  train_set <- BreastCancer[-test_indices, ]

  # Train k-NN model
  k_value <- 57
  model <- knn(train = train_set[, -1], test = test_set[, -1], cl = train_set$Class, k = k_value)

  # Compute accuracy of model
  predictions <- factor(model, levels = c("2", "4"))
  actual <- test_set$Class
  accuracy[i] <- sum(predictions == actual) / length(actual)
}

# Average accuracy over all folds
mean_accuracy <- mean(accuracy)
print(mean_accuracy)

##########################################
library(caret)
library(e1071)

set.seed(123)

# Specify the training control
trainControl <- trainControl(method = "cv",  # k-fold cross-validation
                     number = 10,   # number of folds
                     classProbs = TRUE, # compute class probabilities
                     summaryFunction = twoClassSummary) # compute accuracy metric
# Create the kNN model
knn_model <- train(Class ~ ., data = train, method = "knn", trControl = trainControl(method = "cv"), tuneLength = 10)

# Print the model results
print(knn_model)
