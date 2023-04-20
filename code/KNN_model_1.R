library(caret)
set.seed(123)

# Load the dataset
data("BreastCancer")

# Remove rows with missing values
BreastCancer <- BreastCancer[complete.cases(BreastCancer),]

# Convert the target variable to a factor
BreastCancer$Class <- as.factor(BreastCancer$Class)

# remove the ID column
BreastCancer <- BreastCancer[, -1]

# Split the data into training and test sets
set.seed(123)
split <- sample.split(BreastCancer$Class, SplitRatio = 0.7)
train <- subset(BreastCancer, split == TRUE)
test <- subset(BreastCancer, split == FALSE)

# Create the kNN model
knn_model <- train(Class ~   Cell.size +
    Epith.c.size +
     Bare.nuclei +
       Normal.nucleoli , data = train, method = "knn", trControl = trainControl(method = "cv"), tuneLength = 5)

# Make predictions on the test set
knn_predictions <- predict(knn_model, newdata = test)

# Calculate the accuracy of the model
accuracy <- mean(knn_predictions == test$Class)
cat(paste("Accuracy:", accuracy))


############################################## K folding ###########################

# Load the mlbench package
library(mlbench)
library(class)
library(caret)

# Load the BreastCancer dataset
data(BreastCancer)

# Remove rows with missing values
BreastCancer <- BreastCancer[complete.cases(BreastCancer),]

# Convert the Class variable to a factor
#BreastCancer$Class <- as.factor(BreastCancer$Class)

#convert the class to a numeric variable
BreastCancer$Class <- as.numeric(BreastCancer$Class)

# remove the ID column
BreastCancer <- BreastCancer[, -1]

# Split the data into training and testing sets

set.seed(321)
train_index <- sample(1:nrow(BreastCancer), size = 0.7 * nrow(BreastCancer))
train_data <- BreastCancer[train_index, -1]
train_labels <- BreastCancer[train_index, 1]
valid_data <- BreastCancer[-train_index, -1]
valid_labels <- BreastCancer[-train_index, 1]

# Try different values of k and evaluate their performance on the validation set
k_values <- c(1:150)
accuracy <- numeric(length(k_values))
for (i in 1:length(k_values)) {
    
  knn_pred <- knn(train_data, valid_data, train_labels, k = k_values[i])
  accuracy[i] <- sum(knn_pred == valid_labels) / length(valid_labels)
}

# Find the value of k that performs the best on the validation set
best_k <- k_values[which.max(accuracy)]
print(paste0("Best value of k: ", best_k))
