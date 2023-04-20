library(mlbench)
library(dplyr)
library(DataExplorer)
library(ggplot2)
library(httpgd)
library(psych)
library(caret)
library(corrplot)
library(caTools)
library(BSDA)
library(class)
# Load the dataset
data("BreastCancer")

# Remove rows with missing values
BreastCancer <- BreastCancer[complete.cases(BreastCancer),]

# Convert the Class variable to a factor
#BreastCancer$Class <- as.factor(BreastCancer$Class)

#convert the class to a numeric variable
BreastCancer$Class <- as.numeric(BreastCancer$Class)

# remove the ID column
BreastCancer <- BreastCancer[, -1]


BreastCancer$Cl.thickness <- as.numeric(as.character(BreastCancer$Cl.thickness))
BreastCancer$Cell.size <- as.numeric(as.character(BreastCancer$Cell.size))
BreastCancer$Cell.shape <- as.numeric(as.character(BreastCancer$Cell.shape))
BreastCancer$Marg.adhesion <- as.numeric(as.character(BreastCancer$Marg.adhesion))
BreastCancer$Epith.c.size <- as.numeric(as.character(BreastCancer$Epith.c.size))
BreastCancer$Bare.nuclei <- as.numeric(as.character(BreastCancer$Bare.nuclei))
BreastCancer$Bl.cromatin <- as.numeric(as.character(BreastCancer$Bl.cromatin))
BreastCancer$Normal.nucleoli <- as.numeric(as.character(BreastCancer$Normal.nucleoli))
BreastCancer$Mitoses <- as.numeric(as.character(BreastCancer$Mitoses))


set.seed(123)

# Split data in training, validation, en test sets
train_indices <- sample(1:nrow(BreastCancer), round(0.6*nrow(BreastCancer)))
valid_indices <- sample(setdiff(1:nrow(BreastCancer), train_indices), round(0.2*nrow(BreastCancer)))
test_indices <- setdiff(setdiff(1:nrow(BreastCancer), train_indices), valid_indices)
train_set <- BreastCancer[train_indices, ]
valid_set <- BreastCancer[valid_indices, ]
test_set <- BreastCancer[test_indices, ]

# Initialize accuracy vector
accuracy <- rep(0, 20)

# Doe 10-fold cross-validation met een validatie set
for (k in 1:10) {
  # Train k-NN model op training set
  model <- knn(train = train_set[, -1], test = valid_set[, -1], cl = train_set$Class, k = k)

  # bereken accuraatheid van model met validatie set
  predictions <- factor(model, levels = c("1", "2"))
  print(predictions)
  actual <- valid_set$Class
  accuracy[k] <- sum(predictions == actual) / length(actual)
  print(accuracy[k])
}

# Selecteer de beste K waarde
best_k <- which.max(accuracy)
print(best_k)
# Train k-NN model met de beste K waarde voeg ook de training en validatie set samen voor het uiteindelijke trainen
model <- knn(train = rbind(train_set, valid_set)[, -1], test = test_set[, -1], cl = rbind(train_set, valid_set)$Class, k = best_k)

# Bereken de accuraatheid van het model met de test set die nog niet gebruikt
predictions <- factor(model, levels = c("1", "2"))
actual <- test_set$Class
accuracy <- sum(predictions == actual) / length(actual)
print(accuracy)