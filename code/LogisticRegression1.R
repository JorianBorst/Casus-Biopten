library(mlbench)
data("BreastCancer")
#head(BreastCancer)
#str(BreastCancer)

BreastCancer <- na.omit(BreastCancer)

dup_rows <- sum(duplicated(BreastCancer))
BreastCancer <- BreastCancer[!dup_rows,]


# Remove the ID column
BreastCancer <- BreastCancer[, -1]

# Remove the Class column
BreastCancer <- BreastCancer[, -10]

# Create a new column with the class
BreastCancer$Class <- as.factor(BreastCancer$Class)



set.seed(123)
train <- sample(nrow(BreastCancer), 0.7 * nrow(BreastCancer))
train_data <- BreastCancer[train,]
test_data <- BreastCancer[-train,]
model <- glm(Class ~ Cl.thickness + Cell.size + Cell.shape + Marg.adhesion + Epith.c.size + Bare.nuclei + Bl.cromatin + Normal.nucleoli
+ Mitoses, data = train_data, family = binomial)
summary(model)

predicted <- predict(model, newdata = test_data, type = "response")
predicted_class <- ifelse(predicted > 0.5, "M", "B")
actual_class <- test_data$Class
confusion_matrix <- table(predicted_class, actual_class)
confusion_matrix



features <- BreastCancer[, c("Cl.thickness", "Cell.shape", "Marg.adhesion", "Epith.c.size", "Bare.nuclei", "Bl.cromatin", "Normal.nucleoli", "Mitoses")]

target <- BreastCancer$Class


features <- BreastCancer[, c("Cl.thickness", "Cell.shape", "Marg.adhesion", "Epith.c.size", "Bare.nuclei", "Bl.cromatin", "Normal.nucleoli", "Mitoses")]
target <- BreastCancer$Class

set.seed(123)
indices <- sample(nrow(features))
train_indices <- indices[1:round(0.6 * nrow(features))]
valid_indices <- indices[(round(0.6 * nrow(features)) + 1):round(0.8 * nrow(features))]
test_indices <- indices[(round(0.8 * nrow(features)) + 1):nrow(features)]

train_features <- features[train_indices,]
valid_features <- features[valid_indices,]
test_features <- features[test_indices,]

train_set <- data.frame(train_features, target[train_indices])
valid_set <- data.frame(valid_features, target[valid_indices])
test_set <- data.frame(test_features, target[test_indices])