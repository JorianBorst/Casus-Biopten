library(mlbench)
library(dplyr)
library(DataExplorer)
library(ggplot2)
library(httpgd)
library(psych)
library(caret)
library(corrplot)
library(caTools)

data(BreastCancer)
str(BreastCancer)
# remove the missing values
BreastCancer <- na.omit(BreastCancer)


# remove the ID column
BreastCancer <- BreastCancer[, -1]

#############

set.seed(123)

split <- sample.split(BreastCancer$Class, SplitRatio = 0.7)
train <- subset(BreastCancer, split == TRUE)
test <- subset(BreastCancer, split == FALSE)

########################################################Logistic Regression

# Create a logistic regression model
logisitcal_model <- glm(Class ~., data = train, family = binomial)

# Print the summary of the model
summary(logisitcal_model)

# Create a confusion matrix
predicted_classes <- ifelse(predict(logisitcal_model, newdata = test, type = "response") > 0.5, "Malignant", "Benign")
actual_classes <- test$Class
table(predicted_classes, actual_classes)



# create a logistic regression model with the Cell.size and Cl.thickness
logistical_model_1 <- glm(Class ~ 
  Cell.size +
    Epith.c.size +
     Bare.nuclei +
       Normal.nucleoli 
        , data = train, family = binomial)

# Print the summary of the model

predicted_classes <- ifelse(predict(logistical_model_1, newdata = test, type = "response") > 0.5, "Malignant", "Benign")
actual_classes <- test$Class
table(predicted_classes, actual_classes)

