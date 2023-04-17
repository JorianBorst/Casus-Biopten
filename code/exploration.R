
################################### LOAD DATA ###################################
# load the breast-cancer-wisconsin dataset
setwd("C:/Users/Jorian/Documents/GitHub/Casus-Biopten/code")
# install data explorer package
#install.packages("DataExplorer")
#install.packages("pandoc")
#install.packages("httpgd")
#install.packages("psych")
# load the mlbench package
library(mlbench)
library(dplyr)
library(DataExplorer)
library(ggplot2)
library(httpgd)
library(psych)
# load the breast cancer package
data(BreastCancer)
# load the ggplot2 package

# show the meta data of BreastCancer
str(BreastCancer)

# Change range of mitoses to 10
BreastCancer <- na.omit(BreastCancer)

dup_rows <- sum(duplicated(BreastCancer))
BreastCancer <- BreastCancer[!dup_rows,]



###################################report###################################
# Create a data quality report for the BreastCancer dataset
report <- create_report(BreastCancer)
# show the report
print(report)

summary(BreastCancer[, 2:11])

describe(BreastCancer[, 2:11])

#Calculate the quantiles of the Cl.thickness variable
BreastCancer$Cl.thickness <- as.numeric(BreastCancer$Cl.thickness)
quantile(BreastCancer$Cl.thickness, na.rm = TRUE)
#Calculate the quantiles of the Cell.size variable
BreastCancer$Cell.size <- as.numeric(BreastCancer$Cell.size)
quantile(BreastCancer$Cell.size, na.rm = TRUE)
#Calculate the quantiles of the Cell.shape variable
BreastCancer$Cell.shape <- as.numeric(BreastCancer$Cell.shape)
quantile(BreastCancer$Cell.shape, na.rm = TRUE)
#Calculate the quantiles of the Marg.adhesion variable
BreastCancer$Marg.adhesion <- as.numeric(BreastCancer$Marg.adhesion)
quantile(BreastCancer$Marg.adhesion, na.rm = TRUE)
#Calculate the quantiles of the Epith.c.size variable
BreastCancer$Epith.c.size <- as.numeric(BreastCancer$Epith.c.size)
quantile(BreastCancer$Epith.c.size, na.rm = TRUE)
#Calculate the quantiles of the Bare.nuclei variable
BreastCancer$Bare.nuclei <- as.numeric(BreastCancer$Bare.nuclei)
quantile(BreastCancer$Bare.nuclei, na.rm = TRUE)
#Calculate the quantiles of the Bl.cromatin variable
BreastCancer$Bl.cromatin <- as.numeric(BreastCancer$Bl.cromatin)
quantile(BreastCancer$Bl.cromatin, na.rm = TRUE)
#Calculate the quantiles of the Nucleoli variable
BreastCancer$Normal.nucleoli <- as.numeric(BreastCancer$Normal.nucleoli)
quantile(BreastCancer$Normal.nucleoli, na.rm = TRUE)
#Calculate the quantiles of the Mitoses variable
BreastCancer$Mitoses <- as.numeric(BreastCancer$Mitoses)
quantile(BreastCancer$Mitoses, na.rm = TRUE)

# Count the number of non-missing values in the column
count <- sum(!is.na(BreastCancer$Class))

# Calculate the number of missing values in the column
missing <- sum(is.na(BreastCancer$Class))

# Calculate the cardinality of the column
cardinality <- length(unique(BreastCancer$Class, na.rm = TRUE))

# Calculate the mode and mode frequency of the column
mode <- names(sort(table(BreastCancer$Class, exclude = NA), decreasing = TRUE))[1]
mode_freq <- max(table(BreastCancer$Class, exclude = NA))

# Calculate the mode percentage of the column
mode_perc <- mode_freq / count * 100

# Calculate the second mode and second mode frequency of the column
second_mode <- names(sort(table(BreastCancer$Class, exclude = NA), decreasing = TRUE))[2]
second_mode_freq <- sort(table(BreastCancer$Class, exclude = NA), decreasing = TRUE)[2]
second_mode_perc <- second_mode_freq / count * 100
# Print the results
cat("Count: ", count, "\n")
cat("Missing: ", missing, "\n")
cat("Cardinality: ", cardinality, "\n")
cat("Mode: ", mode, "\n")
cat("Mode Frequency: ", mode_freq, "\n")
cat("Mode Percentage: ", mode_perc, "%\n")
cat("Second Mode: ", second_mode, "\n")
cat("Second Mode Frequency: ", second_mode_freq, "\n")
cat("Second Mode Percentage: ", second_mode_perc, "%\n")

################################### EXPLORATION ###################################

# Create 2 dataframes with the data of the 2 classes
benignCancer <- subset(BreastCancer, Class = "benign")
malignentCancer <- subset(BreastCancer, Class = "Maligant")

#create a t-test to see if there is a difference between the two groups
#t.test(benignCancer$Cell.size, malignentCancer$Cell.size)

# create a boxplot for cell size for the malignant and benign cancer
boxplot(BreastCancer$Cell.size ~ BreastCancer$Class, main = "Cell.size", xlab = "Class", ylab = "Value", col = "blue")
# create a boxplot for cromatin for the malignant and benign cancer
boxplot(BreastCancer$Chromatin ~ BreastCancer$Class, main = "Chromatin", xlab = "Class", ylab = "Value", col = "blue")
# create a boxplot for thickness for the malignant and benign cancer
boxplot(BreastCancer$Cl.thickness ~ BreastCancer$Class, main = "Cl.thickness", xlab = "Class", ylab = "Value", col = "blue")

# create a scatterplot matrix for all features for the malignant and benign cancer
pairs(BreastCancer[c("Cell.size", "Cl.thickness", "Cell.shape", "Marg.adhesion",
                     "Epith.c.size", "Bare.nuclei", "Bl.cromatin", "Normal.nucleoli",
                     "Mitoses")], main = "Scatterplot matrix", col = "blue")
######################################### Correlation and covalence #########################################
BreastCancer$Cell.size <- as.numeric(BreastCancer$Cell.size)
BreastCancer$Cl.thickness <- as.numeric(BreastCancer$Cl.thickness)
BreastCancer$Cell.shape <- as.numeric(BreastCancer$Cell.shape)
BreastCancer$Marg.adhesion <- as.numeric(BreastCancer$Marg.adhesion)
BreastCancer$Epith.c.size <- as.numeric(BreastCancer$Epith.c.size)
BreastCancer$Bare.nuclei <- as.numeric(BreastCancer$Bare.nuclei)
BreastCancer$Bl.cromatin <- as.numeric(BreastCancer$Bl.cromatin)
BreastCancer$Normal.nucleoli <- as.numeric(BreastCancer$Normal.nucleoli)
BreastCancer$Mitoses <- as.numeric(BreastCancer$Mitoses)
# Compute the correlation coefficient
cor(BreastCancer$Cell.size, BreastCancer$Cl.thickness)

cor(BreastCancer$Cell.size, BreastCancer$Cell.shape)

# Laad de corrplot package (indien nodig)
#install.packages("corrplot")
library(corrplot)

# Bereken de correlatiematrix
corr_matrix <- cor(BreastCancer[c("Cell.size", "Cl.thickness", "Cell.shape", "Marg.adhesion",
                                  "Epith.c.size", "Bare.nuclei", "Bl.cromatin", "Normal.nucleoli",
                                  "Mitoses")])
print(corr_matrix)

######################################### Data preparation #########################################
# Remove the missing values
BreastCancer <- na.omit(BreastCancer)

# Remove the outliers
BreastCancer <- BreastCancer[BreastCancer$Cell.size < 10,]
BreastCancer <- BreastCancer[BreastCancer$Cl.thickness < 10,]
BreastCancer <- BreastCancer[BreastCancer$Cell.shape < 10,]
BreastCancer <- BreastCancer[BreastCancer$Marg.adhesion < 10,]
BreastCancer <- BreastCancer[BreastCancer$Epith.c.size < 10,]
BreastCancer <- BreastCancer[BreastCancer$Bare.nuclei < 10,]
BreastCancer <- BreastCancer[BreastCancer$Bl.cromatin < 10,]
BreastCancer <- BreastCancer[BreastCancer$Normal.nucleoli < 10,]
BreastCancer <- BreastCancer[BreastCancer$Mitoses < 10,]

# Remove the ID column
BreastCancer <- BreastCancer[, -1]

# Remove the Class column
BreastCancer <- BreastCancer[, -10]

# Create a new column with the class
BreastCancer$Class <- as.factor(BreastCancer$Class)


