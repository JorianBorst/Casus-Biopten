
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

# Create a boxplot for the Cl.thickness variable
boxplot(BreastCancer$Cl.thickness, main = "Cl.thickness", xlab = "Cl.thickness", ylab = "Value", col = "blue")

# Create 2 dataframes with the data of the 2 classes
benignCancer <- subset(BreastCancer, Class = "benign")
# Createa a plot for the cell density and cromatin variable
#ggplot(benignCancer, aes(x = Cell.density, y = Chromatin)) + geom_point()

malignentCancer <- subset(BreastCancer, Class = "Maligant")

#create a t-test to see if there is a difference between the two groups
t.test(benignCancer$Cell.size, malignentCancer$Cell.size)