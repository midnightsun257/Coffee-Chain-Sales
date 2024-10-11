# Load necessary libraries
install.packages("ggplot2")
install.packages("dplyr")
install.packages("corrplot")

library(ggplot2)
library(dplyr)
library(corrplot)

# Load the dataset
dataset <- read.csv('/Users/cdmstudent/Desktop/Coffee_Chain_Sales.csv')

# Summary statistics
summary(dataset)

# Check for missing values
missing_data <- colSums(is.na(dataset))
print(missing_data)

# Correlation analysis (numeric columns only)
numeric_columns <- dataset %>% select_if(is.numeric)
correlation_matrix <- cor(numeric_columns, use = "complete.obs")
corrplot(correlation_matrix, method = "circle")

# Boxplot for outlier detection (Profit)
ggplot(dataset, aes(y = Profit)) +
  geom_boxplot(fill = "orange", color = "black") +
  labs(title = "Boxplot of Profit", y = "Profit")

# Pairwise scatter plots
pairs(numeric_columns)

# Loop through numeric columns to plot histograms
for(col in colnames(numeric_columns)) {
  p <- ggplot(dataset, aes_string(x = col)) +
    geom_histogram(binwidth = 10, color = "black", fill = "blue") +
    labs(title = paste("Distribution of", col), x = col, y = "Count")
  print(p)
}
