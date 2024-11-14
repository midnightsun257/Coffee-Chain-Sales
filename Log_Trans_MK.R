# Install necessary packages if not already installed
if (!require(MASS)) install.packages("MASS")
if (!require(e1071)) install.packages("e1071")
if (!require(caret)) install.packages("caret")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(dplyr)) install.packages("dplyr")
if (!require(corrplot)) install.packages("corrplot")

# Load necessary libraries
library(MASS)
library(e1071)
library(caret)
library(ggplot2)
library(dplyr)
library(corrplot)

# Load dataset
dataset <- read.csv("Updated_Coffee_Chain_Sales.csv")
data <- dataset  # Copy for transformations to maintain original data

# Initial Summary Statistics
summary(data)

# Check for Missing Values
missing_values <- colSums(is.na(data))
print("Missing Values Check:")
print(missing_values)

# Check the skewness of the "Profit" variable
original_skewness <- skewness(dataset$Profit, na.rm = TRUE)
print(paste("Original Skewness of Profit:", original_skewness))

# Replace negative or NA values in Profit with a small positive value
dataset$Profit[is.na(dataset$Profit) | dataset$Profit <= 0] <- 1e-5

# Apply Various Transformations to "Profit" Variable
# Log Transformation (adding 1 to avoid log(0) issues)
dataset$Log_Profit <- log(dataset$Profit + 1)

# Square Root Transformation
dataset$Sqrt_Profit <- sqrt(dataset$Profit)

# Box-Cox Transformation
boxcox_result <- boxcox(lm(Profit ~ 1, data = dataset), plotit = FALSE)
lambda_boxcox <- boxcox_result$x[which.max(boxcox_result$y)]
dataset$BoxCox_Profit <- ((dataset$Profit ^ lambda_boxcox) - 1) / lambda_boxcox

# Yeo-Johnson Transformation
pre_process_model <- preProcess(as.data.frame(dataset$Profit), method = "YeoJohnson")
dataset$YeoJohnson_Profit <- predict(pre_process_model, as.data.frame(dataset$Profit))
dataset$YeoJohnson_Profit <- as.numeric(dataset$YeoJohnson_Profit[, 1])

# Visualization of Original and Transformed Distributions
par(mfrow = c(2, 2)) # Arrange plots in a 2x2 layout
hist(dataset$Log_Profit, main = "Log Transformed Profit", col = "blue", breaks = 30)
hist(dataset$Sqrt_Profit, main = "Square Root Transformed Profit", col = "green", breaks = 30)
hist(dataset$BoxCox_Profit, main = "Box-Cox Transformed Profit", col = "red", breaks = 30)
hist(dataset$YeoJohnson_Profit, main = "Yeo-Johnson Transformed Profit", col = "purple", breaks = 30)

# Skewness Comparison
log_skewness <- skewness(dataset$Log_Profit)
sqrt_skewness <- skewness(dataset$Sqrt_Profit)
boxcox_skewness <- skewness(dataset$BoxCox_Profit)
yeojohnson_skewness <- skewness(dataset$YeoJohnson_Profit)

transformed_skewness <- data.frame(
  Transformation = c("Original", "Log", "Square Root", "Box-Cox", "Yeo-Johnson"),
  Skewness = c(original_skewness, log_skewness, sqrt_skewness, boxcox_skewness, yeojohnson_skewness)
)
print("Skewness Comparison of Original and Transformed Variables:")
print(transformed_skewness)

# Additional EDA and Correlation Analysis on Updated Dataset
# Correlation Matrix (before transformations)
# Clear the plotting window
dev.off()  

# Generate Correlation Matrix and Plot
numeric_data <- data %>% select_if(is.numeric)
corr_matrix <- cor(numeric_data, use = "complete.obs")

# Adjust text size with tl.cex
corrplot(corr_matrix, method = "color", title = "Correlation Matrix (Before Transformations)", tl.cex = 0.8)


# Outlier Detection: Boxplot of Profit
ggplot(data, aes(x = Profit)) +
  geom_boxplot() +
  labs(title = "Boxplot of Profit (Outlier Detection)")

# Apply Log Transformations to Key Variables and Plot Distributions
# Define a function to apply log transformation and plot
log_transform_and_plot <- function(df, var_name) {
  # Apply log transformation
  df[[paste0("Log_", var_name)]] <- log(df[[var_name]] + 1)
  
  # Plot original and transformed distributions
  p1 <- ggplot(df, aes_string(x = var_name)) +
    geom_histogram(bins = 30, fill = "skyblue", color = "black") +
    labs(title = paste("Original Distribution of", var_name))
  
  p2 <- ggplot(df, aes_string(x = paste0("Log_", var_name))) +
    geom_histogram(bins = 30, fill = "salmon", color = "black") +
    labs(title = paste("Log-Transformed Distribution of", var_name))
  
  print(p1)
  print(p2)
}

# Apply log transformations to key skewed variables in `data`
data$Log_Profit <- log(data$Profit + 1)
data$Log_Sales <- log(data$Sales + 1)
data$Log_Total_expenses <- log(data$Total_expenses + 1)
#data$Log_Cogs <- log(data$Cogs + 1)


# Skewness Comparison (before and after log transformations)
skewness_comparison <- data.frame(
  Variable = c("Profit", "Sales", "Total_expenses", "Cogs"),
  Original_Skewness = sapply(data[c("Profit", "Sales", "Total_expenses", "Cogs")], function(x) skewness(x, na.rm = TRUE)),
  Log_Skewness = sapply(data[c("Log_Profit", "Log_Sales", "Log_Total_expenses", "Log_Cogs")], function(x) skewness(x, na.rm = TRUE))
)

# Print the skewness comparison to review
print("Skewness Comparison (Before and After Log Transformations):")
print(skewness_comparison)

# Correlation Matrix (after transformations)
transformed_data <- data %>% select(starts_with("Log_"))
transformed_corr_matrix <- cor(transformed_data, use = "complete.obs")
corrplot(transformed_corr_matrix, method = "color", title = "Correlation Matrix (After Transformations)")

# Save updated dataset with transformations
write.csv(data, "Updated_Coffee_Chain_Sales_with_Log_Transforms.csv", row.names = FALSE)

# Save to a specific location (e.g., Desktop)
write.csv(data, "~/Desktop/Updated_Coffee_Chain_Sales_with_Log_Transforms.csv", row.names = FALSE)


#hist(data$Log_Marketing, main = "Log-Transformed Marketing Variable", col = "lightgreen", breaks = 30)



