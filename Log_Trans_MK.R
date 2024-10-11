# Install necessary packages if not already installed
if (!require(MASS)) install.packages("MASS")
if (!require(e1071)) install.packages("e1071")
if (!require(caret)) install.packages("caret")

# Load necessary libraries
library(MASS)
library(e1071)
library(caret)

# Checking the skewness of the "Profit" variable
original_skewness <- skewness(dataset$Profit, na.rm = TRUE)
print(paste("Original Skewness of Profit:", original_skewness))

# Replace negative or NA values in Profit with a small positive value
dataset$Profit[is.na(dataset$Profit) | dataset$Profit <= 0] <- 1e-5

# Now apply the Log Transformation (adding 1 to avoid log(0) issues)
dataset$Log_Profit <- log(dataset$Profit + 1)

# Square Root Transformation
dataset$Sqrt_Profit <- sqrt(dataset$Profit)

# Box-Cox Transformation (Profit is now adjusted to be positive)
boxcox_result <- boxcox(lm(Profit ~ 1, data = dataset), plotit = FALSE)
lambda_boxcox <- boxcox_result$x[which.max(boxcox_result$y)]
dataset$BoxCox_Profit <- ((dataset$Profit ^ lambda_boxcox) - 1) / lambda_boxcox

# Yeo-Johnson Transformation (can handle zero/negative values directly)
pre_process_model <- preProcess(as.data.frame(dataset$Profit), method = "YeoJohnson")
dataset$YeoJohnson_Profit <- predict(pre_process_model, as.data.frame(dataset$Profit))

# Extract the numeric values from the result (it might return a data frame)
dataset$YeoJohnson_Profit <- as.numeric(dataset$YeoJohnson_Profit[, 1])



# Plotting histograms to visualize the transformations
par(mfrow = c(2, 2)) # Arrange plots in a 2x2 layout
hist(dataset$Log_Profit, main = "Log Transformed Profit", col = "blue", breaks = 30)
hist(dataset$Sqrt_Profit, main = "Square Root Transformed Profit", col = "green", breaks = 30)
hist(dataset$BoxCox_Profit, main = "Box-Cox Transformed Profit", col = "red", breaks = 30)
hist(dataset$YeoJohnson_Profit, main = "Yeo-Johnson Transformed Profit", col = "purple", breaks = 30)

# Calculating skewness for the original and transformed variables
log_skewness <- skewness(dataset$Log_Profit)
sqrt_skewness <- skewness(dataset$Sqrt_Profit)
boxcox_skewness <- skewness(dataset$BoxCox_Profit)
yeojohnson_skewness <- skewness(dataset$YeoJohnson_Profit)

# Display skewness of original and transformed variables
transformed_skewness <- data.frame(
  Transformation = c("Original", "Log", "Square Root", "Box-Cox", "Yeo-Johnson"),
  Skewness = c(original_skewness, log_skewness, sqrt_skewness, boxcox_skewness, yeojohnson_skewness)
)
print(transformed_skewness)

