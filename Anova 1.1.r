# Load necessary libraries
library(readxl)
library(dplyr)

# Read the data from the Excel file
dataset <- read_excel("anova1.xlsx")

# Print the first few rows to understand the structure
print(head(dataset))

# Calculate the grand mean
grand_mean <- mean(dataset$x_bar, na.rm = TRUE)
print(paste("Grand Mean:", grand_mean))

# Calculate the variance within samples (mean of the variances)
variance_within <- mean(dataset$s^2, na.rm = TRUE)
print(paste("Variance Within Samples:", variance_within))

# Calculate the variance between samples
variance_between <- sum(dataset$n * (dataset$x_bar - grand_mean)^2) / (length(dataset$n) - 1)
print(paste("Variance Between Samples:", variance_between))

# Calculate the F statistic
F_statistic <- variance_between / variance_within
print(paste("F Statistic:", F_statistic))

# Degrees of freedom
df1 <- length(dataset$n) - 1
df2 <- sum(dataset$n) - length(dataset$n)
print(paste("Degrees of Freedom Between (df1):", df1))
print(paste("Degrees of Freedom Within (df2):", df2))

# Find the F critical value at 0.05 significance level
alpha <- 0.05
F_critical <- qf(1 - alpha, df1, df2)
print(paste("F Critical Value at 0.05 significance level:", F_critical))

# Decision Rule
if (F_statistic < F_critical) {
  print("Fail to reject the null hypothesis")
} else {
  print("Reject the null hypothesis")
}
