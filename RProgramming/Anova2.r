# Load necessary libraries
library(readxl)
library(dplyr)

# Read the data from the Excel files
dataset_top <- read_excel("anova2_top.xlsx")
dataset_bottom <- read_excel("anova2_bottom.xlsx")

# Calculate the grand mean
grand_meanTOP <- mean(dataset_top$x_bar, na.rm = TRUE)
grand_meanBOTTOM <- mean(dataset_bottom$x_bar, na.rm = TRUE)

# Calculate the variance within samples (mean of the variances)
variance_withinTOP <- mean(dataset_top$s^2, na.rm = TRUE)
variance_withinBOTTOM <- mean(dataset_bottom$s^2, na.rm = TRUE)

# Calculate the variance between samples
variance_betweenTOP <- sum(dataset_top$n * (dataset_top$x_bar - grand_meanTOP)^2) / (length(dataset_top$n) - 1)
variance_betweenBOTTOM <- sum(dataset_bottom$n * (dataset_bottom$x_bar - grand_meanBOTTOM)^2) / (length(dataset_bottom$n) - 1)

# Calculate the F statistic
F_top <- variance_betweenTOP / variance_withinTOP
F_bottom <- variance_betweenBOTTOM / variance_withinBOTTOM

# Degrees of freedom
df1_top <- length(dataset_top$n) - 1
df2_top <- sum(dataset_top$n) - length(dataset_top$n)
df1_bottom <- length(dataset_bottom$n) - 1
df2_bottom <- sum(dataset_bottom$n) - length(dataset_bottom$n)

# Find the F critical value at 0.05 significance level
alpha <- 0.05
F_critical_top <- qf(1 - alpha, df1_top, df2_top)
F_critical_bottom <- qf(1 - alpha, df1_bottom, df2_bottom)

# Print the results
print(paste("F Statistic for top 300 songs:", F_top))
print(paste("F Critical Value for top 300 at 0.05 significance level:", F_critical_top))
print(ifelse(F_top < F_critical_top, "Fail to reject the null hypothesis for top 300 songs", "Reject the null hypothesis for top 300 songs"))

print(paste("F Statistic for bottom 300:", F_bottom))
print(paste("F Critical Value for bottom 300 at 0.05 significance level:", F_critical_bottom))
print(ifelse(F_bottom < F_critical_bottom, "Fail to reject the null hypothesis for bottom 300 songs", "Reject the null hypothesis for bottom 300 songs"))
