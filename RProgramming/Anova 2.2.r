# Load necessary libraries if not loaded already
library(ggplot2)
library(dplyr)

# Read the data (adjust the file path as per your actual CSV file location)
data <- read.csv("/Users/raudhahmaszamanie/Downloads/anova1_spotify.csv")

anova$key <- as.factor(anova$key)

# ANOVA TOP AND BOTTOM 300
top_300 <- data.frame(head(anova, 300))
bottom_300 <- data.frame(tail(anova, 300))

# Group by 'key' and summarize statistics
summary_stats_top <- top_300 %>%
  group_by(key) %>%
  summarize(
    n = n(),                         # Number of observations per key
    streams_mean = mean(streams, na.rm = TRUE),  # Mean number of streams per key
    sd_streams = sd(streams, na.rm = TRUE)       # Standard deviation of number of streams per key
  )

# Print the summary statistics
print(summary_stats_top)

# Group by 'key' and summarize statistics
summary_stats_bottom <- bottom_300 %>%
  group_by(key) %>%
  summarize(
    n = n(),                         # Number of observations per key
    streams_mean = mean(streams, na.rm = TRUE),  # Mean number of streams per key
    sd_streams = sd(streams, na.rm = TRUE)       # Standard deviation of number of streams per key
  )

# Print the summary statistics
print(summary_stats_bottom)

# Calculate overall mean for top 300 and bottom 300
overall_mean_top <- mean(top_300$streams, na.rm = TRUE)
overall_mean_bottom <- mean(bottom_300$streams, na.rm = TRUE)

# Calculate overall variance for top 300 and bottom 300
overall_var_top <- var(top_300$streams, na.rm = TRUE)
overall_var_bottom <- var(bottom_300$streams, na.rm = TRUE)

# Calculate between-sample variance
between_var <- ((overall_mean_top - overall_mean_bottom)^2) / 2

# Calculate within-sample variance (average of variances within top and bottom)
within_var <- (overall_var_top + overall_var_bottom) / 2

# Calculate mean between samples
mean_between_samples <- (overall_mean_top + overall_mean_bottom) / 2

# Calculate standard deviation between samples
sd_between_samples <- sqrt(between_var)

# Print mean between samples, standard deviation between samples, variance between samples, variance within samples
cat("Mean Between Samples:", mean_between_samples, "\n")
cat("Standard Deviation Between Samples:", sd_between_samples, "\n")
cat("Variance Between Samples:", between_var, "\n")
cat("Variance Within Samples:", within_var, "\n")

# Add sample_type variable to identify top and bottom 300 samples
top_300$sample_type <- "top"
bottom_300$sample_type <- "bottom"

# Combine top and bottom 300 into one data frame
top_bottom_combined <- rbind(top_300, bottom_300)

# Perform ANOVA
anova_result <- aov(streams ~ key * sample_type, data = top_bottom_combined)

# Summary of ANOVA
summary(anova_result)

# Print the first few rows to understand the structure
print(head(anova))

# Print the column names to identify the actual names
print(colnames(anova))

# Select and rename the columns
data <- anova %>%
  select(key, streams) %>%
  rename(key = key, streams = streams)

# Convert 'key' column to a factor and ensure 'NA' is treated as a level
data$key <- factor(anova$key, levels = c("A", "A#", "B", "C#", "D", "D#", "E", "F",
                                         "F#", "G", "G#", "NA"))
pastel_palette <- c("#FFCDB2", "#FFD966", "#9EE09E", "#C1E1C1", "#A6E3D7", 
                    "#87CEEB", "#FFABAB", "#FFC3A0", "#FFD966", "#9EE09E", 
                    "#C1E1C1", "#7B9EA8")

ggplot(anova, aes(x = key, y = streams, color = key)) +
  geom_point(size = 1, alpha = 0.8) +  # Adjust size and transparency
  labs(title = "Streams by Keys",
       x = "Keys",
       y = "Streams") +
  scale_color_manual(values = pastel_palette) +  # Apply pastel color palette
  theme_minimal() +
  theme(legend.position = "none")









