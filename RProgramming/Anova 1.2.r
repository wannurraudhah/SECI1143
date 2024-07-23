# Load necessary libraries
library(ggplot2)
library(dplyr)

# Read the data
data <- read.csv("/Users/raudhahmaszamanie/Downloads/anova1_spotify.csv",
                 stringsAsFactors = FALSE)

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

# Define the 12 keys (corrected to use 'keys' instead of 'key')
keys <- c("A", "A#", "B", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "NA")

# Calculate the mean (x̄), standard deviation (s), and count (n) for each key
results <- anova %>%
  filter(key %in% keys) %>%
  group_by(key) %>%
  summarise(
    n = n(),  # Count number of observations (n)
    x_bar = mean(streams, na.rm = TRUE),
    s = sd(streams, na.rm = TRUE)
  )

print(results)

# Check if there are any rows in results
if (nrow(results) == 0) {
  print("No data found after filtering. Check your data and filtering logic.")
} else {
  # Save the results to a CSV file
  write.csv(results, "key_statistics.csv", row.names = FALSE)
}

# Perform one-way ANOVA
anova_result <- anova(lm(streams ~ key, data = data))

# Print ANOVA table
print(anova_result)

