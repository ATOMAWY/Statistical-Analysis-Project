# Reset environment
rm(list = ls())

# Load data
df <- read.csv("Sleep_health_and_lifestyle_dataset.csv")

# Extract Stress Level column
stress <- df$Stress.Level

# Summary statistics
n <- length(stress)
mean_val <- mean(stress)
median_val <- median(stress)
min_val <- min(stress)
max_val <- max(stress)
range_val <- max_val - min_val
Q1 <- quantile(stress, 0.25)
Q3 <- quantile(stress, 0.75)
IQR_val <- IQR(stress)
var_val <- var(stress)
sd_val <- sd(stress)

summary_stats <- data.frame(
  n = n,
  mean = mean_val,
  median = median_val,
  min = min_val,
  max = max_val,
  range = range_val,
  Q1 = Q1,
  Q3 = Q3,
  IQR = IQR_val,
  variance = var_val,
  sd = sd_val
)

summary_stats

# Frequency table (counts of each stress level)
table(stress)

# Bar plot for discrete stress levels
barplot(
  table(stress),
  main = "Bar Plot of Stress Levels",
  xlab = "Stress Level (1–10)",
  ylab = "Frequency",
  col = "lightblue",
  border = "black",
  axes = FALSE            # turn off default axes
)

axis(2)                   # add the y-axis (normal)
abline(h = 0)             # add the x-axis baseline

# Boxplot
boxplot(
  stress,
  main = "Boxplot of Stress Levels",
  ylab = "Stress Level (1–10)",
  col = "lightgreen"
)

# Dotplot
dotchart(
  stress,
  main = "Dotplot of Stress Levels",
  xlab = "Stress Level (1–10)"
)

# Outliers
outliers <- boxplot.stats(stress)$out
outliers
