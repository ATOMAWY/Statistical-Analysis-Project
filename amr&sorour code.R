# Reset environment
rm(list = ls())

# Load data
df <- read.csv("Sleep_health_and_lifestyle_dataset.csv")

# Extract the Sleep Duration column
sleep <- df$Sleep.Duration

# Summary statistics
n <- length(sleep)
mean_val <- mean(sleep)
median_val <- median(sleep)
min_val <- min(sleep)
max_val <- max(sleep)
range_val <- max_val - min_val
Q1 <- quantile(sleep, 0.25)
Q3 <- quantile(sleep, 0.75)
IQR_val <- IQR(sleep)
var_val <- var(sleep)
sd_val <- sd(sleep)

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

# Histogram
hist(sleep,
     main = "Histogram of Sleep Duration",
     xlab = "Sleep Duration (hours)",
     col = "lightblue",
     border = "black")

# Boxplot
boxplot(sleep,
        main = "Boxplot of Sleep Duration",
        ylab = "Sleep Duration (hours)",
        col = "lightgreen")

# Dotplot
dotchart(sleep,
         main = "Dotplot of Sleep Duration",
         xlab = "Sleep Duration (hours)")

# Outliers
outliers <- boxplot.stats(sleep)$out
outliers

