#############################
#   STATISTICAL ANALYSIS    #
#  Sleep Duration (X) and   #
#   Stress Level (Y)        #
#############################

# Clear environment
rm(list = ls())

# Load dataset
df <- read.csv("Sleep_health_and_lifestyle_dataset.csv")

#################################################
# 1) X VARIABLE — SLEEP DURATION ANALYSIS (X)   #
#################################################

analyze_sleep <- function(df) {
  
  sleep <- df$Sleep.Duration
  
  # Summary statistics
  stats <- list(
    n = length(sleep),
    mean = mean(sleep),
    median = median(sleep),
    min = min(sleep),
    max = max(sleep),
    range = max(sleep) - min(sleep),
    Q1 = quantile(sleep, 0.25),
    Q3 = quantile(sleep, 0.75),
    IQR = IQR(sleep),
    variance = var(sleep),
    sd = sd(sleep)
  )
  
  # ---- PRINT CLEAN OUTPUT ----
  cat("\n==============================\n")
  cat(" X VARIABLE: SLEEP DURATION\n")
  cat("==============================\n")
  
  cat("n:", stats$n, "\n")
  cat("Mean:", stats$mean, "\n")
  cat("Median:", stats$median, "\n")
  cat("Min:", stats$min, "\n")
  cat("Max:", stats$max, "\n")
  cat("Range:", stats$range, "\n")
  cat("Q1:", stats$Q1, "\n")
  cat("Q3:", stats$Q3, "\n")
  cat("IQR:", stats$IQR, "\n")
  cat("Variance:", stats$variance, "\n")
  cat("SD:", stats$sd, "\n")
  
  # Outliers
  outliers <- boxplot.stats(sleep)$out
  cat("\nSleep Duration Outliers:\n")
  print(outliers)
  
  # ---- GRAPHS ----
  
  # Histogram
  hist(
    sleep,
    main = "Histogram of Sleep Duration",
    xlab = "Sleep Duration (hours)",
    col = "lightblue",
    border = "black"
  )
  
  # Boxplot
  boxplot(
    sleep,
    main = "Boxplot of Sleep Duration",
    ylab = "Sleep Duration (hours)",
    col = "lightgreen"
  )
  
  # Dotplot
  dotchart(
    sleep,
    main = "Dotplot of Sleep Duration",
    xlab = "Sleep Duration (hours)"
  )
  
  return(list(values = sleep, stats = stats, outliers = outliers))
}

#################################################
# 2) Y VARIABLE — STRESS LEVEL ANALYSIS (Y)     #
#################################################

analyze_stress <- function(df) {
  
  stress <- df$Stress.Level
  
  # Summary statistics
  stats <- list(
    n = length(stress),
    mean = mean(stress),
    median = median(stress),
    min = min(stress),
    max = max(stress),
    range = max(stress) - min(stress),
    Q1 = quantile(stress, 0.25),
    Q3 = quantile(stress, 0.75),
    IQR = IQR(stress),
    variance = var(stress),
    sd = sd(stress)
  )
  
  # ---- PRINT CLEAN OUTPUT ----
  cat("\n==============================\n")
  cat(" Y VARIABLE: STRESS LEVEL\n")
  cat("==============================\n")
  
  cat("n:", stats$n, "\n")
  cat("Mean:", stats$mean, "\n")
  cat("Median:", stats$median, "\n")
  cat("Min:", stats$min, "\n")
  cat("Max:", stats$max, "\n")
  cat("Range:", stats$range, "\n")
  cat("Q1:", stats$Q1, "\n")
  cat("Q3:", stats$Q3, "\n")
  cat("IQR:", stats$IQR, "\n")
  cat("Variance:", stats$variance, "\n")
  cat("SD:", stats$sd, "\n")
  
  # Frequency table
  cat("\nStress Level Frequency Table:\n")
  print(table(stress))
  
  # Outliers
  outliers <- boxplot.stats(stress)$out
  cat("\nStress Level Outliers:\n")
  print(outliers)
  
  # ---- GRAPHS ----
  
  # Bar plot
  barplot(
    table(stress),
    main = "Bar Plot of Stress Levels",
    xlab = "Stress Level (1–10)",
    ylab = "Frequency",
    col = "lightblue",
    border = "black"
  )
  
  # Boxplot
  boxplot(
    stress,
    main = "Boxplot of Stress Levels",
    ylab = "Stress Level (1–10)",
    col = "lightgreen"
  )
  
  # Dot plot
  dotchart(
    stress,
    main = "Dotplot of Stress Levels",
    xlab = "Stress Level (1–10)"
  )
  
  return(list(values = stress, stats = stats, outliers = outliers))
}

############################################################
# 3) RELATIONSHIP BETWEEN X (SLEEP) AND Y (STRESS)         #
############################################################

analyze_relationship <- function(sleep, stress) {
  
  complete <- complete.cases(sleep, stress)
  sleep <- sleep[complete]
  stress <- stress[complete]
  
  # --- SCATTER PLOT ---
  plot(
    sleep,
    stress,
    main = "Scatter Plot: Sleep vs Stress",
    xlab = "Sleep Duration (hours)",
    ylab = "Stress Level (1–10)",
    pch = 19,
    col = rgb(0, 0, 1, 0.2)
  )
  
  model <- lm(stress ~ sleep)
  abline(model, col = "red", lwd = 2)
  
  # --- DIRECTION ---
  direction <- ifelse(coef(model)[2] > 0, "positive", "negative")
  
  # --- CORRELATION ---
  r <- cor(sleep, stress)
  strength <- ifelse(
    abs(r) < 0.3, "weak",
    ifelse(abs(r) < 0.6, "moderate", "strong")
  )
  
  # --- UNUSUAL POINTS ---
  res <- rstandard(model)
  unusual_idx <- which(abs(res) > 2)
  
  # ---- PRINT CLEAN OUTPUT ----
  cat("\n==============================\n")
  cat(" RELATIONSHIP: SLEEP vs STRESS\n")
  cat("==============================\n")
  
  cat("\n--- Scatter Plot Description ---\n")
  cat("Direction:", direction, "\n")
  cat("Strength:", strength, "\n")
  
  if (length(unusual_idx) > 0) {
    cat("Unusual points (sleep, stress):\n")
    for (i in unusual_idx) {
      cat(" {", sleep[i], ",", stress[i], "}\n")
    }
  } else {
    cat("No clearly unusual points detected.\n")
  }
  
  # --- Correlation ---
  cat("\n--- Correlation ---\n")
  cat("r =", round(r, 3), "\n")
  cat("Interpretation: There is a", strength, direction, "linear relationship.\n")
  
  # --- Regression ---
  summary_model <- summary(model)
  B0 <- coef(model)[1]
  B1 <- coef(model)[2]
  R2 <- summary_model$r.squared
  
  cat("\n--- Simple Linear Regression ---\n")
  cat("Intercept (B0):", round(B0, 3), "\n")
  cat("Slope (B1):", round(B1, 3), "\n")
  cat("R-squared:", round(R2, 3), "\n")
  
  cat("\nInterpretations:\n")
  cat("- Slope: For each additional hour of sleep, predicted stress changes by", round(B1, 3), "units.\n")
  cat("- R²:", round(R2 * 100, 1), "% of variation in stress is explained by sleep duration.\n")
}

###################################
#      RUN ALL ANALYSES           #
###################################

sleep_result <- analyze_sleep(df)
stress_result <- analyze_stress(df)

analyze_relationship(
  sleep_result$values,
  stress_result$values
)
