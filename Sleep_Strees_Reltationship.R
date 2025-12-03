relationship_sleep_stress <- function() {
  source("SleepDuration_XVariable_Calculations.R")
  source("StressLevels_YVariable_Calculations.R")
  
  # Get the data
  sleep <- get_sleep_data()$values
  stress <- get_stress_data()$values
  
  # Clean data: remove any NA rows just in case
  complete <- complete.cases(sleep, stress)
  sleep <- sleep[complete]
  stress <- stress[complete]
  
  # --- 1. Scatter Plot ---
  plot(
    sleep,
    stress,
    main = "Scatter Plot: Sleep / Stress",
    xlab = "Sleep Duration (hours)",
    ylab = "Stress Level",
    pch = 19,
    col = rgb(0, 0, 1, 0.2)
  )
  
  # Fit linear model
  model <- lm(stress ~ sleep)
  abline(model, col = "red", lwd = 2)
  
  # --- 2. Describe Scatter ---
  direction <- ifelse(coef(model)[2] > 0, "positive", "negative")
  
  r <- cor(sleep, stress)
  strength <- ifelse(abs(r) < 0.3, "weak",
                     ifelse(abs(r) < 0.6, "moderate", "strong"))
  
  # Unusual points: standardized residual > 2
  res <- rstandard(model)
  unusual_idx <- which(abs(res) > 2)
  
  cat("\n--- Scatterplot Description ---\n")
  cat("Direction:", direction, "\n")
  cat("Strength:", strength, "\n")
  
  if(length(unusual_idx) > 0) {
    cat("Unusual points (x, y):\n")
    for(i in unusual_idx) {
      cat("{", sleep[i], ",", stress[i], "}\n")
    }
  } else {
    cat("No clearly unusual points detected.\n")
  }
  
  # --- 3. Correlation ---
  cat("\n--- Correlation ---\n")
  cat("r =", round(r, 3), "\n")
  cat("Interpretation: There is a", strength, direction, "linear relationship between sleep and stress.\n")
  
  # --- 4. Simple Linear Regression ---
  summary_model <- summary(model)
  B0 <- coef(model)[1]
  B1 <- coef(model)[2]
  R2 <- summary_model$r.squared
  
  cat("\n--- Simple Linear Regression ---\n")
  cat("Intercept (B0):", round(B0, 3), "\n")
  cat("Slope (B1):", round(B1, 3), "\n")
  cat("R-squared:", round(R2, 3), "\n")
  
  cat("\nInterpretations:\n")
  cat("- Slope: For each additional hour of sleep, predicted stress changes by", round(B1,3), "units.\n")
  cat("- R²:", round(R2*100,1), "% of variation in stress is explained by sleep duration.\n")
}

relationship_sleep_stress()
