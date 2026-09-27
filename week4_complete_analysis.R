# Week 4 Comprehensive Data Analysis using R
# Self-contained: uses datasets::airquality and base R only.
# Run from R Console: source("week4_complete_analysis.R")
data(airquality, package = "datasets")
raw <- airquality
raw$MonthName <- factor(raw$Month, levels = 5:9,
                        labels = c("May", "June", "July", "August", "September"))
raw$Date <- as.Date(sprintf("1973-%02d-%02d", raw$Month, raw$Day))
cat("Raw dimensions: 153 rows x 6 original columns\n")
cat("Missing measurements:\n")
print(colSums(is.na(raw[c("Ozone", "Solar.R", "Wind", "Temp")])))
cat("Duplicate Month-Day pairs:", sum(duplicated(raw[c("Month", "Day")])), "\n")

# Week 1: flag outliers from observed data, retain them, and impute by month.
iqr_bounds <- function(x) {
  q <- quantile(x, c(.25, .75), na.rm = TRUE)
  c(q[1] - 1.5 * IQR(x, na.rm = TRUE), q[2] + 1.5 * IQR(x, na.rm = TRUE))
}
for (nm in c("Ozone", "Solar.R", "Wind", "Temp")) {
  bounds <- iqr_bounds(raw[[nm]])
  cat("Observed IQR outlier count for", nm, ":",
      sum(raw[[nm]] < bounds[1] | raw[[nm]] > bounds[2], na.rm = TRUE), "\n")
}
clean <- raw
for (nm in c("Ozone", "Solar.R")) {
  for (month in levels(raw$MonthName)) {
    missing <- which(raw$MonthName == month & is.na(raw[[nm]]))
    clean[[nm]][missing] <- median(raw[[nm]][raw$MonthName == month], na.rm = TRUE)
  }
}
stopifnot(!anyNA(clean))
clean$OzoneOutlier <- clean$Ozone > iqr_bounds(raw$Ozone)[2]
measure <- c("Ozone", "Solar.R", "Wind", "Temp")
for (nm in measure) {
  x <- clean[[nm]]
  clean[[paste0(nm, "_scaled")]] <- (x - min(x)) / (max(x) - min(x))
}
month_dummies <- as.data.frame(model.matrix(~ MonthName - 1, data = clean))
analysis_ready <- cbind(clean, month_dummies)
write.csv(analysis_ready, "week4_analysis_ready.csv", row.names = FALSE)
cat("Missing after cleaning:", sum(is.na(clean)), "\n")

# Week 2: interpret statistics from the original observed readings.
cat("Observed summary:\n"); print(summary(raw[measure]))
month_mean <- tapply(raw$Ozone, raw$MonthName, mean, na.rm = TRUE)
month_count <- tapply(!is.na(raw$Ozone), raw$MonthName, sum)
cat("Observed monthly ozone means and sample counts:\n")
print(round(month_mean, 2)); print(month_count)
cat("Observed ozone-temperature correlation:",
    round(cor(raw$Ozone, raw$Temp, use = "complete.obs"), 3), "\n")
png("week4_monthly_ozone.png", width = 1200, height = 760, res = 150)
bp <- barplot(month_mean, col = "#3678A5", ylim = c(0, 75),
              main = "Observed mean ozone by month", ylab = "Ozone (ppb)")
text(bp, month_mean + 3, paste0("n=", month_count))
dev.off()
png("week4_ozone_temperature.png", width = 1200, height = 760, res = 150)
plot(raw$Temp, raw$Ozone, pch = 19, col = "#3678A5",
     main = "Observed ozone and temperature",
     xlab = "Maximum temperature (F)", ylab = "Ozone (ppb)")
abline(lm(Ozone ~ Temp, data = raw), col = "#C04B40", lwd = 2)
dev.off()

# Week 3: inferential test uses measured ozone-temperature pairs.
pair <- raw[complete.cases(raw[c("Ozone", "Temp")]), ]
cat("Pearson correlation hypothesis test:\n")
print(cor.test(pair$Ozone, pair$Temp, method = "pearson"))

# Prediction uses original complete cases; imputed target is never used.
cases <- raw[complete.cases(raw[c("Ozone", "Temp", "Wind", "Solar.R")]), ]
rownames(cases) <- NULL
test_idx <- seq(5, nrow(cases), by = 5)
train <- cases[-test_idx, ]
test <- cases[test_idx, ]
fit <- lm(Ozone ~ Temp + Wind + Solar.R, data = train)
cat("Model summary:\n"); print(summary(fit))
pred <- predict(fit, newdata = test)
metrics <- function(actual, predicted) {
  e <- actual - predicted
  c(MAE = mean(abs(e)), RMSE = sqrt(mean(e^2)),
    R2 = 1 - sum(e^2) / sum((actual - mean(actual))^2))
}
cat("Train metrics:\n"); print(round(metrics(train$Ozone, fitted(fit)), 3))
cat("Test metrics:\n"); print(round(metrics(test$Ozone, pred), 3))
cat("Test mean-only baseline:\n")
print(round(metrics(test$Ozone, rep(mean(train$Ozone), nrow(test))), 3))

# Validation folds are formed only within training observations.
cv_pred <- rep(NA_real_, nrow(train))
for (fold in 0:4) {
  val <- which(seq_len(nrow(train)) %% 5 == fold)
  cv_fit <- lm(Ozone ~ Temp + Wind + Solar.R, data = train[-val, ])
  cv_pred[val] <- predict(cv_fit, newdata = train[val, ])
}
cat("Five-fold train-only validation:\n")
print(round(metrics(train$Ozone, cv_pred), 3))
cat("Shapiro-Wilk on training residuals:\n")
print(shapiro.test(residuals(fit)))
png("week4_test_predictions.png", width = 1200, height = 760, res = 150)
plot(test$Ozone, pred, pch = 19, col = "#3678A5",
     xlab = "Measured ozone (ppb)", ylab = "Predicted ozone (ppb)",
     main = "Held-out test predictions")
abline(0, 1, col = "#C04B40", lwd = 2, lty = 2)
dev.off()
png("week4_residuals.png", width = 1200, height = 760, res = 150)
plot(fitted(fit), residuals(fit), pch = 19, col = "#3678A5",
     xlab = "Fitted ozone (ppb)", ylab = "Training residual (ppb)",
     main = "Residuals versus fitted values")
abline(h = 0, col = "#C04B40", lwd = 2, lty = 2)
dev.off()
png("week4_qq_plot.png", width = 1200, height = 760, res = 150)
qqnorm(residuals(fit), main = "Training residual Q-Q plot")
qqline(residuals(fit), col = "#C04B40", lwd = 2)
dev.off()
write.csv(data.frame(Measured = test$Ozone, Predicted = pred,
                     Error = test$Ozone - pred),
          "week4_test_predictions.csv", row.names = FALSE)
cat("Completed. Check the five PNG charts and two CSV outputs in getwd().\n")
