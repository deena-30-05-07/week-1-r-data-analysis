# Week 3 Statistical Analysis and Predictive Modeling using R
# Base R only. Run from any working directory: source("week3_model.R")
data(airquality, package = "datasets")
aq <- airquality
cat("Original observations:", nrow(aq), "\n")
print(colSums(is.na(aq[c("Ozone", "Solar.R", "Wind", "Temp")])))

# Descriptive hypothesis test on available ozone-temperature pairs.
# H0: population Pearson correlation = 0; two-sided alternative != 0.
pair <- aq[complete.cases(aq[c("Ozone", "Temp")]), ]
ct <- cor.test(pair$Ozone, pair$Temp, method = "pearson")
cat("Ozone-temperature paired days:", nrow(pair), "\n")
print(ct)

# Complete-case modeling avoids leakage from imputed outcomes.
# Every fifth complete row is a reserved test case; deterministic split.
model_data <- aq[complete.cases(aq[c("Ozone", "Temp", "Wind", "Solar.R")]), ]
rownames(model_data) <- NULL
test_idx <- seq(5, nrow(model_data), by = 5)
train <- model_data[-test_idx, ]
test <- model_data[test_idx, ]
cat("Model complete cases:", nrow(model_data),
    "; train:", nrow(train), "; test:", nrow(test), "\n")

model <- lm(Ozone ~ Temp + Wind + Solar.R, data = train)
print(summary(model))
train_pred <- predict(model, newdata = train)
test_pred <- predict(model, newdata = test)
baseline_pred <- rep(mean(train$Ozone), nrow(test))

metrics <- function(actual, predicted) {
  errors <- actual - predicted
  c(MAE = mean(abs(errors)),
    RMSE = sqrt(mean(errors^2)),
    R2 = 1 - sum(errors^2) / sum((actual - mean(actual))^2))
}
cat("Train metrics:\n"); print(round(metrics(train$Ozone, train_pred), 3))
cat("Test metrics:\n"); print(round(metrics(test$Ozone, test_pred), 3))
cat("Test mean-only baseline:\n")
print(round(metrics(test$Ozone, baseline_pred), 3))

# Five folds within the training subset; test data remain untouched.
cv_pred <- rep(NA_real_, nrow(train))
for (fold in 0:4) {
  valid <- which(seq_len(nrow(train)) %% 5 == fold)
  fitted <- lm(Ozone ~ Temp + Wind + Solar.R, data = train[-valid, ])
  cv_pred[valid] <- predict(fitted, newdata = train[valid, ])
}
cat("Five-fold training cross-validation:\n")
print(round(metrics(train$Ozone, cv_pred), 3))

# Residual assumption check: Shapiro-Wilk test on training residuals.
# Small p value cautions against trusting usual coefficient t intervals.
cat("Shapiro-Wilk training residuals:\n")
print(shapiro.test(residuals(model)))

png("week3_01_test_predictions.png", width = 1200, height = 760, res = 150)
plot(test$Ozone, test_pred, pch = 19, col = "#3678A5",
     xlab = "Measured ozone (ppb)", ylab = "Predicted ozone (ppb)",
     main = "Held-out test predictions")
abline(0, 1, col = "#C04B40", lwd = 2, lty = 2)
grid(col = "#E5E5E5")
dev.off()

png("week3_02_training_residuals.png", width = 1200, height = 760, res = 150)
plot(fitted(model), residuals(model), pch = 19, col = "#3678A5",
     xlab = "Fitted ozone (ppb)", ylab = "Training residual (ppb)",
     main = "Residuals versus fitted values")
abline(h = 0, col = "#C04B40", lwd = 2, lty = 2)
grid(col = "#E5E5E5")
dev.off()

png("week3_03_residual_qq.png", width = 1200, height = 760, res = 150)
qqnorm(residuals(model), pch = 19, col = "#3678A5",
       main = "Normal Q-Q plot of training residuals")
qqline(residuals(model), col = "#C04B40", lwd = 2)
dev.off()

write.csv(data.frame(MeasuredOzone = test$Ozone,
                     PredictedOzone = test_pred,
                     Error = test$Ozone - test_pred),
          "week3_test_predictions.csv", row.names = FALSE)
