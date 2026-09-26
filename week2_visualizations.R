# Week 2 Data Visualization and Insight Communication using R
# Run in R or RStudio. Base R only; no package installation is needed.
data(airquality, package = "datasets")
aq <- airquality
aq$MonthName <- factor(aq$Month, levels = 5:9,
                       labels = c("May", "June", "July", "August", "September"))
aq$Date <- as.Date(sprintf("1973-%02d-%02d", aq$Month, aq$Day))
cat("Rows:", nrow(aq), "Columns:", ncol(aq), "\n")
print(colSums(is.na(aq[c("Ozone", "Solar.R", "Wind", "Temp")])))
month_mean <- tapply(aq$Ozone, aq$MonthName, mean, na.rm = TRUE)
month_count <- tapply(!is.na(aq$Ozone), aq$MonthName, sum)
print(round(month_mean, 2)); print(month_count)
cat("Observed ozone mean:", round(mean(aq$Ozone, na.rm = TRUE), 2), "ppb\n")
cat("Observed ozone median:", median(aq$Ozone, na.rm = TRUE), "ppb\n")
cat("Ozone and temperature correlation:",
    round(cor(aq$Ozone, aq$Temp, use = "complete.obs"), 3), "\n")
cat("Hottest day:", format(aq$Date[which.max(aq$Temp)]),
    "; temperature:", max(aq$Temp), "F\n")

# Chart 1: category comparison. Counts above the bars show coverage.
png("01_monthly_ozone_bar.png", width = 1200, height = 760, res = 150)
par(mar = c(5, 5, 4, 2))
bp <- barplot(month_mean, col = "#3678A5", ylim = c(0, 72),
              main = "Average measured ozone by month in 1973",
              xlab = "Month", ylab = "Mean ozone (ppb)")
text(bp, month_mean + 3, paste0("n=", month_count), cex = 0.85)
mtext("Only recorded ozone values are averaged; June has limited coverage.",
      side = 1, line = 3.4, cex = 0.75)
dev.off()

# Chart 2: paired continuous readings. Missing ozone points are omitted.
png("02_ozone_temperature_scatter.png", width = 1200, height = 760, res = 150)
par(mar = c(5, 5, 4, 2))
plot(aq$Temp, aq$Ozone, pch = 19, col = adjustcolor("#276B93", alpha.f = 0.7),
     xlab = "Daily maximum temperature (degrees F)", ylab = "Ozone (ppb)",
     main = "Ozone versus temperature on measured days")
abline(lm(Ozone ~ Temp, data = aq), col = "#C04B40", lwd = 2)
grid(col = "#E7E7E7")
dev.off()

# Chart 3: distribution of observed ozone. Bin width is 20 ppb.
png("03_ozone_histogram.png", width = 1200, height = 760, res = 150)
par(mar = c(5, 5, 4, 2))
hist(aq$Ozone, breaks = seq(0, 180, by = 20), right = FALSE,
     col = "#5D9F90", border = "white", xlim = c(0, 180),
     main = "Distribution of measured ozone", xlab = "Ozone (ppb)",
     ylab = "Number of measured days")
abline(v = median(aq$Ozone, na.rm = TRUE), col = "#A83C38", lwd = 2, lty = 2)
legend("topright", "Median 31.5 ppb", col = "#A83C38", lty = 2, bty = "n")
dev.off()

# Chart 4: time sequence. Temperature is complete; no imputation is needed.
png("04_daily_temperature_line.png", width = 1200, height = 760, res = 150)
par(mar = c(5, 5, 4, 2))
plot(aq$Date, aq$Temp, type = "l", col = "#AC6248", lwd = 1.7,
     main = "Daily maximum temperature from May to September 1973",
     xlab = "Date", ylab = "Daily maximum temperature (degrees F)")
abline(h = mean(aq$Temp), col = "#365F84", lty = 2, lwd = 2)
legend("topleft", sprintf("Period mean %.2f F", mean(aq$Temp)),
       col = "#365F84", lty = 2, bty = "n")
grid(col = "#E7E7E7")
dev.off()
