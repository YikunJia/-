
ggplot(data, aes(x = x_value, y = y_value, color = period, shape = period)) +
  geom_point(size = 3, alpha = 0.8, stroke = 1.2) +
 
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = TRUE, aes(fill = period), alpha = 0.2) +
  scale_color_manual(
    name = "Period",
    values = c("MX" = "#8B0000", "ITS" = "#FF8C00", "OTS" = "#006400"),
    labels = c("MX Period", "ITS Period", "OTS Period")
  ) +
  scale_shape_manual(
    name = "Period",
    values = c("MX" = 1, "ITS" = 1, "OTS" = 1), 
    labels = c("MX Period", "ITS Period", "OTS Period")
  ) +
  scale_fill_manual(
    name = "Period",
    values = c("MX" = "#8B0000", "ITS" = "#FF8C00", "OTS" = "#006400"),
    labels = c("MX Period", "ITS Period", "OTS Period")
  ) +
  labs(
    title = "",
    x = "x_value",
    y = "y_value"
  ) +
  theme_minimal()


par(mar = c(5, 5, 4, 8), xpd = TRUE)


plot(NA, xlim = range(data$x_value), ylim = range(data$y_value),
     xlab = "x_value", ylab = "y_value",
     main = "")


mx_data <- data[data$period == "MX", ]
its_data <- data[data$period == "ITS", ]
ots_data <- data[data$period == "OTS", ]


points(mx_data$x_value, mx_data$y_value, col = "#8B0000", pch = 1, cex = 1.5, lwd = 1.5)
points(its_data$x_value, its_data$y_value, col = "#FF8C00", pch = 1, cex = 1.5, lwd = 1.5)
points(ots_data$x_value, ots_data$y_value, col = "#006400", pch = 1, cex = 1.5, lwd = 1.5)

x_seq <- seq(min(data$x_value), max(data$x_value), length.out = 100)

quadratic_model <- lm(y_value ~ poly(x_value, 2), data = data)

y_pred <- predict(quadratic_model, newdata = data.frame(x_value = x_seq))

lines(x_seq, y_pred, lty = 2, lwd = 2, col = "black")

legend(par("usr")[2] * 1.02, par("usr")[4], 
       legend = c("MX Period", "ITS Period", "OTS Period", "Equation"),
       col = c("#8B0000", "#FF8C00", "#006400", "black"),
       pch = c(1, 1, 1, NA), 
       lty = c(NA, NA, NA, 2),
       lwd = c(1.5, 1.5, 1.5, 2),
       pt.cex = 1.2,
       bty = "n")


par(mar = c(5, 4, 4, 2) + 0.1, xpd = FALSE)


cat("=== 一元二次方程回归统计摘要 ===\n\n")


overall_quadratic_model <- lm(y_value ~ poly(x_value, 2), data = data)
cat("总体一元二次方程回归:\n")
print(summary(overall_quadratic_model))

# 分时期一元二次方程回归
cat("\n--- Linear Correlation ---\n")
for(period_type in c("MX", "ITS", "OTS")) {
  period_data <- data[data$period == period_type, ]
  period_model <- lm(y_value ~ poly(x_value, 2), data = period_data)
  cat(paste0("\n", period_type, "Linear Correlation:\n"))
  print(summary(period_model))
}


library(broom)
quadratic_models <- list(
  "Sum" = lm(y_value ~ poly(x_value, 2), data = data),
  "MX" = lm(y_value ~ poly(x_value, 2), data = mx_data),
  "ITS" = lm(y_value ~ poly(x_value, 2), data = its_data),
  "OTS" = lm(y_value ~ poly(x_value, 2), data = ots_data)
)

quadratic_coefficients_table <- bind_rows(lapply(names(quadratic_models), function(name) {
  tidy(quadratic_models[[name]]) %>% 
    mutate(Model = name)
}))
