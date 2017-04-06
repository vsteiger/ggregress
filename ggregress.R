ggplotRegression <- function (fit) {
  # takes the estimation from a linear regression model using "fit <- lm(y ~ x + Covariate, data)"
  require(ggplot2)

  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) +
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 4),
                       "Intercept =",signif(fit$coef[[1]], 4),
                       " Slope =",signif(fit$coef[[2]], 4),
                       " P =",signif(summary(fit)$coef[2,4], 4)))
}
