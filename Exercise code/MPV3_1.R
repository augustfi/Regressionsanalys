# MPV, Exercise 3.1

library(MPV)
library(scatterplot3d)
nfl <- table.b1
pairs(nfl, pch = ".") # :)

n <- nrow(nfl)

# (a) Fit passing yardage, rushing plays, and opponent rushing yards
nfl.model <- lm(y ~ x2 + x7 + x8, data = nfl)
summary(nfl.model)
par(mfrow = c(1, 1))
nfl.3dplot <- scatterplot3d(nfl$x2, nfl$x8, nfl$y, angle = 130, grid=F)
nfl.3dplot$plane3d(
  nfl.model$coefficients["(Intercept)"],
  x.coef = nfl.model$coefficients["x2"],
  y.coef = nfl.model$coefficients["x8"]
)

# (b) "ANOVA table" found at the bottom of the model summary
summary(nfl.model)

### By hand: MPV 3.3.1
p <- length(coef(nfl.model))
SS_R <- sum((nfl.model$fitted.values - mean(nfl$y))^2)
SS_Res <- sum((nfl$y - nfl.model$fitted.values)^2)
F_0 <- (SS_R / (p - 1)) / (SS_Res / (n - p))
pf(F_0, p - 1, n - p, lower.tail = FALSE)

# (c) t statistic for beta_2 = 0, beta_7 = 0, and beta_8 = 0
summary(nfl.model)

# (d) Calculate R^2 and adjusted R^2
### By hand
SS_Res <- sum((nfl$y - nfl.model$fitted.values)^2)
SS_T <- sum((nfl$y - mean(nfl$y))^2)

### Alternatively, for matrix syntax
games_won <- matrix(nfl$y)
SS_T <- t(games_won) %*% games_won - sum(games_won)^2 / nrow(games_won)

R2 <- 1 - SS_Res / SS_T
R2.adj <- 1 - (SS_Res / (n - p)) / (SS_T / (n - 1))

### compare with built-in function
summary(nfl.model)

# (e) F test for significance of beta_7
### Calculate SS_R(beta_7 | beta_reduced) [MPV pp. 89-90]
nfl.model.reduced <- lm(y ~ x2 + x8, data = nfl)
r <- length(coef(nfl.model)) - length(coef(nfl.model.reduced))

X.reduced <- model.matrix(nfl.model.reduced)
beta.reduced <- matrix(coef(nfl.model.reduced))
SS_R.reduced <- (t(beta.reduced) %*% t(X.reduced) %*% games_won
                - sum(games_won)^2 / nrow(games_won))
SS_R.extra <- SS_R - SS_R.reduced

### F test
F0.reduced <- (SS_R.extra / r) / (SS_Res / (n - p))
pf(F0.reduced, r, n - p, lower.tail=FALSE)
# the p-value is less that .05, so the contribution of x_7 is significant.

### MPV C.3.5: t_d^2 is equivalent to F_1,d for extra-sum-of-squares test
summary(nfl.model)
sqrt(F0.reduced)

