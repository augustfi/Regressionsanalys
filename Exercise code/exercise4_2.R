# Exercise session 4, exercise 2

set.seed(25)
n <- 100
xx <- 1:n
# general formula: yy = beta0 + beta1 * xx + epsilon
yy.a <- 2 + 1 * xx + rnorm(n)
yy.b <- 2 + 1 * xx + rnorm(n) * (xx)
yy.c <- 2 + 1 * xx + rnorm(n) * (1 + xx / n)
yy.d <- cos(2 * xx * pi / n) + rnorm(n)
p <- 2

### Cutoff values (from Belsley, Kuh, and Welsch)
leverage.cutoff <- 2 * p / n  # MPV p. 213
cooks.cutoff <- qf(0.5, p, n - p, lower.tail = FALSE)  # MPV p. 215
# studres.cutoff <- qt(0.05 / 2, n - p, lower.tail = FALSE)  # MPV p. 135

# (a) plot regression models
model.a <- lm(yy.a ~ xx)
par(mfrow = c(1, 1))
plot(xx, yy.a)
abline(model.a)
par(mfrow = c(2, 3))
plot(model.a, which=1:5)
### seems ok: some influential points, but that is to be expected

model.b <- lm(yy.b ~ xx)
par(mfrow = c(1, 1))
plot(xx, yy.b)
abline(model.b)
par(mfrow = c(2, 3))
plot(model.b, which=1:5)
### nonconstant variance, leading to high leverage points and non-
### normality of residuals.

model.c <- lm(yy.c ~ xx)
par(mfrow = c(1, 1))
plot(xx, yy.c)
abline(model.c)
par(mfrow = c(2, 3))
plot(model.c, which=1:5)
### nonconstant variance, but not as strong as model B.

model.d <- lm(yy.d ~ xx)
par(mfrow = c(1, 1))
plot(xx, yy.d)
abline(model.d)
par(mfrow = c(2, 3))
plot(model.d, which=1:5)
### model is wrong: non-linear dependence. Residuals vs. fitted
### indicates nonconstant variance.

# (b) Using resplot
source("resplot.R")
resplot(model.a)  # seems ok
resplot(model.b)  # scale-location
resplot(model.c)  # ...
resplot(model.d)  # non-linearity

# (c) Repeat for different seeds, different n

# Observation size
set.seed(25)
n <- 30
xx <- 1:n
# general formula: yy = beta0 + beta1 * xx + epsilon
yy.a <- 2 + 1 * xx + rnorm(n)
yy.b <- 2 + 1 * xx + rnorm(n) * (xx)
yy.c <- 2 + 1 * xx + rnorm(n) * (1 + xx / n)
yy.d <- cos(2 * xx * pi / n) + rnorm(n)
p <- 2

### Cutoff values (from Belsley, Kuh, and Welsch)
leverage.cutoff <- 2 * p / n  # MPV p. 213
cooks.cutoff <- qf(0.5, p, n - p, lower.tail = FALSE)  # MPV p. 215
# studres.cutoff <- qt(0.05 / 2, n - p, lower.tail = FALSE)  # MPV p. 135

# (a) plot regression models
model.a <- lm(yy.a ~ xx)
par(mfrow = c(1, 1))
plot(xx, yy.a)
abline(model.a)
par(mfrow = c(2, 3))
plot(model.a, which=1:5)
### Even model with correct specification, small sample can result in suspicious diagnosis plot.

model.b <- lm(yy.b ~ xx)
par(mfrow = c(1, 1))
plot(xx, yy.b)
abline(model.b)
par(mfrow = c(2, 3))
plot(model.b, which=1:5)
### nonconstant variance, the Residuals vs Leverage plot looks better than that in model.a.

model.c <- lm(yy.c ~ xx)
par(mfrow = c(1, 1))
plot(xx, yy.c)
abline(model.c)
par(mfrow = c(2, 3))
plot(model.c, which=1:5)
### nonconstant variance

model.d <- lm(yy.d ~ xx)
par(mfrow = c(1, 1))
plot(xx, yy.d)
abline(model.d)
par(mfrow = c(2, 3))
plot(model.d, which=1:5)
### model is still wrong: non-linear dependence.