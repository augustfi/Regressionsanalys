# Exercise MPV 6.1

library(MPV)
solar <- table.b2

### Cutoff values (from Belsley, Kuh, and Welsch)
n <- nrow(solar)
p <- ncol(solar)
leverage.cutoff <- 2 * p / n  # MPV p. 213
cooks.cutoff <- qf(0.5, p, n - p, lower.tail = FALSE)  # MPV p. 215
dfbetas.cutoff <- 2 / sqrt(n)  # MPV p. 218
dffits.cutoff <- 2 * sqrt(p / n)  # MPV p. 219
studres.cutoff <- qt(0.05 / 2, n - p, lower.tail = FALSE)  # MPV p. 135

solar.model <- lm(y ~ ., data = solar)

### leverage points
solar.hat <- hatvalues(solar.model)
solar.hat[solar.hat > leverage.cutoff]

### Cook's distance
solar.cooks <- cooks.distance(solar.model)
solar.cooks[solar.cooks > cooks.cutoff]

### DFBETAS
solar.dfbetas <- dfbetas(solar.model)
solar.dfbetas[abs(solar.dfbetas[, 1]) > dfbetas.cutoff, 1]  # beta_0
solar.dfbetas[abs(solar.dfbetas[, 2]) > dfbetas.cutoff, 2]  # beta_1
solar.dfbetas[abs(solar.dfbetas[, 3]) > dfbetas.cutoff, 3]  # etc
solar.dfbetas[abs(solar.dfbetas[, 4]) > dfbetas.cutoff, 4]
solar.dfbetas[abs(solar.dfbetas[, 5]) > dfbetas.cutoff, 5]
solar.dfbetas[abs(solar.dfbetas[, 6]) > dfbetas.cutoff, 6]

### DFFITS
solar.dffits <- dffits(solar.model)
solar.dffits[abs(solar.dffits) > dffits.cutoff]

# Built-in functions
par(mfrow = c(2, 3))
plot(solar.model, 1:5)

solar.influence <- influence.measures(solar.model)
summary(solar.influence)
