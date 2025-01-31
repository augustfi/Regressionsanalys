# Exercise session 4, exercise 6

moore <- read.csv("moore.csv", comment.char="#")
par(mfrow = c(1, 1))
plot(moore)

n <- nrow(moore)
p <- ncol(moore)
cooks.cutoff <- qf(0.5, p, n - p, lower.tail = FALSE) # MPV p. 215

# Simple linear model
moore.model <- lm(Transistors ~ Year, data=moore)
summary(moore.model)
abline(moore.model)

par(mfrow = c(2, 3))
plot(moore.model, which=1:5)

### The extremal observations appear to be outliers

# Log transformation
moore$Transistors <- log(moore$Transistors)
moore.model <- lm(Transistors ~ Year, data=moore)
summary(moore.model)
par(mfrow = c(1, 1))
plot(moore)
abline(moore.model)

par(mfrow = c(2, 3))
plot(moore.model, which=1:5)

### The candidates for outliers are 2, 8, 44, 70.
