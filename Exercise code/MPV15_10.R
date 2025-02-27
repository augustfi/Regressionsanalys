# MPV Exercise 15.10

library(MPV)
data(softdrink)

# Try a linear model
softdrink.model <- lm(y ~ ., data=softdrink)
plot(softdrink.model)

# It's not a good model

library(car)

m_values <- c(100, 200, 300, 400, 500)

results <- data.frame(m = m_values, case = NA, residual = NA)

for (i in 1:5) {
  softdrink.case <- Boot(softdrink.model, R=m_values[i], method="case") #subsampling
  results$case[i] <- sd(softdrink.case$t[, 2])  
  
  softdrink.res <- Boot(softdrink.model, R=m_values[i], method="residual") #shuffling around residuals
  results$residual[i] <- sd(softdrink.res$t[, 2])  
}

print(results)

summary(softdrink.model)

