# Exercise MPV 9.6

library(MPV)
nfl <- table.b1
nfl.model <- lm(y ~ x2 + x7 + x8, data = nfl)

# (a) Correlation matrix to detect multicollinearity
X <- model.matrix(nfl.model)
XtX <- cor(X[,-1])  # X^T X in correlation form
### Strong correlation between x7 and x8

# Using other built-in functions
library("GGally")  # ggpairs
ggpairs(data = data.frame(nfl$x2, nfl$x7, nfl$x8))

# (b) Variance inflation factor and condition number
library(car)  # vif
vif(nfl.model)  # cutoff value is 10 (MPV p. 118)
solve(XtX)  # = (X^T X)^{-1}. Compare diagonal values with VIFs

### Condition number, cutoff value is 100 (MPV p.298)
nfl.eigen <- eigen(XtX)
max(nfl.eigen$values) / min(nfl.eigen$values)
