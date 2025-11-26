# --- Assume you already have mu and cv from your returns ---
# mu = column mean returns
# cv = covariance matrix

# Vector of ones
ones <- matrix(1, nrow = nrow(mu), ncol = 1)

# Inverse of covariance matrix
invC <- solve(cv)

# Minimum variance weights
w_mvp <- invC %*% ones / as.numeric(t(ones) %*% invC %*% ones)
w_mvp <- as.vector(w_mvp)
names(w_mvp) <- colnames(returns)
# Expected return of MVP
Er_mvp <- sum(w_mvp * colMeans(returns))

# Risk (std dev) of MVP
sigma_mvp <- sqrt(drop(t(w_mvp) %*% cv %*% w_mvp))

cat("Minimum Variance Portfolio Weights:\n")
print(w_mvp)

cat("\nExpected Return of MVP:\n")
print(Er_mvp)

cat("\nRisk (Std. Dev.) of MVP:\n")
print(sigma_mvp)
