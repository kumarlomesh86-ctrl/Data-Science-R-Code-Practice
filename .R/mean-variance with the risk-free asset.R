### --- Mean-Variance with Risk-Free Asset ---

# Example returns for 3 risky assets (replace with your data)
set.seed(123)
returns <- matrix(rnorm(300), ncol = 3)
colnames(returns) <- c("Asset1", "Asset2", "Asset3")

# Expected returns (mu) and covariance matrix (cv)
mu <- colMeans(returns)
cv <- cov(returns)

# Assume a risk-free rate
rf <- 0.03   # 3% risk-free

### 1. Compute tangency portfolio (Max Sharpe ratio portfolio)

ones <- matrix(1, nrow = length(mu), ncol = 1)
inv_cv <- solve(cv)

excess_mu <- mu - rf   # excess returns over risk-free

# Tangency weights (not yet normalized)
tan_wts_raw <- inv_cv %*% excess_mu

# Normalize so weights sum to 1
tan_wts <- tan_wts_raw / sum(tan_wts_raw)

names(tan_wts) <- colnames(returns)

cat("\nTangency Portfolio Weights:\n")
print(tan_wts)

### 2. Portfolio mean and risk of tangency portfolio
mean_tan <- sum(tan_wts * mu)
risk_tan <- sqrt(drop(t(tan_wts) %*% cv %*% tan_wts))

cat("\nTangency Portfolio Expected Return:\n", mean_tan, "\n")
cat("Tangency Portfolio Risk (Std Dev):\n", risk_tan, "\n")

### 3. Combine with risk-free asset for a target portfolio return
target_Er <- 0.12   # e.g. 12% desired return

# Weight in risky tangency portfolio
w_tan_alloc <- (target_Er - rf) / (mean_tan - rf)

# Final portfolio weights (risky + risk-free)
final_wts <- w_tan_alloc * tan_wts
w_rf <- 1 - w_tan_alloc

cat("\nFinal Portfolio Allocation:\n")
cat("Risk-free asset:", w_rf, "\n")
print(final_wts)

### 4. Portfolio risk (std dev)
sigma_final <- w_tan_alloc * risk_tan
cat("\nFinal Portfolio Risk (Std Dev):\n", sigma_final, "\n")