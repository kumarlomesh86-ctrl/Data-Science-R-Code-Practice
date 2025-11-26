markowitz_weights <- function(returns, Er) {
  # Ensure input is a matrix
  returns <- as.matrix(returns)
  
  # Expected returns (mu) as column vector
  mu <- colMeans(returns)
  mu <- matrix(mu, ncol = 1)
  
  # Covariance matrix
  cv <- cov(returns)
  
  # Vector of ones
  wuns <- matrix(1, nrow = length(mu), ncol = 1)
  
  # Key scalars
  A <- t(wuns) %*% solve(cv) %*% mu
  B <- t(mu)   %*% solve(cv) %*% mu
  C <- t(wuns) %*% solve(cv) %*% wuns
  D <- B * C - A^2
  
  # Lagrange multipliers
  lam <- (C * Er - A) / D
  gam <- (B - A * Er) / D
  
  # Optimal weights
  wts <- lam[1] * (solve(cv) %*% mu) + gam[1] * (solve(cv) %*% wuns)
  wts <- as.vector(wts)  # simplify to numeric vector
  
  # Name weights with asset names if available
  if (!is.null(colnames(returns))) {
    names(wts) <- colnames(returns)
  }
  
  return(wts)
}

#PARAMETERS
mu=matrix(c(0.02,0.10,0.20) ,3,1)
n = length(mu)
cv =matrix(c(0.0001,0,0,0,0.04,0.02,0,0.02,0.16) ,n,n)
Er = 0.18
#SOLVEPORTFOLIOPROBLEM
wts =markowitz(mu,cv,Er)
print(wts)

Er = 0.10
wts =markowitz(mu,cv,Er)
print(wts)


# Simulate returns for 3 assets (100 days)
set.seed(123)
returns <- matrix(rnorm(300), ncol = 3)
colnames(returns) <- c("StockA", "StockB", "StockC")

# Target return (say 5% per period)
Er <- 0.05

# Compute optimal weights
markowitz_weights(returns, Er)

Er=0.10
wts=markowitz(mu,cv,Er)
print(wts)