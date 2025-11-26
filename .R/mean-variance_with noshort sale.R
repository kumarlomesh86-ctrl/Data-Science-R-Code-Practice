# ---------------------------
# Efficient frontiers: with and without short sales
# ---------------------------

# If you don't have quadprog installed, uncomment the next line:
# install.packages("quadprog")

library(quadprog)

# --- Example data: replace 'returns' with your real returns matrix if you have one ---
set.seed(123)
returns <- matrix(rnorm(300, mean = 0.0008, sd = 0.02), ncol = 3)
colnames(returns) <- c("A", "B", "C")

# --- compute mu and covariance ---
returns <- as.matrix(returns)
mu_vec <- as.numeric(colMeans(returns))   # length-n numeric vector
cv <- cov(returns)
n <- length(mu_vec)

# small regularization to ensure positive-definite
eps <- 1e-8
Dmat_base <- 2 * (cv + diag(eps, n))   # Dmat = 2 * Sigma for solve.QP (min 1/2 w' D w - d' w)

# --- analytic/unconstrained Markowitz function (mu & cv) ---
markowitz_mu_cv <- function(mu, cv, Er) {
  mu <- matrix(mu, ncol = 1)
  n <- length(mu)
  ones <- matrix(1, n, 1)
  invC <- solve(cv)
  A <- as.numeric(t(ones) %*% invC %*% mu)
  B <- as.numeric(t(mu)   %*% invC %*% mu)
  C <- as.numeric(t(ones) %*% invC %*% ones)
  D <- B*C - A^2
  lam <- (C * Er - A) / D
  gam <- (B - A * Er) / D
  wts <- lam * (invC %*% mu) + gam * (invC %*% ones)
  as.vector(wts)
}

# --- function to get no-short-sales solution for a given target return Er ---
# Uses quadprog::solve.QP to minimize (1/2) w' (2Sigma) w subject to:
# 1) mu^T w == Er  (equality)
# 2) 1^T w  == 1   (equality)
# 3) w >= 0        (inequality)
no_short_markowitz <- function(mu, cv, Er) {
  n <- length(mu)
  Dmat <- 2 * (cv + diag(eps, n))     # 2*Sigma (make PD)
  dvec <- rep(0, n)                   # no linear term
  # Build constraint matrix A where constraints are A^T w >= bvec
  # We'll place the equality constraints first and set meq=2
  # Columns of A: mu, ones, I_n (for w >= 0)
  A <- cbind(mu, rep(1, n), diag(1, n))
  bvec <- c(Er, 1, rep(0, n))
  # solve.QP expects Dmat to be positive definite
  sol <- tryCatch({
    solve.QP(Dmat = Dmat, dvec = dvec, Amat = A, bvec = bvec, meq = 2)
  }, error = function(e) {
    warning("solve.QP failed: ", conditionMessage(e))
    return(NULL)
  })
  if (is.null(sol)) return(NULL)
  w <- sol$solution
  return(as.vector(w))
}

# --- Trace frontiers ---
Er_vec <- seq(min(mu_vec)*0.9, max(mu_vec)*1.5, length.out = 80)  # range of target returns
Sig_uncon <- numeric(length(Er_vec))
Sig_noshort <- rep(NA, length(Er_vec))
W_uncon <- matrix(NA, nrow = n, ncol = length(Er_vec))
W_noshort <- matrix(NA, nrow = n, ncol = length(Er_vec))

for (i in seq_along(Er_vec)) {
  Er <- Er_vec[i]
  # unconstrained analytic solution
  w_uncon <- markowitz_mu_cv(mu_vec, cv, Er)
  W_uncon[, i] <- w_uncon
  w_uncon_m <- matrix(w_uncon, ncol = 1)
  Sig_uncon[i] <- sqrt(drop(t(w_uncon_m) %*% cv %*% w_uncon_m))
  
  # no-short solution via quadprog
  w_ns <- no_short_markowitz(mu_vec, cv, Er)
  if (!is.null(w_ns)) {
    W_noshort[, i] <- w_ns
    w_ns_m <- matrix(w_ns, ncol = 1)
    Sig_noshort[i] <- sqrt(drop(t(w_ns_m) %*% cv %*% w_ns_m))
  } else {
    Sig_noshort[i] <- NA
  }
}

# Remove NA points from no-short frontier (quadprog may fail for unreachable Er)
valid_ns <- !is.na(Sig_noshort)

# --- Minimum Variance Portfolios (for labels) ---
# unconstrained MVP
ones <- rep(1, n)
invC <- solve(cv)
w_mvp_uncon <- as.vector(invC %*% ones / as.numeric(t(ones) %*% invC %*% ones))
sigma_mvp_uncon <- sqrt(drop(t(w_mvp_uncon) %*% cv %*% w_mvp_uncon))
er_mvp_uncon <- sum(w_mvp_uncon * mu_vec)

# no-short MVP: minimize variance subject to sum(w)=1 and w>=0
# This is a quadratic program with equality sum(w)=1 and w>=0 (no target return)
A_mvp <- cbind(rep(1, n), diag(1, n))
b_mvp <- c(1, rep(0, n))
sol_mvp_ns <- solve.QP(Dmat = Dmat_base, dvec = rep(0, n), Amat = A_mvp, bvec = b_mvp, meq = 1)
w_mvp_ns <- as.vector(sol_mvp_ns$solution)
sigma_mvp_ns <- sqrt(drop(t(w_mvp_ns) %*% cv %*% w_mvp_ns))
er_mvp_ns <- sum(w_mvp_ns * mu_vec)

# --- Plot both frontiers ---
op <- par(no.readonly = TRUE)
par(mar = c(5,5,4,2) + 0.1)

plot(Sig_uncon, Er_vec, type = "l", lwd = 2,
     xlab = expression(Risk~(sigma)), ylab = "Expected return",
     main = "Efficient Frontier: Unconstrained vs No-Short-Sales",
     xlim = range(c(Sig_uncon, Sig_noshort, sigma_mvp_uncon, sigma_mvp_ns), na.rm = TRUE),
     ylim = range(c(Er_vec, er_mvp_uncon, er_mvp_ns), na.rm = TRUE))

# unconstrained: solid black line
lines(Sig_uncon, Er_vec, lwd = 2)

# no-short: dashed blue line (only valid points)
lines(Sig_noshort[valid_ns], Er_vec[valid_ns], lwd = 2, lty = 2, col = "blue")

# mark MVPs
points(sigma_mvp_uncon, er_mvp_uncon, pch = 19, col = "black", cex = 1.4)
text(sigma_mvp_uncon, er_mvp_uncon, labels = "MVP (unconstr)", pos = 4, col = "black")

points(sigma_mvp_ns, er_mvp_ns, pch = 17, col = "blue", cex = 1.4)
text(sigma_mvp_ns, er_mvp_ns, labels = "MVP (no-short)", pos = 4, col = "blue")

legend("topleft",
       legend = c("Unconstrained (short allowed)", "No-short-sales"),
       lty = c(1, 2), col = c("black", "blue"),
       lwd = 2, bty = "n")

par(op)

