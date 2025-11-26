# Example Treasury rates dataset
rates <- data.frame(
  DATE   = as.Date(c("2025-01-01", "2025-02-01", "2025-03-01")),
  FYGM3  = c(4.95, 5.02, 4.88),   # 3-Month Treasury
  FYGM6  = c(5.10, 5.15, 5.05),   # 6-Month Treasury
  FYGT1  = c(5.20, 5.18, 5.12),   # 1-Year Treasury
  FYGT2  = c(4.90, 4.85, 4.80),   # 2-Year Treasury
  FYGT3  = c(4.75, 4.70, 4.65),   # 3-Year Treasury
  FYGT5  = c(4.60, 4.58, 4.55),   # 5-Year Treasury
  FYGT7  = c(4.50, 4.48, 4.45),   # 7-Year Treasury
  FYGT10 = c(4.40, 4.38, 4.35)    # 10-Year Treasury
)

# Check the structure
str(rates)

# View first rows
head(rates)


# Remove DATE column and keep only numeric rate data
rates_num <- rates[ , -1]   # drops the first column (DATE)

# Compute covariance matrix
cov_matrix <- cov(rates_num)

# Eigen decomposition
eig <- eigen(cov_matrix)
eig$value
eig$vectors
# Remove DATE column
rates_num <- rates[, -1]

# Compute correlation matrix
rcorr <- cor(rates_num)
rcorr
