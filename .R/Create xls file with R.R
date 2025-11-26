library(writexl)

set.seed(123)

# Number of firms
n <- 64   # for example, 32 defaulting, 32 non-defaulting

# Simulate data
df <- data.frame(
  Default = factor(c(rep(0, 32), rep(1, 32))),  # 0 = non-default, 1 = default
  CurrentRatio = c(rnorm(32, 2.0, 0.5), rnorm(32, 1.0, 0.3)),
  DebtEquity = c(rnorm(32, 1.0, 0.3), rnorm(32, 2.0, 0.5)),
  ROA = c(rnorm(32, 0.08, 0.02), rnorm(32, 0.03, 0.01)),
  CashFlow = c(rnorm(32, 500, 100), rnorm(32, 200, 50)),
  LiquidityRatio = c(rnorm(32, 1.8, 0.4), rnorm(32, 1.0, 0.3)),
  Leverage = c(rnorm(32, 0.5, 0.1), rnorm(32, 1.5, 0.3)),
  ProfitMargin = c(rnorm(32, 0.12, 0.03), rnorm(32, 0.05, 0.02)),
  InterestCoverage = c(rnorm(32, 5, 1), rnorm(32, 2, 0.5))
)

# Save as Excel
write_xlsx(df, "default-analysis-data.xlsx")