# assume you already have the cleaned Fama-French dataset in df
# with columns: Date, Mkt_RF (market excess return), and RF (risk-free)

library(dplyr)
library(LearnBayes)
library(moments)

df<-read.csv('Fama-French data.csv')

# Equity premium = Mkt-RF (already in data)
# If you want raw Rm = (Mkt-RF + RF):
library(dplyr)
Mkt_RF=as.numeric(df[, 2])
RF=as.numeric(df[,5])

df$Rm <- Mkt_RF + RF

desc_stats <- function(x) {
  c(
    mean = mean(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    min = min(x, na.rm = TRUE),
    max = max(x, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    skewness = moments::skewness(x, na.rm = TRUE),
    kurtosis = moments::kurtosis(x, na.rm = TRUE)
  )
}

# 1. Market return Rm
Rm_stats <- desc_stats(df$Rm)

# 2. Risk-free rate Rf
Rf_stats <- desc_stats(df$RF)

# 3. Equity premium (Mkt-RF)
Prem_stats <- desc_stats(df$Mkt_RF)

cat("Market Return (Rm):\n"); 
print(Rm_stats)

hist(Rm_stats,
     main = "Distribution of Market Returns",
     xlab = "Market Return (%)",
     ylab = "Frequency",
     col = "lightblue",
     border = "white")


# Suppose Rm is your vector of market returns
h <- hist(Rm_stats, 
          breaks = 10,       # number of bins (you can change)
          probability = TRUE,  # histogram scaled to density
          plot = FALSE)        # don’t plot, just return data

# Inspect the structure
str(h)

# Extract discrete values (midpoints of bins) and probabilities
prior <- data.frame(
  midpoints = h$mids,        # center of each bin
  probability = h$density / sum(h$density)  # normalize to sum = 1
)

print(prior)

library(LearnBayes)

# ----------------
df$Rm <- as.numeric(as.character(df$Rm))   # safe conversion if factor/char
# check
is.numeric(df$Rm)

# Step 2: subset pre-2000 data
Rm_pre2000 <- df$Rm[df$Year <= 2000]
Rm_post2000 <- df$Rm[df$Year > 2000]

# Step 3: subset post-2000 data
Rm_post2000 <- df$Rm[df$Year > 2000]
h <- (hist(Rm_pre2000, breaks = 10, plot = FALSE))

 prior_values <- h$mids
prior_probs  <- h$density / sum(h$density)

# Make sure this is a numeric matrix, not data.frame
prior <- cbind(as.numeric(prior_values), as.numeric(prior_probs))
prior <- as.matrix(prior)
colnames(prior) <- NULL   # drop column names
rownames(prior) <- NULL 
mode(prior) <- "numeric"
str(prior)
head(prior)

# Likelihood from post-2000 data
mean_post <- mean(Rm_post2000,na.rm = TRUE)
sd_post   <- sd(Rm_post2000,na.rm=TRUE)

likelihood <- dnorm(prior_values, mean = mean_post, sd = sd_post)

# Posterior
posterior <- discrete.bayes(prior, likelihood)

print(posterior)
# ----------------
# Plot prior vs posterior
par(mfrow = c(1,1))
barplot(prior_probs, names.arg = round(prior_values,2), col = "lightblue",
        main = "Prior vs Posterior", xlab = "Equity Premium", ylab = "Probability")
barplot(posterior[,2], names.arg = round(prior_values,2), col = rgb(1,0,0,0.5), add = TRUE)

legend("topright", legend = c("Prior", "Posterior"),
       fill = c("lightblue", rgb(1,0,0,0.5)))