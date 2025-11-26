# Load libraries
library(readxl)   # to read Excel files
library(MASS)     # for lda()
library(ggplot2)  # for plotting

# 1. Import the Excel file
df <- read_excel("default-analysis-data.xlsx")

# 2. Prepare the data
# Assume dependent variable is named "Default" (0 = no default, 1 = default)
df$Default <- as.factor(df$Default)

# 3. Fit the LDA model
lda_model <- lda(Default ~ ., data = df)

# Print model summary
print(lda_model)

# 4. Predictions
pred <- predict(lda_model)

# Confusion matrix
cm <- table(Predicted = pred$class, Actual = df$Default)
print(cm)

# 5. Model fit metrics
accuracy <- sum(diag(cm)) / sum(cm)
misclassification_rate <- 1 - accuracy
cat("Accuracy:", accuracy, "\n")
cat("Misclassification Rate:", misclassification_rate, "\n")

# 6. Chi-square test (significance of classification ability)
print(chisq.test(cm))

# 7. Cross-validation (robustness check)
lda_cv <- lda(Default ~ ., data = df, CV = TRUE)
cv_cm <- table(Predicted = lda_cv$class, Actual = df$Default)
print(cv_cm)

# 8. Plot Discriminant Scores
scores <- pred$x   # LD scores
df$LD1 <- scores[,1]

ggplot(df, aes(x = LD1, fill = Default)) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 20) +
  labs(title = "Discriminant Function Separation",
       x = "LD1 (Discriminant Score)",
       y = "Count") +
  scale_fill_manual(values = c("red", "blue"),
                    labels = c("Non-default", "Default")) +
  theme_minimal()