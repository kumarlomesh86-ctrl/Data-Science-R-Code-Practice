# Sample data 
df <- read.csv("C:/Users/welcome/synthetic_housing_data.csv")

# Multiple Linear Regression 
model <- lm(Price ~ Size + Bedrooms + Age + Distance, data = df) 
summary(model) 
# Lasso Regression 
library(glmnet) 
X <- as.matrix(df[, c("Size", "Bedrooms", "Age", "Distance")]) 
y <- df$Price 
lasso <- cv.glmnet(X, y, alpha = 1) 
coef(lasso, s = "lambda.min") 