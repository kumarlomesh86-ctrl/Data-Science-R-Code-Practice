set.seed(123)

# Simulated data
group1 <- data.frame(
  X1 = rnorm(50, mean=2, sd=1),   # Good separation
  X2 = rnorm(50, mean=5, sd=2),   # More overlap
  Group = "Group 1"
)

group2 <- data.frame(
  X1 = rnorm(50, mean=6, sd=1),   # Separated along X1
  X2 = rnorm(50, mean=6, sd=2),   # Overlap along X2
  Group = "Group 2"
)

data <- rbind(group1, group2)

# Plot
par(mfrow=c(1,2))
# --- Plot density of X1 (Discriminant 1) ---
plot(density(data$X1[data$Group=="Group 1"]), col="blue", lwd=2,
     main="Discriminant 1 (X1): Better Separation", xlab="X1")
lines(density(data$X1[data$Group=="Group 2"]), col="red", lwd=2)
legend("topright", legend=c("Group 1","Group 2"), col=c("blue","red"), lwd=2, cex=0.4)

# --- Plot density of X2 (Discriminant 2) ---
plot(density(data$X2[data$Group=="Group 1"]), col="blue", lwd=2,
     main="Discriminant 2 (X2): Poor Separation", xlab="X2")
lines(density(data$X2[data$Group=="Group 2"]), col="red", lwd=2)
legend("topright", legend=c("Group 1","Group 2"), col=c("blue","red"), lwd=2, cex=0.4)