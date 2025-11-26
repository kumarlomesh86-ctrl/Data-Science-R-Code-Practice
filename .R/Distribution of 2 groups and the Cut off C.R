# Parameters for the two groups (mean and sd)
mu1 <- 5
sd1 <- 1
mu2 <- 8
sd2 <- 1.2
C <- 6.5  # cutoff

# Sequence of x values
x <- seq(0, 12, length=500)

# Densities
y1 <- dnorm(x, mean=mu1, sd=sd1)
y2 <- dnorm(x, mean=mu2, sd=sd2)

# Plot the two distributions
plot(x, y1, type="l", lwd=2, col="blue", ylim=c(0, max(y1,y2)),
     ylab="Density", xlab="Discriminant (xk)", main="Discriminant Function Illustration",
     cex.lab=0.8)
lines(x, y2, lwd=2, col="red")

# Draw cutoff
abline(v=C, lty=2)

# Shade misclassified areas
# Group 1 misclassified as Group 2
x_shade1 <- x[x>C]
y_shade1 <- y1[x>C]
polygon(c(x_shade1, rev(x_shade1)), c(y_shade1, rep(0,length(y_shade1))), col=rgb(0,0,1,0.3))

# Group 2 misclassified as Group 1
x_shade2 <- x[x<C]
y_shade2 <- y2[x<C]
polygon(c(x_shade2, rev(x_shade2)), c(y_shade2, rep(0,length(y_shade2))), col=rgb(1,0,0,0.3))

# Add legend
legend("topright", legend=c("Group 1","Group 2","Misclassified"),
       col=c("blue","red","purple"), lwd=2, fill=c(NA,NA, rgb(0.5,0,0.5,0.3)))
