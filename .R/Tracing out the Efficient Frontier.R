#TRACING OUT THE EFFICIENT FRONTIER
Er_vec = matrix(seq(0.01 ,0.15 ,0.01) ,15 ,1)

Sig_vec =matrix(0,15,1)
j = 0
for (Er in Er_vec) {
  j = j+1
  wts =markowitz(mu,cv,Er)
  Sig_vec[j]=drop(sqrt(t(wts)%*%cv%*%wts))
}


plot(Sig_vec, Er_vec, 
     type = "l",           # line plot
     lwd = 2,              # thicker line
     col = "blue",         # line color
     xlab = "Risk (σ)", 
     ylab = "Expected Return (Er)",
     main = "Efficient Frontier",
     cex.lab = 1.2,        # bigger axis labels
     cex.main = 1.3)       # bigger title


