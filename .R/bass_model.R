library(quantmod)
data_iphone=read.table("iphone_sales.txt", header=TRUE)
isales=data_iphone[ ,2]
cum_isales=cumsum(isales)
cum_isales2=cum_isales^2
res=lm(isales~cum_isales+cum_isales2)
print(res)
#coeficient fo cummulative sales
b<-coef(res)
#Fit to bass model
m1=(b[2]+sqrt(b[2]^2-4*b[1]*b[3]))/(2*b[3])
m2=(b[2]-sqrt(b[2]^2-4*b[1]*b[3]))/(2*b[3])
print(c(m1,m2))
m=max(m1,m2)
print(m)
p=b[1]/m
q=-m*b[3]
print(c(p,q))
#Plot the fitted model
nqtrs=100
t=seq(0,nqtrs)
# Bass diffusion function for adoption over time
f_t <- function(t, p, q, m) {
  ((p + q * (cumadopt(t-1, p, q, m) / m)) * (m - cumadopt(t-1, p, q, m)))
}

# Function for cumulative adoption up to time t
cumadopt <- function(t, p, q, m) {
  m * (1 - exp(-(p+q)*t)) / (1 + (q/p)*exp(-(p+q)*t))
}

# New adopters at each time
new_adopters <- sapply(t, function(x) f_t(x, p, q, m))

# Check first few
head(new_adopters)


plot(t,new_adopters,type="l")
n = length(isales)
lines(1:n,isales,col="red",lwd=2,lty=2)
#PEAK SALES TIME POINT (IN QUARTERS)
tstar = 1/(p+q)*log(p/q)
print( tstar )
length(isales)
