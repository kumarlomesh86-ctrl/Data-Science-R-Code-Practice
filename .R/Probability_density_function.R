#Model for fourier inversion for arithmetic brownian motion
x0 = 10
mu = 10
sig = 5
tau = 0.25
s = (0:10000)/100
ds = s[2]-s[1]
phi = exp(1i*s*x0+mu*1i*s*tau-0.5*s^2*sig^2*tau)
x = (0:250)/10
fx=NULL
for ( k in 1:length(x)){
  g = sum(as.numeric(exp(-1i*s*x[k]) * phi * ds))/pi
  print(c(x[k],g))
  fx = c(fx,g)
}
plot(x,fx,type="l")

