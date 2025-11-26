rd = read.table("tryrates.txt",header=TRUE)
r1=as.matrix((rd[2]))
plot(r1,type="l")
dr1 = resid(lm(r1 ~ seq(along = r1)))
plot(dr1,type="l")
y=fft(dr1)
plot(abs(y),type="l")
dr1 = resid(lm(r1 ~ seq(along = r1)))
#Application to Binomial Option Pricing
ifft = function(x) { fft(x,inverse=TRUE)/length(x) }
ct = c(599.64,102,0,0)
q = c(0.43523,0.56477,0,0)
R = 1.0033
ifft(fft(ct)*( (4*ifft(q)/R)^3) )
