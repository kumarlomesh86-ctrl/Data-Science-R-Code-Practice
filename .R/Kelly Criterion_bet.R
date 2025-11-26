#Simulation of the Kelly Criterion
#Basic data
pstar = 0.25 #private probability of winning
odds = 4 #actual odds
p= 1/(1+odds) #house probability of winning
edge = pstar*odds-(1-pstar)
f = edge/odds
print(c("p=",p,"pstar=",pstar,"edge=",edge,"f",f))
n = 1000
x = runif(n)
f_over = 1.5*f
f_under = 0.5*f
bankroll = rep(0,n); bankroll[1]=1
br_overbet = bankroll; br_overbet[1]=1
br_underbet = bankroll; br_underbet[1]=1
for (i in 2:n) {
  if (x[i]<=pstar) {
    bankroll[i] = bankroll[i-1] + bankroll[i-1]*f*odds
    br_overbet[i] = br_overbet[i-1] + br_overbet[i-1]*f_over*odds
    br_underbet[i] = br_underbet[i-1] + br_underbet[i-1]*f_under*odds
  }
  else {
    bankroll[i] = bankroll[i-1]-bankroll[i-1]*f
    br_overbet[i] = br_overbet[i-1]-br_overbet[i-1]*f_over
    br_underbet[i] = br_underbet[i-1]-br_underbet[i-1]*f_under
  }
}

par(mfrow=c(3,1))
plot(bankroll,type="l")
plot(br_overbet,type="l")
plot(br_underbet,type="l")
print(c(bankroll[n],br_overbet[n],br_underbet[n]))
print(c(bankroll[n]/br_overbet[n],bankroll[n]/br_underbet[n]))
source("kelly.R")
