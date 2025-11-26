x = (1:1000) /1000
y = x*450+50
# Note that we have used the non-central Beta distribution, with shape parameters a = 2 and b = 4.
prob_y = dbeta(x,2,4)
print(c("check=",sum(prob_y)/1000))
prob_y=prob_y/sum(prob_y)
plot(y,prob_y,type="l")

print(c("mean=" ,sum(y*prob_y)))
print(c("stdev=",sqrt(sum(y^2*prob_y)-(sum(y*prob_y))^2)))

x = (1:1000)/1000
y = 50 + 450*x
cumprob_y = pbeta(x,2,4)
exp_profit = (300-y)*cumprob_y^10
idx = which(exp_profit==max(exp_profit))
y[idx]


#If there were only 5 other bidders, then the bid would be
exp_profit=(300-y)*cumprob_y^5
idx = which(exp_profit==max(exp_profit ))
y[idx]
