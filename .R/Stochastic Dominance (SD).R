x = seq(-4,4,0.1)
F_B = pnorm(x,mean=0,sd=1);
F_A = pnorm(x,mean=0.25,sd=1);
F_A-F_B #FSD exists

cumsum(F_A-F_B)