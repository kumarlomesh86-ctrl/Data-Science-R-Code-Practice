asbrec = function(w,p) {
  #w: payoffs
  #p: probabilities
  #BASIC SETUP
  N= length(w)
  maxloss = sum(w)
  bucket = c(0,seq(maxloss))
  LP=matrix(0,N,maxloss+1) #probability grid over losses
  #DO FIRST FIRM
  LP[1,1] = 1-p[1];
  LP[1,w[1]+1] =p[1];
  #LOOP OVER REMAINING FIRMS
  for (i in seq(2,N)) {
    for (j in seq(maxloss+1)) {
      LP[i,j] =LP[i-1,j]*(1-p[i])
      if (bucket[j]-w[i] >= 0) {
        LP[i,j] =LP[i,j]+LP[i-1,j-w[i]]*p[i]
      }
    }
  }
  
#FINISH UP
lossprobs =LP[N,]
print(t(LP))
result =matrix(c(bucket,lossprobs),(maxloss+1),2)
}
w= c(5,8,4,2,1)
p= array(1/length(w),length(w))
res = asbrec(w,p)
print(res)
print(sum(res[,2]))
barplot(res[,2],names.arg=res[ ,1],
        xlab="portfolio value",ylab="probability")