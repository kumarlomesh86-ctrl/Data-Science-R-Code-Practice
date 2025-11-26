#CRITICALITY
crit = Ri * cent
print("Criticality Vector")
print(crit)
sorted_crit = sort(crit ,decreasing=TRUE,index.return=TRUE)
Scrit = sorted_crit$x
idxScrit = sorted_crit$ix
barplot(t(Scrit) ,col="orange" ,xlab="NodeNumber" ,
        names.arg=idxScrit ,cex.names=0.75)