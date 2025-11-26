RiskIncr

#PLOT RISK INCREMENTS
sorted_RiskIncr = sort(RiskIncr,decreasing=TRUE,
                       index.return=TRUE)
RI = sorted_RiskIncr$x
idxRI = sorted_RiskIncr$ix
print("Risk Increment (per unit increase in any node risk")
print(RiskIncr)
barplot(t(RI),col="dark blue",xlab="NodeNumber",
        names.arg=idxRI,cex.names=0.75)