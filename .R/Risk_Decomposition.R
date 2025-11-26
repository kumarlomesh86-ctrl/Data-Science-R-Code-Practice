#Compute Risk Decomposition
RiskDecomp=RiskIncr*Ri
sorted_RiskDecomp=sort(RiskDecomp,decreasing = TRUE, index.return=TRUE)
RD=sorted_RiskDecomp$x
idxRD=sorted_RiskDecomp$ix
print("Risk_Contribution");
print(RiskDecomp)
print(sum(RiskDecomp))
barplot(t(RD),col = "dark green", xlab="Node_Number", names.arg = idxRD, cex.names = 0.75)
print(RD)

#Compute normalized score SBar
Sbar=S/sqrt(t(Ri)%*%Ri)
print("Sbar_(normalized_risk_score");
print(Sbar)
