# Create a sample data frame
cahomedata <- data.frame(
  MedHouseVal = c(2.34, 3.12, 1.89, 4.56, 3.78, 2.95),
  MedInc      = c(3.55, 4.65, 2.85, 6.25, 5.40, 3.90),
  HouseAge    = c(15, 30, 25, 10, 18, 40),
  AveRooms    = c(6.98, 5.21, 4.32, 7.85, 6.50, 5.10),
  AveBedrms   = c(1.02, 0.95, 1.10, 1.20, 1.05, 0.98),
  Population  = c(1120, 980, 1430, 2100, 1700, 1350),
  AveOccup    = c(2.45, 2.87, 3.10, 2.75, 2.95, 3.20),
  Latitude    = c(34.21, 36.77, 37.78, 33.95, 38.58, 34.05),
  Longitude   = c(-118.45, -119.42, -122.41, -118.25, -121.49, -118.24)
)

# Write to cahomedata.txt
write.table(cahomedata, file="cahomedata.txt", row.names=FALSE)

library(tree)
cahomes=read.table("cahomedata.txt", header = TRUE)
fit=tree(log(MedHouseVal)~Longitude+Latitude,data=cahomes)
plot(fit)
text(fit ,cex=0.8)

price.deciles = quantile(cahomes$MedHouseVal,0:10/10)
cut.prices = cut(cahomes$MedHouseVal,price.deciles, include.lowest=TRUE)
plot(cahomes$Longitude, cahomes$Latitude,
       col=grey(10:2/11)[cut.prices],pch=20,xlab="Longitude",ylab="Latitude")
partition.tree(fit,ordvars=c("Longitude","Latitude"),add=TRUE)