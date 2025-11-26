# Suppose each column is a maturity, each row is a time point
maturity <- colnames(rates_num)     # maturities (e.g., FYGM3, FYGM6, etc.)
year <- 1:nrow(rates_num)           # year index
z <- as.matrix(rates_num)           # convert rates to numeric matrix

# 3D surface plot
persp(x = year, 
      y = 1:ncol(rates_num), 
      z = z, 
      theta = 30, phi = 20,
      xlab = "Year", ylab = "Maturity", zlab = "Rates",
      col = "lightblue", ticktype = "detailed")
