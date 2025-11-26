#CROSS IMPACT MATRIX
#CHECK FOR SPILLOVER EFFECTS FROM ONE NODE TO ALL OTHERS
d_RiskDecomp = NULL
n = length(Ri)
for ( j in 1:n) {
  Ri2 = Ri
  Ri2 [ j ] = Ri[ j]+1
  res = NetRisk(Ri2 ,X)
  d_Risk = as.matrix(res [[3]]) 
  RiskDecomp
  d_RiskDecomp = cbind(d_RiskDecomp,d_Risk)
}
#3D plots
library("RColorBrewer");
library("lattice");
library("latticeExtra")

cloud(d_RiskDecomp,
      panel.3d.cloud = panel.3dbars,
      xbase = 0.25, ybase = 0.25,
      zlim= c(min(d_RiskDecomp) , max(d_RiskDecomp)) ,
      scales = list(arrows = FALSE, just = "right") ,
      xlab = "On" , ylab = "From" , zlab =NULL,
      main="Change in Risk Contribution" ,
      col.facet = level.colors(d_RiskDecomp,
                                 at =do.breaks(range(d_RiskDecomp), 20),
                                 col.regions = cm.colors, colors =TRUE),
      colorkey = list(col = cm.colors ,
                      at =do.breaks(range(d_RiskDecomp) , 20)) ,
      #screen = list (z = 40, x = 30)
)
brewer.div <- colorRampPalette(brewer.pal(11, "Spectral") ,
                              interpolate = "spline")
levelplot(d_RiskDecomp, aspect = "iso" ,
          col.regions = brewer.div(20) ,
          ylab="Impact from" , xlab="Impact on" ,
          main="Change in Risk Contribution")