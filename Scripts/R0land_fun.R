######################################################################
#landscape-level R0
#Function to calculate the R0 of the landscape
#
#Parameters:
#R0.X - patch-level R0 for forested (vector)
#R0.Y - patch-level R0 for cleared (vector)
#land.prop - proportion of landscape of each land type (dataframe) 
######################################################################

R0.land = function(R0.X, R0.Y, land.prop){
  
  R0.X.avg = R0.X$R0 * land.prop$X
  
  R0.Y.avg = R0.Y$R0 * land.prop$Y
  
  R0.land = R0.X.avg + R0.Y.avg
  
  R0.land.out = data.frame(R0.land = R0.land)
  
  return(R0.land.out)
} 