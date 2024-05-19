######################################################################
#Patch-level R0
#Function to calculate the R0 of a single patch
#
#Parameters:
#R0.parms - R0 parameters (vector: gH, mH, beta)
#land.prop - proportion of landscape of land type (vector) 
#densityarea.parms - densityarea.fun parms (vector: px, p0, p1, Nmax)
######################################################################

R0.patch = function(R0.parms, land.prop, densityarea.parms){
  
  with(as.list(c(R0.parms, densityarea.parms)), {
    
    R0.out = NULL
    
    for(i in 1:length(land.prop)){
      
      #get the abundance densityarea.fun
      Khost = densityarea.fun(land.prop[i], px, p0, p1, Nmax)
      
      #Calc R0
      R0 = (beta * Khost)/(gH + mH)
      
      R0.out = rbind(R0.out, R0)
      
    }
    
    return(data.frame(R0.out))
  })
  
}
