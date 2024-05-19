##########################################
#Land-use ODE model
#Function for the ODE model describing land-use.
#State variables
#X - proportion of landscape forested
#Y - proportion of landscape cleared
#
#Parameters:
#aX - deforestation rate 
#r - reforestation rate
#p - binomial, p = 1 -> cleared reverts to forested
#              p = 0 -> cleared reverts to settleed
#t.crit - time that deforestation stops, refor. starts
##########################################

LU.ODE = function(t.vec, y, parms){
  
  with(as.list(c(parms, y)), {
    
    
    dX = p*(t.vec>t.crit)*r*Y - (t.vec<t.crit)*aX*X
    dY = -(t.vec>t.crit)*r*Y + (t.vec<t.crit)*aX*X
    
    list(c(dX, dY))
  })
}