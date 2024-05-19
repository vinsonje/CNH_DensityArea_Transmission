##########################################
#Host-density and area function
#Function for the host density-area relationship
#
#Parameters:
#pl - porportion of landscape of land type
#px - proportion of land for point of inflection
#p0 - proportion of Nmax when pl = 0
#p1 - proportion of Nmax when pl = 1 
#Nmax - maximum host density
##########################################

densityarea.fun = function(pl, px, p0, p1, Nmax){
  
  den.out = NA
  
  for(i in 1:length(pl)){
    if(pl[i] < px){den.out.temp = (Nmax - Nmax*p0)/px*pl[i] + p0*Nmax}
    if(pl[i] >= px){den.out.temp = (Nmax - Nmax*p1)/(px-1)*pl[i] + Nmax*p1-(Nmax-Nmax*p1)/(px-1)}
    
    den.out[i] = den.out.temp  
  }
  return(den.out)
}
