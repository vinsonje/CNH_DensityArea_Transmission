##########################################
#Calculate the time for the land 
#halflife time, time it takes to reach 0.5
##########################################

halflife.land = function(parms){
  
  with(as.list(parms), {
    
  t = log(1 - 0.5*((aX+r)/aX))/-(aX+r)
  
  return(t)
  })
  
}

#########################
#Testing
#########################
# 
# aX = 0.01
# r = 0.02
# 
# parms.test = c(aX = aX, r = r)
# 
# halflife.land(parms.test)