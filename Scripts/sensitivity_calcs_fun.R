##########################################
#Sensitivity Calculations
##########################################

sensitivity.calcs = function(data){
  sen.calcs = NULL
  
  for(i in 1:max(unique(data$index)))
  {
    data.sub = subset(data, index == i)
    
    hl.time = halflife.land(data.sub[1,13:14])
    
    sub.max = max(data.sub$R0.land)
    sub.max.index = which(data.sub$R0.land == sub.max)
    sub.max.time = data.sub$time[sub.max.index]
    
    sub.min = min(data.sub$R0.land)
    sub.min.index = which(data.sub$R0.land == sub.min)
    sub.min.time = data.sub$time[sub.min.index]
    
    sen.calcs.out = data.frame(data.sub[1,5:17], 
                               max.time = sub.max.time, max = sub.max,
                               min.time = sub.min.time, min = sub.min,
                               hl.time = hl.time)
    
    sen.calcs = rbind(sen.calcs, sen.calcs.out)
  }
  
  return(sen.calcs)
}