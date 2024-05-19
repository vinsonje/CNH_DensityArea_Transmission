simulate.densityarea.trans = function(parms.df){
  
  R0.df = NULL
  
  for(i in 1:dim(parms.df)[1]){
    print(i)
    parms.now = parms.df[i,]
    
    R0.df.or = simulate.onerun(parms.now)
    R0.df.temp = data.frame(R0.df.or, index = i)
    
    R0.df = rbind(R0.df, R0.df.temp)
  } #end for loop
  
  return(R0.df)
  
} #end function