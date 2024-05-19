simulate.onerun = function(parms){
  
  with(as.list(c(parms)), {
    
    X0 = 1.0
    Y0 = 0
    y0.vec = c(X=X0, Y = Y0)
    
    t.vec = seq(0, 100, 1/12)
    
    parms.ex= c(aX=aX, r=r, t.crit = t.crit, p = p)
    
    model.dyn = data.frame(ode(y0.vec, t.vec, LU.ODE, parms.ex))
    
    densityarea.parms.X = c(Nmax = Nmax.X, px = px.X, p0 = p0.X, p1 = p1.X)
    
    densityarea.parms.Y = c(Nmax = Nmax.Y, px = px.Y, p0 = p0.Y, p1 = p1.Y)
    
    
    R0patch.parms.X = c(gH = gH.X, mH = mH.X, beta = beta.X)
    
    R0patch.parms.Y = c(gH = gH.Y, mH = mH.Y, beta = beta.Y)
    
    R0.X = R0.patch(R0patch.parms.X, model.dyn$X, densityarea.parms.X)
    R0.Y = R0.patch(R0patch.parms.Y, model.dyn$Y, densityarea.parms.Y)
    
    R0.land = R0.land(R0.X, R0.Y, model.dyn)
    
    R0.temp = data.frame(model.dyn, R0.land, 
                         Nmax.X = Nmax.X, px.X = px.X, p0.X = p0.X, p1.X = p1.X,
                         Nmax.Y = Nmax.Y, px.Y = px.Y, p0.Y = p0.Y, p1.Y = p1.Y, 
                         aX = aX, r = r, t.crit = t.crit, p = p
    )
    
    return(R0.temp)
    
  }) #end (with) parameter section

} #end function