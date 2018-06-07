CliquetBinomial <- function(mSteps, QSteps, NSteps, GlobalFloor, LocalCap, LocalFloor, Volatility, IntRate){
    
    CompTime = proc.time() # Starting timer.
    CliquetPrice = vector(length = length(Volatility)) 
    # Empty vector that will be filled with option prices calculated for each volatility.
    r = log(1 + IntRate)
    dt = 1/mSteps # Discretizing the time steps.
    
    for (k in 1:length(Volatility)){            # Calculating price for each volatility.
        u = exp(Volatility[k]*sqrt(dt))         # CRR limit parameters.
        d = (1/u)
        p = (exp(r*dt)-d)/(u-d)                 # Risk-neutral probability.
        
        Q = seq(LocalFloor, 0.5, length = QSteps)
        
        xi = vector(length = mSteps)
        ResetVal = vector(length = QSteps)      # Values on the reset dates.
        Values = matrix(data = 0, nrow = mSteps, ncol = mSteps) 
        # Empty matrix that will be filled with the values in all states.
        
        for (i in 1:mSteps){
            xi[i] = (u**(i-1))*(d**(mSteps-i))}                       
        
        for (i in 1:QSteps){
            ResetVal[i] = max(GlobalFloor, Q[i])}
        
        for (n in NSteps:1){                    # For all reset periods.
            for (j in 1:QSteps){                # For all possible payoffs.
                for (t in mSteps:1){            # For all sub-periods.
                    for (i in 1:t){             # For all knots.
                        if (t == mSteps){
                            Q_up <- xi[i]*u
                            Q_down <- xi[i]*d
                            # Linear Interpolation since initial Q is discrete.
                            Values[i,t]=exp(-r*dt)*
                                (p*approxfun(Q,ResetVal,method="linear",rule=2)(Q[j]
                                                                                +min(LocalCap,max(LocalFloor,Q_up-1)))+
                                     (1-p)*approxfun(Q,ResetVal,method="linear",rule=2)
                                 (Q[j]+min(LocalCap,max(LocalFloor,Q_down-1))))}
                        else{
                            Values[i,t] = exp(-r*dt)*(p*Values[i+1, t+1]+(1-p)*
                                                          Values[i,t+1])}}}
                ResetVal[j] <- Values[1,1]}}   
        # Cliquet Option price is calculated at Q = 0. 
        CliquetPrice[k] <- approxfun(Q, ResetVal, method = "linear", rule = 2)(0)}
    
    CompTimeEnd = paste(round(as.numeric(proc.time() - CompTime)[3], 0), "seconds")            
    print(paste("The elapsed computation time was:", CompTimeEnd))
    print(paste("The contract value is:", round(CliquetPrice, 5)))}