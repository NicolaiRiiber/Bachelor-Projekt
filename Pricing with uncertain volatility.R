library(lubridate)

Cliquet <- function(mSteps, QSteps, NSteps, GlobalFloor, LocalCap, Volatility, IntRate){
  
    CompTime = proc.time() # Starting timer.
    CliquetPrice = vector(length = length(Volatility)) # Empty vector that will be filled with option prices calculated for each volatility.
    LocalFloor = 0
    r = log(1 + IntRate)
    dt = 1/mSteps # Discretizing the time steps.
    
    for (k in 1:length(Volatility)){            # Calculating price for each volatility.
        u = exp(Volatility[k]*sqrt(dt))         # Cox, Ross & Rubinstein binomial limit parameters.
        d = (1/u)
        p = (exp(r*dt)-d)/(u-d)                 # Risk-neutral probability.
  
        Q_upperBound = 0.5                      # Q upper bound corresponding to approx. 5 * Local Cap.
        Q_lowerBound = LocalFloor               # Q lower bound corresponding to local floor (i.e. 0).
        Q = seq(Q_lowerBound, Q_upperBound, length = QSteps) # Q is discretized with the number of QSteps.
  
        xi = vector(length = mSteps)
        ValueObs = vector(length = QSteps)
        Values = matrix(data = 0, nrow = mSteps, ncol = mSteps) # Empty matrix that will be filled with the values in all states.
        Gamma = vector(length = mSteps)
  
        for (i in 1:mSteps){
            xi[i] = (u**(i-1))*(d**(mSteps-i))}                       
  
        for (i in 1:QSteps){
            ValueObs[i] = max(GlobalFloor, Q[i])}
  
        for (n in NSteps:1){                    # For all reset periods.
            for (j in 1:QSteps){                # For all Q-steps (i.e. all possible payoffs).
                for (t in mSteps:1){            # For all m-steps (i.e. all sub-periods).
                    for (i in 1:t){             # For all knots.
                        if (t == mSteps){
                            Q_up <- xi[i]*u
                            Q_down <- xi[i]*d
                            Values[i,t]=exp(-r*dt)*(p*approxfun(Q,ValueObs,method="linear",rule=2)(Q[j]+min(LocalCap,max(LocalFloor, Q_up-1)))+(1-p)*approxfun(Q,ValueObs,method="linear",rule=2)(Q[j]+min(LocalCap,max(LocalFloor,Q_down-1))))
                        }
                        else{
                            Values[i,t] = exp(-r*dt)*(p*Values[i+1, t+1]+(1-p)*Values[i,t+1])
                        }
                    }
                }
                ValueObs[j] <- Values[1,1] 
            }
        }   
        CliquetPrice[k] <- approxfun(Q, ValueObs, method = "linear", rule = 2)(0)     # Cliquet Option price is calculated at Q = 0. 
    }
    
    for (k in 1:QSteps){
      for (i in 2:(mSteps-1)){
        Gamma[k] = (Values[i+1,k]-2*Values[i,k]+Values[i-1,k]) / dt / dt
      }
    } 
    
    print(Gamma)
    if (as.numeric(proc.time() - CompTime)[3] >= 60){
        CompTimeEnd = round(seconds_to_period(as.numeric(proc.time() - CompTime)[3]), 0)         # Calculates the elapsed computation time if more than (or equal to) 60 seconds.
        print(paste("The elapsed computation time was:", CompTimeEnd))}                          # Prints the elapsed computation time.
    else{
      CompTimeEnd = paste(round(as.numeric(proc.time() - CompTime)[3], 0), "seconds")            # Calculates the elapsed computation time if less than 60 seconds..
      print(paste("The elapsed computation time was:", CompTimeEnd))}                            # Prints the elapsed computation time.
    
    if(length(CliquetPrice) == 1){                                                    # Prints contract value when volatility is constant.
        print(paste("The contract value is:", round(CliquetPrice, 5)))}
    else{
        print(paste("The maximum contract value is:", round(max(CliquetPrice), 5)))   # Prints the maximal contract value when volatility is uncertain.
        print(paste("The minimum contract value is:", round(min(CliquetPrice), 5)))}  # Prints the minimal contract value when volatlity is uncertain.
}

Cliquet(150, 150, 5, 0.16, 0.08, 0.25, 0.03)
