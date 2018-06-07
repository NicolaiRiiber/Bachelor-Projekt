MonteCarloPrice <- function(mSteps, NSteps, nSim, GlobalFloor, LocalFloor, LocalCap, Volatility, IntRate){
    
    CompTime = proc.time() # Starting timer.
    r = log(1 + IntRate)
    dt = 1/mSteps
    Payoff = 0
    SimulatedPath<-matrix(nrow=nSim, ncol= mSteps*NSteps)
    SimulatedPath[,1] <- 100
    
    for(j in 1:nSim){                                         # For all simulated paths.
        for(i in 2:(mSteps*NSteps)){                          # For all time steps.
            z1 <- rnorm(1,0,1)
            SimulatedPath[j,i] <- SimulatedPath[j,i-1]*exp((r-(Volatility^2)/2)*dt+Volatility*sqrt(dt)*z1)
        }
    }
    
    for(j in 1:nSim){                                         # For alle simulated paths.
        Q = 0                                                 # Initial payoff is set to 0.
        for(i in 1:NSteps){                                   
            ifelse(i==1, S1 <- 100, S1 <- SimulatedPath[j,(i-1)*mSteps])   
            S2 <- SimulatedPath[j,i*mSteps]
            Q <- Q + min(LocalCap,max(LocalFloor, (S2/S1)-1)) # Local payoffs are stored.                           
        }
        Payoff = Payoff + exp(-r*NSteps)*max(Q,GlobalFloor)   # The total contract payoff is updated.
    }
    
    MCPrice = (Payoff/nSim) # Monte Carlo estimate.
    
    CompTimeEnd = paste(round(as.numeric(proc.time() - CompTime)[3], 0), "seconds")
    print(paste("The elapsed computation time was:", CompTimeEnd))                          
    print(paste("The contract value is:", round(MCPrice, 5)))
}