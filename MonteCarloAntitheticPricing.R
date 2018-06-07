MonteCarloPriceAV <- function(mSteps, NSteps, nSim, GlobalFloor, LocalFloor, LocalCap, Volatility, IntRate){
    
    CompTime = proc.time() # Starting timer.
    r = log(1 + IntRate)
    dt = 1/mSteps
    Payoff = 0
    Payoff2 = 0
    SimulatedPath<-matrix(nrow=nSim, ncol= mSteps*NSteps)
    SimulatedPath2<-matrix(nrow=nSim, ncol= mSteps*NSteps)
    SimulatedPath[,1] <- SimulatedPath2[,1] <- 100
    
    for(j in 1:nSim){                                         # For all simulated paths.
        for(i in 2:(mSteps*NSteps)){                          # For all time steps.
            z1 <- rnorm(1,0,1)
            z2 <- -z1
            SimulatedPath[j,i] <- SimulatedPath[j,i-1]*exp((r-(Volatility^2)/2)*dt+Volatility*sqrt(dt)*z1)
            SimulatedPath2[j,i] <- SimulatedPath2[j,i-1]*exp((r-(Volatility^2)/2)*dt+Volatility*sqrt(dt)*z2) 
        }
    }
    
    for(j in 1:nSim){                                         # For alle simulated paths.
        Q = 0                                                 # Initial payoff is set to 0.
        Q1= 0
        for(i in 1:NSteps){                                   # For all reset dates.
            ifelse(i==1, S1 <- 100, S1 <- SimulatedPath[j,(i-1)*mSteps])   
            S2 <- SimulatedPath[j,i*mSteps]
            Q <- Q + min(LocalCap,max(LocalFloor, (S2/S1)-1))              
            ifelse(i==1, S3 <- 100, S3 <- SimulatedPath2[j,(i-1)*mSteps]) 
            S4 <- SimulatedPath2[j,i*mSteps]
            Q1 <- Q1 + min(LocalCap,max(LocalFloor, (S4/S3)-1))            
        }
        Payoff = Payoff + exp(-r*NSteps)*max(Q,GlobalFloor)         # The total contract payoff is updated.
        Payoff2 = Payoff2 + exp(-r*NSteps)*max(Q1,GlobalFloor)      # The total contract AV payoff is updated.
    }
    
    MCPrice = (Payoff/nSim)      # Monte Carlo estimate.
    MCPrice2 = (Payoff2/nSim)    # Monte Carlo estimate (Atithetic path).
    AV = (MCPrice + MCPrice2)/2  # The final price is found as the average of the two MC estimates. 
    
    CompTimeEnd = paste(round(as.numeric(proc.time() - CompTime)[3], 0), "seconds")      
    print(paste("The elapsed computation time was:", CompTimeEnd))                       
    print(paste("The contract value is:", round(AV, 5)))
}