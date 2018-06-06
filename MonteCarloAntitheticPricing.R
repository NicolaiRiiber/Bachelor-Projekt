MonteCarloPriceAV <- function(mSteps, NSteps, nSim, GF, LF, LC, Volatility, InterestRate){
    
    CompTime = proc.time() # Starting timer.
    r = log(1 + InterestRate)
    dt = 1/mSteps
    S0 = 100
    Payoff = 0
    Payoff2 = 0
    SimulatedPath<-matrix(nrow=nSim, ncol= mSteps*N_Obs)
    SimulatedPath2<-matrix(nrow=nSim, ncol= mSteps*N_Obs)
    SimulatedPath[,1] <- SimulatedPath2[,1] <- S0 

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
        for(i in 1:NSteps){                                   # For all observation dates (i.e. the dates at which payoff is to be calculated).
            ifelse(i==1, S1 <- S0, S1 <- SimulatedPath[j,(i-1)*mSteps])   # The value of the underlying stock on the observation dates is stored.
            S2 <- SimulatedPath[j,i*mSteps]
            Q <- Q + min(LC,max(LF, (S2/S1)-1))                           # The payoff from the simulated path is calculated.
            ifelse(i==1, S3 <- S0, S3 <- SimulatedPath2[j,(i-1)*mSteps])  # The value of the underlying stock (Antithetic path) on the obs dates is stored.
            S4 <- SimulatedPath2[j,i*mSteps]
            Q1 <- Q1 + min(LC,max(LF, (S4/S3)-1))                         # The payoff from the simulated antithetic path is calculated.
        }
        Payoff = Payoff + exp(-r*NSteps)*max(Q,GF)                        # The total contract payoff is updated.
        Payoff2 = Payoff2 + exp(-r*NSteps)*max(Q1,GF)                     # The total contract payoff is updated. (Antithetic path)
    }
    
    
    
    MCPrice = (Payoff/nSim)      # Monte Carlo estimate.
    MCPrice2 = (Payoff2/nSim)    # Monte Carlo estimate (Atithetic path).
    AV = (MCPrice + MCPrice2)/2  # The final price is found as the average of the two MC estimates. 
    
    if (as.numeric(proc.time() - CompTime)[3] >= 60){
        CompTimeEnd = round(seconds_to_period(as.numeric(proc.time() - CompTime)[3]), 0)       # Calculates the elapsed computation time if more than (or equal to) 60 seconds.
        print(paste("The elapsed computation time was:", CompTimeEnd))}                        # Prints the elapsed computation time.
    else{ CompTimeEnd = paste(round(as.numeric(proc.time() - CompTime)[3], 0), "seconds")      # Calculates the elapsed computation time if less than 60 seconds..
    print(paste("The elapsed computation time was:", CompTimeEnd))}                          
    
    print(paste("The contract value is:", round(AV, 5)))
    print(paste("The relative error [%] compared to Wilmott is:", round(((0.1731 - AV) / 0.1731) * 100,5)))
}

MonteCarloPriceAV(200, 5, 2000, 0.16, 0, 0.08, 0.25, 0.03)

