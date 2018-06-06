HestonModel <- function(mSteps, NSteps, nSim, GF, LF, LC, V0, InterestRate, Rho, k, theta, sigma){
    
    CompTime = proc.time() # Starting timer.
    r = log(1 + InterestRate)
    dt = 1/mSteps
    S0 = 100
    V0 = V0
    Payoff = 0
    Payoff2 = 0
    SimulatedPath<-matrix(nrow=nSim, ncol= mSteps*NSteps)
    SimulatedPath2<-matrix(nrow=nSim, ncol= mSteps*NSteps)
    SimulatedPath[,1] <- S0
    SimulatedPath2[,1] <- V0
    
    for(j in 1:nSim){                                         # For all simulated paths.
        for(i in 2:(mSteps*NSteps)){                          # For all time steps.
            z_t <- rnorm(1,0,1)
            z_v <- rnorm(1,0,1)
            z_s <- Rho * z_v + sqrt((1-Rho^2)) * z_t
            #SimulatedPath2[j,i] <- SimulatedPath2[j,i-1]+k*dt*(theta - max(SimulatedPath2[j,i-1],0))+sigma*sqrt(max(SimulatedPath2[j,i-1],0)*dt)*z_v
            #SimulatedPath[j,i] <- SimulatedPath[j,i-1]*(1+r*dt+sqrt(max(SimulatedPath2[j,i-1])*dt)*z_s)
            #SimulatedPath2[j, i] <- max(SimulatedPath2[j,i-1],0) + k*dt*(theta - max(SimulatedPath2[j,i-1],0))+sigma*sqrt(max(SimulatedPath2[j,i-1],0)*dt)*z_v + 1/4*(sigma^2)*dt*((z_v)^2-1)
            SimulatedPath2[j, i] <- (sqrt(max(SimulatedPath2[j, i-1],0))+1/2*sigma*sqrt(dt)*z_v)^2+k*(theta-max(SimulatedPath2[j,i-1],0))*dt-1/4*sigma*dt
            SimulatedPath[j, i] <- SimulatedPath[j,i-1]*(1+r*dt+sqrt(max(SimulatedPath2[j,i-1],0)*dt)*z_s+1/2*max(SimulatedPath2[j,i-1],0)*dt*((z_s)^2-1))
            #SimulatedPath[j, i] <- SimulatedPath[j,i-1]*exp((r-1/2*max(SimulatedPath2[j,i-1],0))*dt+sqrt(max(SimulatedPath2[j,i-1],0)*dt)*z_s)
        }
    }
    
    for(j in 1:nSim){                                         # For alle simulated paths.
        Q = 0                                                 # Initial payoff is set to 0.
        for(i in 1:NSteps){                                   # For all observation dates (i.e. the dates at which payoff is to be calculated).
            ifelse(i==1, S1 <- S0, S1 <- SimulatedPath[j,(i-1)*mSteps])   # The value of the underlying stock on the observation dates is stored.
            S2 <- SimulatedPath[j,i*mSteps]
            Q <- Q + min(LC,max(LF, (S2/S1)-1))                           # The payoff from the simulated path is calculated.
        }
        Payoff = Payoff + exp(-r*NSteps)*max(Q,GF)                        # The total contract payoff is updated.
    }
    
    MCPrice = (Payoff/nSim)      # Monte Carlo estimate.
    print(MCPrice)

    if (as.numeric(proc.time() - CompTime)[3] >= 60){
        CompTimeEnd = round(seconds_to_period(as.numeric(proc.time() - CompTime)[3]), 0)       # Calculates the elapsed computation time if more than (or equal to) 60 seconds.
        print(paste("The elapsed computation time was:", CompTimeEnd))}                        # Prints the elapsed computation time.
    else{ CompTimeEnd = paste(round(as.numeric(proc.time() - CompTime)[3], 0), "seconds")      # Calculates the elapsed computation time if less than 60 seconds..
    print(paste("The elapsed computation time was:", CompTimeEnd))}                          
    
    #print(paste("The contract value is:", round(MCPrice, 5)))
    #print(paste("The relative error [%] compared to Wilmott is:", round(((0.1731 - AV) / 0.1731) * 100,5)))
}

HestonModel(100, 5, 100, 0.16, 0, 0.08, 0.20, 0.03, -0.30, 2.00, 0.20, 0.10)

