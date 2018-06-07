HestonModelAV <- function(mSteps, NSteps, nSim, GF, LF, LC, V0, InterestRate, Rho, k, theta, sigma){
    
    CompTime = proc.time() # Starting timer.
    r = log(1 + InterestRate)
    dt = 1/mSteps
    S0 = 100
    V0 = V0
    Payoff = 0
    Payoff2 = 0
    SimulatedPath<- SimulatedPath1 <- matrix(nrow=nSim, ncol= mSteps*NSteps)
    SimulatedPath2 <- SimulatedPath22 <- matrix(nrow=nSim, ncol= mSteps*NSteps)
    SimulatedPath[,1] <- SimulatedPath1[,1] <- S0
    SimulatedPath2[,1] <- SimulatedPath22[,1] <- V0
    
    for(j in 1:nSim){                                         # For all simulated paths.
        for(i in 2:(mSteps*NSteps)){                          # For all time steps.
            z_t <- rnorm(1,0,1)
            z_v <- rnorm(1,0,1)
            z_t2 <- -z_t
            z_v2 <- -z_v
            z_s <- (Rho) * z_v + sqrt(1-(Rho)^2) * z_t
            z_s2 <- (Rho) * z_v2 + sqrt(1-(Rho)^2) * z_t2
            SimulatedPath2[j, i] <- (sqrt(max(SimulatedPath2[j,i-1],0))+1/2*sigma*sqrt(dt)
                                     *z_v)^2+k*(theta-max(SimulatedPath2[j,i-1],0))*dt-1/4*sigma^2*dt
            SimulatedPath22[j, i] <- (sqrt(max(SimulatedPath22[j,i-1],0))+1/2*sigma*sqrt(dt)
                                      *z_v2)^2+k*(theta-max(SimulatedPath22[j,i-1],0))*dt-1/4*sigma^2*dt
            SimulatedPath[j, i] <- SimulatedPath[j,i-1]*(1+r*dt+sqrt(max(SimulatedPath2[j,i-1],0)*dt)
                                                         *z_s+1/2*max(SimulatedPath2[j,i-1],0)*dt*((z_s)^2-1))
            SimulatedPath1[j, i] <- SimulatedPath1[j,i-1]*(1+r*dt+sqrt(max(SimulatedPath22[j,i-1],0)*dt)
                                                           *z_s2+1/2*max(SimulatedPath22[j,i-1],0)*dt*((z_s2)^2-1))
        }
    }
    
    for(j in 1:nSim){                                         # For alle simulated paths.
        Q = 0                                                 # Initial payoff is set to 0.
        Q1= 0
        for(i in 1:NSteps){                                   # For all observation dates.
            ifelse(i==1, S1 <- S0, S1 <- SimulatedPath[j,(i-1)*mSteps])   # The value of the underlying. 
            S2 <- SimulatedPath[j,i*mSteps]
            Q <- Q + min(LC,max(LF, (S2/S1)-1))                           # The payoff is calculated.
            ifelse(i==1, S3 <- S0, S3 <- SimulatedPath1[j,(i-1)*mSteps])  # The value of the underlying (AV).
            S4 <- SimulatedPath1[j,i*mSteps]
            Q1 <- Q1 + min(LC,max(LF, (S4/S3)-1))                         # The AV payoff is calculated.
        }
        Payoff = Payoff + exp(-r*NSteps)*max(Q,GF)                        # The contract payoff is updated.
        Payoff2 = Payoff2 + exp(-r*NSteps)*max(Q1,GF)                     # The contract AV payoff is updated. 
    }
    
    MCPrice = (Payoff/nSim)      # Monte Carlo estimate.
    MCPrice2 = (Payoff2/nSim)    # Monte Carlo estimate (Atithetic path).
    AV = (MCPrice + MCPrice2)/2  # The final price is found as the average of the two MC estimates. 
    
    CompTimeEnd = paste(round(as.numeric(proc.time() - CompTime)[3], 0), "seconds")
    print(paste("The elapsed computation time was:", CompTimeEnd))                          
    print(paste("The contract value is:", round(AV, 5)))
}