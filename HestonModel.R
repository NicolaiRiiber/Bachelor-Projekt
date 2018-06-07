HestonModel <- function(mSteps, NSteps, nSim, GF, LF, LC, V0, InterestRate, Rho, k, theta, sigma){
    
    CompTime = proc.time() # Starting timer.
    r = log(1 + InterestRate)
    dt = 1/mSteps
    Payoff = 0
    Payoff2 = 0
    SimulatedPath<-matrix(nrow=nSim, ncol= mSteps*NSteps)
    SimulatedPath2<-matrix(nrow=nSim, ncol= mSteps*NSteps)
    SimulatedPath[,1] <- 100
    SimulatedPath2[,1] <- V0
    
    for(j in 1:nSim){                                         # For all simulated paths.
        for(i in 2:(mSteps*NSteps)){                          # For all time steps.
            z_t <- rnorm(1,0,1)
            z_v <- rnorm(1,0,1)
            z_s <- Rho * z_v + sqrt((1-Rho^2)) * z_t
            SimulatedPath2[j, i] <- (sqrt(max(SimulatedPath2[j,i-1],0))+1/2*sigma*sqrt(dt)*z_v)^2
            +k*(theta-max(SimulatedPath2[j,i-1],0))*dt-1/4*sigma*dt
            SimulatedPath[j, i] <- SimulatedPath[j,i-1]*(1+r*dt+sqrt(max(SimulatedPath2[j,i-1],0)*dt)
                                                         *z_s+1/2*max(SimulatedPath2[j,i-1],0)*dt*((z_s)^2-1))}}
    
    for(j in 1:nSim){                                         # For alle simulated paths.
        Q = 0                                                 # Initial payoff is set to 0.
        for(i in 1:NSteps){                                   # For all reset dates.
            ifelse(i==1, S1 <- 100, S1 <- SimulatedPath[j,(i-1)*mSteps])  # The value of the underlying stock.
            S2 <- SimulatedPath[j,i*mSteps]
            Q <- Q + min(LC,max(LF, (S2/S1)-1))                           # The payoff is calculated.
        }
        Payoff = Payoff + exp(-r*NSteps)*max(Q,GF)                        # The total contract payoff is updated.
    }
    
    MCPrice = (Payoff/nSim)      # Monte Carlo estimate.
    print(MCPrice)
    
    CompTimeEnd = paste(round(as.numeric(proc.time() - CompTime)[3], 0), "seconds")
    print(paste("The elapsed computation time was:", CompTimeEnd))                         
}